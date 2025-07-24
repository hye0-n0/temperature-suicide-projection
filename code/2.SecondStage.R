################################################################################
# FILE NAME: 2.SecondStage.R @Codes/MAIN
# Purpose: 2nd Stage
#       - Use Multivariate mete-analysis of Reduced coef and Computation of BLUP
#
# SAVE RESULT
#     mvoverall: multivariate mete-regression model (.RData)
#     mypred: region-specific reduced coefficient (.RData)
#     blupres: residual as random effect (.RData)
#     predcoefs, predvcov: Pooled overall cumulative association prediction (.RData)
#     Figure S3, Figure 1
################################################################################
# Load 1st stage ouput data
load("/data/observed/slist.RData")
load("/data/observed/city_list.RData")
load("/data/output//1st_coef_vcov.RData")
################################################################################
# 02 Multivariate meta-analysis of reduced coef and computation of BLUPs
# - Second stage modeling
################################################################################
# 1. Create average temperature and range as meta-predictors
avgtmean = sapply(slist,function(x) mean(x$tmean,na.rm=T))
rangetmean = sapply(slist,function(x) diff(range(x$tmean,na.rm=T)))

# 2. Mixed meta regression for overall cumulative association with random effect
set.seed(13041975)
mvoverall = mixmeta(coefall~regionnm+gdp+avgtmean+rangetmean,
                    data=city_list, random = ~1|countrynm/citynm, control = list(showiter = TRUE),
                    S = vcovall)
save(mvoverall, file="/data/output/mvoverall.RData")  # 2nd-stage model

# 3. Predict region-specific average coef
regions = unique(city_list$regionnm)
datanew = data.frame(
  regionnm = regions,
  gdp = tapply(city_list$gdp, city_list$regionnm, mean)[regions],
  avgtmean = tapply(avgtmean, city_list$regionnm, mean)[regions],
  rangetmean = tapply(rangetmean, city_list$regionnm, mean)[regions],
  row.names = regions
)
mvpred = predict(mvoverall, datanew, vcov=T, format="list")
save(mvpred, file="/data/output/mypred.RData")  # prediction

# 4. Fixed effect
# Predict coefficients
metavariable = cbind(city_list, avgtmean = avgtmean, rangetmean = rangetmean)
fixpred = predict(mvoverall, metavariable, vcov = T)
fixcoef = sapply(fixpred, "[[", "fit") |> t()        # coefs and vcovs
fixvcov = sapply(fixpred, function(x) vechMat(x$vcov)) |> t()
save(metavariable, file="/data/output/metavariable.RData")   # fixed-effect

# 5. Random effect
# Residual
blupres = blup(mvoverall, type = "residual", vcov = T) 
rancoef = sapply(blupres, "[[", "blup") |> t()     # coefs and vcovs
ranvcov = sapply(blupres, function(x) vechMat(x$vcov)) |> t()
save(blupres, file="/data/output/blupres.RData")   # random-effect

# 6. Pooled overall cumulative association prediction (Pooled fixed effect and random effect)
predcoefs = fixcoef + rancoef
predvcov = fixvcov + ranvcov

# Change data structure
make_matrix = function(vec) {matrix(c(vec[1], vec[2], vec[2], vec[3]), nrow = 2, byrow = TRUE)}
city_names = rownames(predvcov)
predvcov_mat = lapply(1:nrow(predvcov), function(i) {
  vec = predvcov[i, ]
  make_matrix(vec)
})
names(predvcov_mat) = city_names

# 7. Save BLUPs
save(predcoefs, predvcov_mat, file="/data/output/New_blups.RData")


################################################################################
# RE-CENTERING (COUNTRY LEVEL)
################################################################################
# 1. Define percentiles and identify labels of interest
predper = c(seq(0, 1, 0.1), 2:98, seq(99, 100, 0.1))
indlab = predper %in% c(0, 1, 10, 50, 90, 99, 100)

# 2. Get list of countries from city list
countries = unique(city_list$countrynm)

# 3. Predict pooled overall cumulative associations by country
cp.country = list()
for (country in countries) {
  cat("Processing:", country, "\n")
  
  # Indices of cities belonging to the country
  idx = which(city_list$countrynm == country)
  if (length(idx) == 0) next
  
  # Average BLUP coefficients and variance-covariance matrix
  if (length(idx) == 1) {
    coef_mean = predcoefs[idx, ]
    vcov_mean = predvcov_mat[[idx]]
  } else {
    coef_mat = predcoefs[idx, ]
    vcov_list = predvcov_mat[idx]
    
    coef_mean = colMeans(coef_mat, na.rm = TRUE)
    vcov_mean = Reduce("+", vcov_list) / length(vcov_list)
  }
  
  # Combine all temperature values from slist
  tmean_all = unlist(lapply(slist[idx], function(x) x$tmean))
  
  # Define basis function for temperature
  knots = quantile(tmean_all, varper / 100, na.rm = TRUE)
  cen = quantile(tmean_all, cenper / 100, na.rm = TRUE)
  bvar = onebasis(tmean_all, fun = varfun, df = vardf, knots = knots)
  
  # Predict overall cumulative association
  pred = crosspred(bvar, coef = coef_mean, vcov = vcov_mean,
                   model.link = "log", at = tmean_all, cen = cen)
  
  # Save results
  cp.country[[country]] = list(pred = pred, tmean_all = tmean_all)
}

# 4. Plot the cumulative RR curves by location
# - Figure S3
par(mfrow = c(6, 5))  
countries = names(cp.country)
for (country in countries) {
  pred = cp.country[[country]]$pred
  plot(pred, "overall",
       xlab = "Temperature (°C)", ylab = "Relative Risk (RR)", main = country,
       col = "red", lwd = 2, ci.arg = list(col = rgb(1, 0, 0, 0.2)), ylim = c(0.8, 1.5))
  abline(h = 1, lty = 2, col = "gray")  # 기준선
}

################################################################################
# RE-CENTERING (REGION LEVEL)
################################################################################
# 1. Define percentiles and related average temperatures 
#   add a 'jitter' to make percentiles unique
predper = c(seq(0,1,0.1),2:98,seq(99,100,0.1))
tmeanregion = tapply(seq(nrow(city_list)),city_list$regionnm,
                     function(ind) rowMeans(sapply(slist[ind], function(x)
                       quantile(jitter(x$tmean),predper/100,na.rm=T))))

# 2. Predict the pooled overall cumulative associations by region
cp.region = list()
for(i in names(mvpred)){
  bvar = onebasis(tmeanregion[[i]],fun=varfun,df=vardf,
                  knots=tmeanregion[[i]][paste(varper,".0%",sep="")])
  
  cen  = tmeanregion[[i]]["50.0%"] # median value by region
  
  pred = crosspred(bvar,coef=mvpred[[i]]$fit,vcov=mvpred[[i]]$vcov,
                   model.link="log",at=tmeanregion[[i]],cen=cen)
  
  cp.region[[i]] = pred
}
# save(cp.region, file="cp_region.RData")

# 3. Plot the cumulative RR curves by location
# - Figure 1
par(mfrow = c(2, 5), mar = c(4, 4, 3, 1))  # 아래, 왼쪽, 위, 오른쪽 여백 조정
regions <- names(cp.region)
for (region in regions) {
  pred = cp.region[[region]]
  plot(pred, "overall", 
       xlab = "Temperature (°C)", ylab = "Relative Risk (RR)", main = region, 
       col = "red", lwd = 2, ci.arg = list(col = rgb(0, 0, 1, 0.2)), ylim = c(0.5, 2.5))
  abline(h = 1, lty = 2, col = "gray")
}

