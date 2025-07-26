################################################################################
# FILE NAME: 1.FirstStage @Codes/MAIN
# Purpose: 1st stage
#       - Use the Conditional Poisson Model w/ dlnm CB fx for temperature 
#    
# SAVE RESULT
#     coefall, vcovall: cumulative overall coefficient and covariance (.RData)
################################################################################
# Load observed data
load("/data/observed/slist.RData")
load("/data/observed/city_list.RData")
################################################################################
# DEFINE THE MAIN PARAMETERS FOR THE ANALYSIS
# 1. Specification of Exposure function
varfun = "ns"
vardf = 2
varper = 50 
cenper = 50

# 2. Specification of Lag function
lag = 3

# 3. Compute percentiles
per = t(sapply(slist,function(x) 
  quantile(x$tmean,c(2.5,10,25,50,75,90,97.5)/100,na.rm=T)))

# 4. Define outcom column
out = "stot"

# 5. Function for computing the Q-AIC
QAIC = function(model) {
  phi = summary(model)$dispersion
  loglik = sum(dpois( model$y, model$fitted.values, log=TRUE))
  return(-2*loglik + 2*summary(model)$df[3]*phi)
}

################################################################################
# 01 Estimation of the location-specific temperature–suicide association
# - 1st stage modeling
################################################################################
# OVERALL CUMULATIVE ESTIMATION 

# 1. Create the objects to store the results
coefall = matrix(NA,nrow(city_list),length(varper)+1,
                  dimnames=list(city_list$citynm))
vcovall = pred1st = vector("list",nrow(city_list))
res = vector("list",nrow(city_list))
names(vcovall) = city_list$citynm   

qaic = disp = numeric(length=nrow(city_list))
names(qaic) = names(disp) = city_list$citynm   

slist.sub = list()

# 2. Run the loop
time = proc.time()[3]

for(i in seq(length(slist))) {
  
  # Print
  cat(i,"")
  
  # Extract the location
  data = slist[[i]]
  
  # Generate month, year, dow, and stratum
  data$month  = as.factor(months(data$date))
  data$year   = as.factor(format(data$date, format="%Y") )
  data$dow    = as.factor(weekdays(data$date))
  data$stratum = as.factor(data$year:data$month:data$dow)
  
  data = data[order(data$date),]
  
  # Check how many strata have no event 
  nevent = with(data, tapply(stot,stratum,sum))
  #table(nevent)
  #sum(table(nevent)[-1]) # how many stratum available
  
  sub = (nevent>0)[data$stratum] # 'sub' is T/F indicator.
  # table(with(data[sub==F,], tapply(stot,stratum,sum))) # double check how many stratum with zero counts
  # length(unique(data$stratum)) # how many stratum as a total
  
  # Expand lagged exposure (tmean)
  tlag = Lag(data$tmean,0:3)      
  tsub = tlag[sub, ] # Remove stratum without an event
  
  # Define the crossbasis
  argvar = list(fun=varfun, df=vardf, knots=quantile(data$tmean,varper/100,na.rm=T))
  
  cb = crossbasis(tsub,argvar=argvar,    
                   arglag=list(fun="strata", breaks=1))
  #summary(cb)
  
  # Extract the subject datab (Remove stratum without an event)
  slist.sub[[i]] = subset = data[sub,]
  subset$cb.temp = cb
  y = subset[ ,out]
  
  model = gnm(y ~ cb.temp, data=subset, family=quasipoisson, eliminate=factor(stratum))
  
  # Store Q-AIC
  qaic[i] = QAIC(model)
  disp[i] = summary(model)$dispersion
  
  # Get predictions
  pred1st[[i]] = crosspred(cb, model, cen=quantile(data$tmean,varper/100,na.rm=T), model.link="log")
  
  # Reduction to overal cumulative association
  redall = crossreduce(cb, model, cen=quantile(data$tmean,varper/100,na.rm=T))
  coefall[i,] = coef(redall)
  vcovall[[i]] = vcov(redall)
  res[[i]] = model$converged
}
proc.time()[3]-time
#dev.off()

rownames(coefall) = names(slist)
names(vcovall) = names(slist)
names(res) = names(slist)

# 3. Save cumulative overall est (coef, cov)
save(coefall, vcovall, file = "/data/output/1st_coef_vcov.RData")

################################################################################
# - PLOT (OVERALL CUMULATIVE LOCATION-SPECIFIC ASSOCIATION FOR 1ST STAGE)
#   The plots show the cumulative exposure-response association, 
#     in terms of relative risks (RR) and centered in the 50% of temperature, 
#     across the 3 days of lag.

# pdf("/results/1st_stage_overall.pdf",width=9,height=13)
layout(matrix(seq(6*4),nrow=6,byrow=T))
par(mar=c(4,3.8,3,2.4),mgp=c(2.5,1,0),las=1)
xlab = expression(paste("Temperature (",degree,"C)"))
for(i in seq(pred1st)) {
  plot(pred1st[[i]], "overall", col="red", axes=T,
       ylab="RR",xlab=xlab, main=paste("Overall -", city_list$citynm[[i]]), lwd = 2)
  abline(v=per[i, "50%"], col=c(2),lty=c(4))
}
# dev.off()


