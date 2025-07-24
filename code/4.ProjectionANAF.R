################################################################################
# FILE NAME: 4.ProjectionAFAF.R @Codes/MAIN
# Purpose: Projection of AN, AF and Quantification of Uncertainty
#    
# SAVE RESULT
#     anabs, anrel, afabs, afrel: attributable number and fraction by region (.RData)
#     anabs_city,afabs_city,anrel_city,afrel_city: attributable number and fraction by city (.RData)
################################################################################
# Load data
# load("/data/observed/slist.RData")
# load("/data/observed/city_list.RData")

# load("/data/modeled/ssp126cal_50s.RData")
# load("/data/modeled/ssp245cal_50s.RData")
# load("/data/modeled/ssp585cal_50s.RData")
# load("/data/modeled/modeled_suicide.RData")

# load("/data/output/mvoverall.RData")
# load("/data/output/metavariable.RData")
# load("/data/output/blupres.RData")
# load("/data/output/New_blups.RData")
################################################################################
# 04 EXTRAPOLATION OF THE EXPOSURE-RESPONSE CURVE
# 05 PROJECTION & QUANTIFICATION OF THE IMPACT
# 06 ENSEMBLE ESTIMATES & QUANTIFICATION OF THE UNCERTAINTY
################################################################################
# The three last steps of the analysis (extrapolation of the curve, 
#   impact projections and quantification of the uncertainty) can be performed 
#   sequentially using the following code.

# In brief, once we extrapolate the curve, we estimate the daily number of attributable 
#   deaths (AN) in each scenario, GCM and temperature range. 
# Then, we compute the sum of ANs per each 10-years period for the ensemble 
#   per each RCP and temperature range. We also estimate the difference in AN 
#   relative to the ANs estimated for the current days (2010-19). 
# By dividing between the total mortality, we estimate the corresponding 
#   attributable fractions (AFs).
# Uncertainty of the estimated impacts is expressed in terms of empirical 
#   confidence intervals, defined as the 2.5th and 97.5th percentiles of the 
#   empirical distribution of the impacts across coefficients samples and GCMs. 
#   The distribution is obtained through Monte Carlo simulation of the coefficients.
# EXCEPT SOME LOCATIONS BECAUSE OF HIGH UNCERTAINTY IN 1ST STAGE

# 1. EXTRACT COEFFICIENTS AND REFERENCE TEMPERATURE
#   50th percentile temperature for each city defining reference temperature.
#   blups, covariance matrix of blups defining the overall exposure-response association.
cen = lapply(slist, function(df) quantile(df$tmean,cenper/100,na.rm=T)) # reference
nc = ncol(predcoefs) # Number of first-stage coefficients

# 2. STORE THE SUICIDE BY PERIOD
suicideperiod = lapply(suicideproj, function(x) sum(x[1:365])*10)

# 3. CREATE TEMPORARY OBJECT TO STORE THE ESTIMATED AN 
# We create an array with 6 dimensions to store the AN estimated in each 
#   simulation for each period, range of temperature, gcm and rcp. An additional
#   dimension is also included to store the absolute AN (estimated directly from
#   the formula below), and the difference in AN relative to current period.

# - DEFINE THE NAMES AND NUMBER OF LEVELS PER DIMENSION (ALSO USED TO DEFINE 
#     THE INDEX TO RUN THE LOOPS BELOW)

# (0) LOCATIONS
location = names(slist)

# (1) DIMENSION - 10-YEAR PERIOD 
#  *LABEL THE HISTORICAL PERIOD
histperiod = "2010-19"

#  *LABELS THE PROJECTION PERIODS
projperiod = c(paste(202:205,0,"-",substr(202:205,3,3),9,sep=""))

#  *DEFINE SEQUENCE OF PERIODS FOR THE PREDICTIONS (HISTORICAL & PROJECTED)
histseqperiod = factor(rep(histperiod,length.out=365*length(seq(2010,2019))))
projseqperiod = factor(c(rep(projperiod,each=365*10)))
seqperiod = factor(c(as.numeric(histseqperiod)-1,as.numeric(projseqperiod)))
levels(seqperiod) = c(histperiod,projperiod)
length(seqperiod) ; dim(ssp126cal_50s[[1]]) ; length(suicideproj[[1]])


# (2) DIMENSION - RANGE OF TEMPERATURES
temprange = c("tot","below","above")

# (3) DIMENSION - ABSOLUTE AN/DIFFERENCE IN AN
absrel = c("abs","rel")

# (4) DIMENSION - GENERAL CIRCULATION MODELS
# *LIST OF GCMs 
gcm = c("GFDL-ESM2M"="gfdl-esm4","MPI-ESM1-HR"="mpi-esm1-2-hr",
        "IPSL-CM5A-LR"="ipsl-cm6a-lr","MRI-ESM2-0"="mri-esm2-0",
        "UKESM1-0-LL"="ukesm1-0-ll")

# (5) DIMENSION - SCENARIO DIMENSION
#  *LIST OF REPRESENTATIVE CONCENTRATION PATHWAYS SCENARIOS 
ssp = c(SSP126="ssp126cal_50s", SSP245="ssp245cal_50s", SSP585="ssp585cal_50s")


# (6) DIMENSION - NUMBER OF ITERATION IN THE MONTE-CARLO SIMULATION 
nsim = 1000

# DEFINE THE ARRAY
ansim = array(NA,dim=c(length(location), length(levels(seqperiod)), length(temprange),
                       length(absrel), length(gcm),length(ssp),nsim+1), 
              dimnames=list(location, levels(seqperiod), temprange,absrel,
                            names(gcm),names(ssp), c("est",paste0("sim",seq(nsim)))))


# RUN LOOP FOR EACH LOCATION
for(loc in location){
  
  # PRINT
  cat("\n\n", loc, "")
  
  # RUN LOOP PER SSP
  for (i in seq(ssp)) {
    
    # PRINT
    cat("\n\n ",names(ssp)[i],"\n ")
    
    # SELECTION OF THE PROJECTED TEMPERATURE SERIES FOR A SPECIFIC SSP SCENARIO
    tmeanproj = get(ssp[[i]])
    
    # RUN LOOP PER GCM
    for(j in seq(gcm)) {
      
      # PRINT
      cat(gcm[j], "")
      
      # (4) EXTRAPOLATION OF THE CURVE: 
      # - DERIVE THE CENTERED BASIS USING THE PROJECTED TEMPERATURE SERIES
      #   AND EXTRACT PARAMETERS
      bvar = do.call(onebasis, c(list(x=tmeanproj[[loc]][, j+1]),
                                 list(fun=varfun, df=vardf, knots=quantile(tmeanproj[[loc]][, j+1], varper/100, na.rm=T), 
                                      Bound=range(tmeanproj[[loc]][, j+1], na.rm=T))))
      cenvec = do.call(onebasis, c(list(x=cen[[loc]]),
                                   list(fun=varfun, df=vardf, knots=quantile(tmeanproj[[loc]][, j+1], varper/100, na.rm=T), 
                                        Bound=range(tmeanproj[[loc]][, j+1], na.rm=T))))
      bvarcen = scale(bvar,center=cenvec,scale=F)
      
      # INDICATOR FOR BELOW/ABOVE DAYS
      indheat = tmeanproj[[loc]][,j+1]>cen[[loc]]
      
      # (5) IMPACT PROJECTIONS:
      # - COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
      an = (1-exp(-bvarcen%*%predcoefs[loc,]))*suicideproj[[loc]]
      
      # - SUM AN (ABS) BY TEMPERATURE RANGE AND PERIOD, STORE IN ARRAY BEFORE THE ITERATIONS
      # NB: ACCOUNT FOR NO TEMPERATURE BELOW/ABOVE CEN FOR GIVEN PERIODS
      ansim[loc, , "tot","abs",j,i,1] <- tapply(an,seqperiod,sum)
      ansim[loc, , "below","abs",j,i,1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
      ansim[loc, , "above","abs",j,i,1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
      
      # (6) ESTIMATE UNCERTAINTY OF THE PROJECTED AN:
      # - SAMPLE COEF ASSUMING A MULTIVARIATE NORMAL DISTRIBUTION
      set.seed(13041975+j)
      # coefsim <- mvrnorm(nsim, blupall[[loc]]$blup, blupall[[loc]]$vcov)
      metacoefsim <- mvrnorm(nsim, coef(mvoverall), vcov(mvoverall))
      
      # Design matrix from second stage, expand for the multivariate outcome
      design_mat = model.matrix(~ regionnm + gdp + avgtmean + rangetmean, 
                                data = metavariable)[loc,] %x% diag(nc)
      
      # Obtain fixed city coef for each simulated meta coef set
      fixsim <- metacoefsim %*% (design_mat)
      
      # Simulate the random part
      ransim <- mvrnorm(nsim, blupres[[loc]]$blup, blupres[[loc]]$vcov) #nearPD(blupres[[loc]]$vcov)$mat
      # Total simulated coefs
      coefsim <- fixsim + ransim
      
      # - LOOP ACROSS ITERATIONS
      for(s in seq(nsim)) {
        
        # COMPUTE THE DAILY CONTRIBUTIONS OF ATTRIBUTABLE DEATHS
        an <- (1-exp(-bvarcen%*%coefsim[s,]))*suicideproj[[loc]]
        
        # STORE THE ATTRIBUTABLE MORTALITY
        ansim[loc, ,"tot","abs",j,i,s+1] <- tapply(an,seqperiod,sum)
        ansim[loc, ,"below","abs",j,i,s+1] <- tapply(an[!indheat],factor(seqperiod[!indheat]),sum)
        ansim[loc, ,"above","abs",j,i,s+1] <- tapply(an[indheat],factor(seqperiod[indheat]),sum)
        
      }
    }
    
  }
}

# ESTIMATE AN IN EACH PERIOD RELATIVE TO CURRENT DAYS (2010-19)
ansim[,,,"rel",,,] <- ansim[,,,"abs",,,] - ansim[,rep("2010-19",length(levels(seqperiod))),,"abs",,,]

# SAVE SIMULATED AN DATA
save(ansim, file="/data/output/ansim.RData")

################################################################################
# COMPUTE AN/AF (95%CI) IN THE ENSEMBLE, BY REGION & RANGE & PERIOD & SSP

# CREATE NEW OBJECTS TO STORE RESULTS
# 1. We now create 4 new arrays for city (2 arrays to store the abs and rel AN, and another 2
#   to store the estimated rel and abs AF) with 5 dimensions each to store the 
#   ensemble estimates (average impacts across GCMs) with the empirical 
#   95% confidence intervals. 
estci = c("est","ci.l","ci.u")
anabs_city = afabs_city = anrel_city = afrel_city = array(NA,dim=c(length(city_list$citynm), length(levels(seqperiod)), length(estci),length(temprange),length(ssp)), dimnames=list(unique(city_list$citynm), levels(seqperiod),estci,temprange,names(ssp)))

# 2. We create another 4 new arrays for region 
# (1) Create a list of city names for each region
code = list()
for (i in seq_along(unique(city_list$regionnm))) {
  region_name = unique(city_list$regionnm)[i]
  code[[region_name]] = city_list$citynm[city_list$regionnm == region_name]
}

# (2) Total number of projected suicides by region over a 10-year period
suicide_region = list()
for (region in names(code)) {
  cities = code[[region]]
  suicide_region[[region]] <- sum(unlist(suicideperiod[cities]))  # The sum of each region
}

# (3) 5 dimensions correspond to regions, 10-year period, point estimate, CI, temperature range and scenario. 
anabs <- afabs <- anrel <- afrel <- array(NA,dim=c(length(unique(city_list$regionnm)), length(levels(seqperiod)),
                                                   length(estci),length(temprange),length(ssp)), 
                                          dimnames=list(unique(city_list$regionnm), levels(seqperiod),estci,temprange,names(ssp)))

# 3. ATTRIBUTABLE NUMBERS 
# (1) ABSOLUTE - GCM ensemble by city
anabs_city[,,"est",,"SSP126"] <- apply(ansim[,,,"abs",,"SSP126",1],1:3,mean)
anabs_city[,,"ci.l",,"SSP126"] <- apply(ansim[,,,"abs",,"SSP126",-1],1:3,quantile,0.025)
anabs_city[,,"ci.u",,"SSP126"] <- apply(ansim[,,,"abs",,"SSP126",-1],1:3,quantile,0.975)

anabs_city[,,"est",,"SSP245"] <- apply(ansim[,,,"abs",,"SSP245",1],1:3,mean)
anabs_city[,,"ci.l",,"SSP245"] <- apply(ansim[,,,"abs",,"SSP245",-1],1:3,quantile,0.025)
anabs_city[,,"ci.u",,"SSP245"] <- apply(ansim[,,,"abs",,"SSP245",-1],1:3,quantile,0.975)

anabs_city[,,"est",,"SSP585"] <- apply(ansim[,,,"abs",,"SSP585",1],1:3,mean)
anabs_city[,,"ci.l",,"SSP585"] <- apply(ansim[,,,"abs",,"SSP585",-1],1:3,quantile,0.025)
anabs_city[,,"ci.u",,"SSP585"] <- apply(ansim[,,,"abs",,"SSP585",-1],1:3,quantile,0.975)

# (2) ABSOLUTE - Aggregate by region then GCM-ensemble
for (region in names(code)) {
  cities = code[[region]]
  cat(region, " ")
  
  # The sum of each region, then average GCMs
  gcm_ensemble = apply(apply(ansim[cities,,,"abs",,,1], 2:5, sum, na.rm = TRUE), c(1,2,4), mean)
  gcm_ci.l = apply(apply(ansim[cities,,,"abs",,,-1], 2:6, sum, na.rm = TRUE), c(1,2,4), quantile,0.025)
  gcm_ci.u = apply(apply(ansim[cities,,,"abs",,,-1], 2:6, sum, na.rm = TRUE), c(1,2,4), quantile,0.975)
  
  for (ssp in dimnames(gcm_ensemble)[[3]]) {
    anabs[region, , "est", , ssp] <- gcm_ensemble[, , ssp]
    anabs[region, , "ci.l", , ssp] <- gcm_ci.l[, , ssp]
    anabs[region, , "ci.u", , ssp] <- gcm_ci.u[, , ssp]
  }
}

# (3) RELATIVE - GCM ensemble by city
anrel_city[,,"est",,"SSP126"] <- apply(ansim[,,,"rel",,"SSP126",1],1:3,mean)
anrel_city[,,"ci.l",,"SSP126"] <- apply(ansim[,,,"rel",,"SSP126",-1],1:3,quantile,0.025)
anrel_city[,,"ci.u",,"SSP126"] <- apply(ansim[,,,"rel",,"SSP126",-1],1:3,quantile,0.975)

anrel_city[,,"est",,"SSP245"] <- apply(ansim[,,,"rel",,"SSP245",1],1:3,mean)
anrel_city[,,"ci.l",,"SSP245"] <- apply(ansim[,,,"rel",,"SSP245",-1],1:3,quantile,0.025)
anrel_city[,,"ci.u",,"SSP245"] <- apply(ansim[,,,"rel",,"SSP245",-1],1:3,quantile,0.975)

anrel_city[,,"est",,"SSP585"] <- apply(ansim[,,,"rel",,"SSP585",1],1:3,mean)
anrel_city[,,"ci.l",,"SSP585"] <- apply(ansim[,,,"rel",,"SSP585",-1],1:3,quantile,0.025)
anrel_city[,,"ci.u",,"SSP585"] <- apply(ansim[,,,"rel",,"SSP585",-1],1:3,quantile,0.975)

# (4) RELATIVE - Aggregate by region
for (region in names(code)) {
  cities = code[[region]]
  cat(region, " ")
  
  # The sum of each region, then average GCMs
  gcm_ensemble = apply(apply(ansim[cities,,,"rel",,,1], 2:5, sum, na.rm = TRUE), c(1,2,4), mean)
  gcm_ci.l = apply(apply(ansim[cities,,,"rel",,,-1], 2:6, sum, na.rm = TRUE), c(1,2,4), quantile,0.025)
  gcm_ci.u = apply(apply(ansim[cities,,,"rel",,,-1], 2:6, sum, na.rm = TRUE), c(1,2,4), quantile,0.975)
  
  for (ssp in dimnames(gcm_ensemble)[[3]]) {
    anrel[region, , "est", , ssp] <- gcm_ensemble[, , ssp]
    anrel[region, , "ci.l", , ssp] <- gcm_ci.l[, , ssp]
    anrel[region, , "ci.u", , ssp] <- gcm_ci.u[, , ssp]
  }
}

# 4. ATTRIBUTABLE FRACTION
# (1) BY CITY
for (i in city_list$citynm) {
  # ABSOLUTE - BY CITY
  afabs_city[i,,,,] <- anabs_city[i,,,,]/suicideperiod[[i]]*100
  
  # RELATIVE - BY CITY
  afrel_city[i,,,,] <- anrel_city[i,,,,]/suicideperiod[[i]]*100
}

# (2) AGGREGATE BY REGION
#   Calculate the sum of ATTRIBUBLE NUMBERS in each region divided by 
#   the total number of projected suicides in each region as a percentage (%)
for (i in names(suicide_region)) {
  # ABSOLUTE - BY REGION
  afabs[i,,,,] <- anabs[i,,,,]/suicide_region[[i]]*100
  
  # RELATIVE - BY REGION
  afrel[i,,,,] <- anrel[i,,,,]/suicide_region[[i]]*100
}

# 5. SAVE AN, AF DATASETS
save(anabs, anrel, afabs, afrel, file="/data/output/ANAF.RData")
save(anabs_city,afabs_city,anrel_city,afrel_city, file="/data/output/ANAF_city.RData")

################################################################################
# - FIGURE 2 (Difference in total AF%)

# 1. Prepare Data
# afrel has columns corresponding to region, time, and SSP scenarios
#   Reshape the data to long format for easier plotting
afrel_long <- melt(afrel, id.vars = c("region", "time", "scenario"),
                   measure.vars = c("est", "ci.l", "ci.u"))
colnames(afrel_long) <- c("region", "time", "estci", "temprange", "scenario", "value")
afrel_long$temprange = ifelse(afrel_long$temprange == "below", "cold",
                              ifelse(afrel_long$temprange == "above", "warm", "tot"))

afrel_wide <- afrel_long %>%
  pivot_wider(names_from = estci, values_from = value) %>%
  mutate(time = recode(time,
                       "2010-19" = "10s",
                       "2020-29" = "20s",
                       "2030-39" = "30s",
                       "2040-49" = "40s",
                       "2050-59" = "50s"),
         scenario = recode(scenario,
                           "SSP126" = "SSP1-2.6",
                           "SSP245" = "SSP2-4.5",
                           "SSP585" = "SSP5-8.5"))

# 2. Plot the data
plot_list <- list()
regions <- unique(afrel_wide$region)
for (rg in regions) {
  df <- afrel_wide %>% filter(region == rg, temprange == "tot") 
  
  p <- ggplot(df, aes(x = time, y = est, fill = scenario)) + 
    geom_hline(yintercept = 0, color = "dimgray", linetype = "dotted", linewidth = 0.2) +
    geom_bar(stat = "identity", alpha = 0.7, width = 0.8) + 
    geom_errorbar(aes(ymin = ci.l, ymax = ci.u), width = 0.3, linewidth = 0.3, color="black") +
    geom_point(size = 0.2, color="black") +
    
    scale_fill_manual(values = c(
      "SSP1-2.6" = "#1C6BA0",   # Blue
      "SSP2-4.5" = "#DAA520",   # Goldenrod
      "SSP5-8.5" = "#B22222"    # Firebrick Red
    )) + 
    
    scale_x_discrete(limits = c("10s", "20s", "30s", "40s", "50s"), expand = c(0, 1)) +
    facet_wrap(~scenario, nrow = 1) +
    ylim(-0.8, 4) +
    ggtitle(rg) +
    labs(y = NULL, x = NULL) +
    theme_minimal() +
    theme(
      legend.position = "none",
      plot.title = element_text(size = 7, face = "bold", hjust = 0.5, vjust = -5),
      axis.text.x = element_text(size = 2.5, vjust = 1.5),
      axis.text.y = element_text(size = 4),
      strip.text = element_text(size = 5, vjust = -2),
      strip.background = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0.1, "lines"),
      panel.spacing.x = unit(0, "lines"),
      axis.title.y = element_blank(),
      axis.line.x = element_line(size = 0.3),
      axis.line.y = if (any(df$scenario == "SSP1-2.6")) element_line(size = 0.3) else element_blank(),
      axis.ticks.y = element_line(size = 0.3, color = "black"),
      axis.ticks.x = element_line(size = 0.3, color = "black"),
      axis.ticks.length = unit(0.1, "lines")
    )
  
  plot_list[[length(plot_list) + 1]] <- p
}

# 3. legend 
legend_dummy <- data.frame(
  x = factor(c("A", "B", "C")),
  y = c(1, 1, 1),
  scenario = factor(c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"), levels = c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5"))
)

legend_all <- cowplot:::get_plot_component(
  ggplot(legend_dummy, aes(x = x, y = y, fill = scenario)) +
    geom_col(position = "stack", alpha = 0.7) +
    scale_fill_manual(values = c(
      "SSP1-2.6" = "#1C6BA0",   # Blue
      "SSP2-4.5" = "#DAA520",   # Goldenrod
      "SSP5-8.5" = "#B22222"    # Firebrick Red
    )) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.text = element_text(size=8),
      legend.title = element_blank(),
      legend.key.size = unit(0.3, "cm")
    ),
  "guide-box",
  return_all = TRUE
)
legend_plot <- legend_all[[3]]

# 4. Difference in Excess Suicide (%)
yaxis_grob <- textGrob("Difference in total attributable fraction (AF) (%)", rot = 90, gp = gpar(fontsize = 8))

# 5. Plot grid
pdf("/results/Figure3.pdf", width = 7, height = 3.5)
plots_per_page <- 10
for (i in seq(1, length(plot_list), by = plots_per_page)) {
  page_plots <- plot_list[i:min(i + plots_per_page - 1, length(plot_list))]
  plots_grid <- plot_grid(plotlist = page_plots, ncol = 5)
  
  # y-label
  plots_with_yaxis <- plot_grid(
    plot_grid(NULL, yaxis_grob, NULL, ncol = 1, rel_heights = c(0.03, 0.5, 0.03)),
    plots_grid,
    rel_widths = c(0.02, 1),
    ncol = 2
  )
  
  final_plot <- plot_grid(plots_with_yaxis, legend_plot, ncol = 1, rel_heights = c(1, 0.04))
  print(final_plot)
}
dev.off()
