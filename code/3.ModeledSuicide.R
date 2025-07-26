################################################################################
# FILE NAME: 3.ModeldSuicide.R @Codes/MAIN
# Purpose: Create Modeled Suicide Series
#       - Based on constant suicide rates and no changes in demographic structure
#
# SAVE RESULT
#     suicideproj: modeled sucide series (.RData)
################################################################################
# Load observed data
load("/data/observed/slist.RData")

library(tsModel) ; library(MASS) ; library(plm)
library(dlnm) ; library(gnm) ; library(splines) 
library(dplyr) ; library(tidyr) ; library(MASS) ; library(ggplot2)
library(mixmeta) ; library(mvmeta) 
library(purrr); library(patchwork) 
################################################################################
# 03 CREATE MODELED SUICIDE SERIES
################################################################################
# It is computed as the average suicide for each day of the year 
#   from daily observed suicide counts, then repeated along the same projection period
#   of the modeled temperature series.

# 1. Projection period: 2010.01.01 ~ 2059.12.31
projday = seq.Date(as.Date("2010-01-01"), as.Date("2059-12-31"), by = "day")
projday = projday[!(format(projday, "%m-%d") == "02-29")]

suicideproj = list()

# 2. Modeled suicide
for (name in names(slist)) {
  cat(name, "\n") 
  
  # Average suicide for each day of the year
  suicidedoy <- tapply(slist[[name]]$stot,
                       as.numeric(format(slist[[name]]$date, "%j")),
                       mean, na.rm = TRUE)[seq(365)]
  
  # Replace 'NA' to mean value
  while (any(isna <- is.na(suicidedoy))) {
    suicidedoy[isna] <- rowMeans(Lag(suicidedoy, c(-1, 1)), na.rm = TRUE)[isna]
  }
  
  # Repeat along the projection period
  suicideproj[[name]] <- rep(suicidedoy, length = length(projday))
}

# 3. Save modeled suicide data
save(suicideproj, file="/data/modeled/modeled_suicide.RData")

