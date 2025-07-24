################################################################################
# FILE NAME: calibration.R @Codes/MAIN
# Purpose: Calibration by applying Bias correction 
#
# SAVE RESULT
#     ssp126cal_50s, ssp245cal_50s, ssp585cal_50s: 
#           calibrated modeled temperature data from 2010s to 2050S (.RData)
################################################################################
# 00 BIAS-CORRECTION
################################################################################
# Load modeled temperature
load("/data/modeled/ssp126.RData")
load("/data/modeled/ssp245.RData")
load("/data/modeled/ssp585.RData")

# LOAD BIAS CORRECTION FUNCTION
# This is a function created to apply the bias-correction method developed 
#    within ISI-MIP (Hempel et al. 2013). More details on the calibration 
#    procedure are described in the "fhempel.R" code.
# RE-CALIBRATE USING THE BIAS CORRECTION FUNCTION
source("/data/modeled/fhempel.r")
################################################################################
# 1. Transformation data structure for calibration
ssp126_list = ssp245_list = ssp585_list = list()

for(name in colnames(ssp585)[10:ncol(ssp585)]){
  ssp126_list[[name]] = ssp126 %>%
    dplyr::select(date, climate_forcing, all_of(name)) %>% 
    pivot_wider(names_from = climate_forcing, values_from = all_of(name)) %>% 
    as.data.frame()
  
  ssp245_list[[name]] = ssp245 %>%
    dplyr::select(date, climate_forcing, all_of(name)) %>% 
    pivot_wider(names_from = climate_forcing, values_from = all_of(name)) %>% 
    as.data.frame()
  
  ssp585_list[[name]] = ssp585 %>%
    dplyr::select(date, climate_forcing, all_of(name)) %>% 
    pivot_wider(names_from = climate_forcing, values_from = all_of(name)) %>% 
    as.data.frame()
}

# 2.Sorting the modeled data
ssp126_list = ssp126_list[names(slist)]
ssp245_list = ssp245_list[names(slist)]
ssp585_list = ssp585_list[names(slist)]

# 3.Calibration
ssp126cal = ssp245cal = ssp585cal = list()

for(name in names(slist)){
  cat(name,"")
  
  ssp126cal[[name]] = fhempel(slist[[name]][c("date","tmean")], ssp126_list[[name]])
  ssp245cal[[name]] = fhempel(slist[[name]][c("date","tmean")], ssp245_list[[name]])
  ssp585cal[[name]] = fhempel(slist[[name]][c("date","tmean")], ssp585_list[[name]])
}

# 4.Extract data upto the 2050s
filter_until_2059 <- function(df) {
  df_filtered = df[df$date <= as.Date("2059-12-31") & df$date >= as.Date("2010-01-01"), ]
  df_filtered = df_filtered[!(format(df_filtered$date, "%m-%d") == "02-29"), ]
  
  return(df_filtered)
}

ssp126cal_50s = lapply(ssp126cal, filter_until_2059)
ssp245cal_50s = lapply(ssp245cal, filter_until_2059)
ssp585cal_50s = lapply(ssp585cal, filter_until_2059)

# 5.Save modeled temperature 
save(ssp126cal_50s, file="/data/modeled/ssp126cal_50s.RData")
save(ssp245cal_50s, file="/data/modeled/ssp245cal_50s.RData")
save(ssp585cal_50s, file="/data/modeled/ssp585cal_50s.RData")
