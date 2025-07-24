################################################################################
# FILE NAME: 0.Setting @Codes/MAIN
# Load packages
# default library
library(dplyr) ; library(tidyr) ; library(MASS) ; library(ggplot2)

# 1. First Stage
library(tsModel) ; library(MASS)
library(dlnm) ; library(gnm) ; library(splines) 

# 2. Second Stage
library(mixmeta) ; library(mvmeta) 
library(purrr); library(patchwork) 

# 4. Projection AN AF
library(reshape2) ;  library(gridExtra) ; library(cowplot)
library(grid) ; library(patchwork)

# Change system languages to English
Sys.setenv(LANG="en"); Sys.setlocale("LC_ALL","C") # day of week in English
options(na.action="na.exclude")
################################################################################