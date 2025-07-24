# Create a sample data of daily suicide data for each city from 1990 to 2010

# Regional average temperature (mean and sd) from Table 2
region_temp_params <- list(
  "North America" = list(mean = 14.2, sd = 9.84),
  "Central America" = list(mean = 19.2, sd = 4.52),
  "South America" = list(mean = 23.2, sd = 4.33),
  "North Europe" = list(mean = 4.4, sd = 8.99),
  "Central Europe" = list(mean = 10.9, sd = 7.27),
  "South Europe" = list(mean = 16.0, sd = 6.59),
  "East Asia" = list(mean = 14.9, sd = 8.84),
  "South-East Asia" = list(mean = 27.3, sd = 2.97),
  "Australia" = list(mean = 18.0, sd = 4.50),
  "South Africa" = list(mean = 18.3, sd = 4.85)
)

# Synthetic Date sequence: 1990-01-01 to 2010-12-31 
dates <- seq.Date(as.Date("1990-01-01"), as.Date("2010-12-31"), by = "day")
n_days <- length(dates)

# Initialize list
slist <- list()

# Use your city list (2 per country)
cities <- city_list$citynm

# Map each city to its region
city_region_map <- setNames(city_list$regionnm, city_list$citynm)

# Set seed for reproducibility
set.seed(42)

# Generate data
for (city in cities) {
  region <- city_region_map[[city]]
  temp_mean <- region_temp_params[[region]]$mean
  temp_sd <- region_temp_params[[region]]$sd
  
  doy <- as.integer(format(dates, "%j"))
  
  # The daily mean temperature (tmean) values were generated artificially
  # - sinusoidal seasonal pattern around the region's mean
  tmean <- temp_mean + 
    0.5 * temp_sd * sin(2 * pi * doy / 365) +  # seasonal pattern
    rnorm(n_days, mean = 0, sd = 0.5 * temp_sd)  # daily variation
  
  df <- data.frame(
    date = dates,
    year = as.integer(format(dates, "%Y")),
    month = as.integer(format(dates, "%m")),
    day = as.integer(format(dates, "%d")),
    doy = doy,
    stot = sample(0:10, n_days, replace = TRUE),  # daily suicide count
    tmean = round(tmean, 1)
  )
  
  slist[[city]] <- df
}

# Example: view structure of one city
str(slist[[1]])
head(slist[[1]])

# Save the data frame to an RData file
save(slist, file="/data/observed/slist.RData")


