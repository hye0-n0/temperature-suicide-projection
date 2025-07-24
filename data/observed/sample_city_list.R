# Load required package
library(dplyr)

# Create a sample data frame with region, country, and city columns

# This sample dataset was created with the same regions and countries as the original data. 
# Since the original data cannot be shared, two arbitrary city names were generated for each country. 
# The dataset includes 'regionnm', 'countrynm', and 'citynm' columns for demonstration purposes only.

countries <- c("Australia", "Germany", "Korea", "Japan", "USA", "UK", "Canada",
               "Mexico", "Romania", "Spain", "Switzerland", "Taiwan", "Vietnam", "Brazil",
               "Philippines", "Czech", "China", "CostaRica", "Ecuador", "Guatemala",
               "Paraguay", "Chile", "Estonia", "Italy", "South Africa", "Finland")

# Map each country to a corresponding region
country_to_region <- list(
  "Australia" = "Australia",
  "Germany" = "Central Europe",
  "Korea" = "East Asia",
  "Japan" = "East Asia",
  "USA" = "North America",
  "UK" = "North Europe",
  "Canada" = "North America",
  "Mexico" = "Central America",
  "Romania" = "Central Europe",
  "Spain" = "South Europe",
  "Switzerland" = "Central Europe",
  "Taiwan" = "East Asia",
  "Vietnam" = "South-East Asia",
  "Brazil" = "South America",
  "Philippines" = "South-East Asia",
  "Czech" = "Central Europe",
  "China" = "East Asia",
  "CostaRica" = "Central America",
  "Ecuador" = "South America",
  "Guatemala" = "Central America",
  "Paraguay" = "South America",
  "Chile" = "South America",
  "Estonia" = "North Europe",
  "Italy" = "South Europe",
  "South Africa" = "South Africa",
  "Finland" = "North Europe"
)

# Function to generate two city names per country using the first 4 letters
make_city_names <- function(country) {
  abbrev <- tolower(substr(gsub(" ", "", country), 1, 4))  # get first 4 letters, removing spaces
  paste0(abbrev, "_loc", 1:2)                              # create two city names
}

# Initialize empty vectors to store results
region_vec <- c()
country_vec <- c()
city_vec <- c()

# Loop through each country to create region, country, and city entries
for (country in countries) {
  region <- country_to_region[[country]]                  # find corresponding region
  cities <- make_city_names(country)                      # generate city names
  
  region_vec <- c(region_vec, rep(region, 2))             # repeat region twice
  country_vec <- c(country_vec, rep(country, 2))          # repeat country twice
  city_vec <- c(city_vec, cities)                         # append generated cities
}

# Combine vectors into a data frame
city_list <- data.frame(
  regionnm = region_vec,
  countrynm = country_vec,
  citynm = city_vec,
  stringsAsFactors = FALSE
)

# Add GDP column based on country-level values (in USD per capita)
city_list <- city_list %>%
  mutate(gdp = case_when(
    countrynm == 'Australia' ~ 20536.40,
    countrynm == 'Brazil' ~ 3596.20,
    countrynm == 'Canada' ~ 22109.60,
    countrynm == 'Chile' ~ 10217.30,
    countrynm == 'China' ~ 1740.10,
    countrynm == 'CostaRica' ~ 6900.00,
    countrynm == 'Czech' ~ 11667.00,
    countrynm == 'Ecuador' ~ 6050.00,
    countrynm == 'Estonia' ~ 16586.00,
    countrynm == 'Finland' ~ 32816.20,
    countrynm == 'Germany' ~ 35240.00,
    countrynm == 'Guatemala' ~ 4140.00,
    countrynm == 'Italy' ~ 33426.20,
    countrynm == 'Japan' ~ 31013.60,
    countrynm == 'Mexico' ~ 8666.30,
    countrynm == 'Paraguay' ~ 5260.00,
    countrynm == 'Philippines' ~ 1929.10,
    countrynm == 'Romania' ~ 7777.20,
    countrynm == 'South Africa' ~ 5414.00,
    countrynm == 'Korea' ~ 11255.95,
    countrynm == 'Spain' ~ 14787.80,
    countrynm == 'Switzerland' ~ 53256.00,
    countrynm == 'Taiwan' ~ 17200.00,
    countrynm == 'UK' ~ 25980.20,
    countrynm == 'USA' ~ 30068.23,
    countrynm == 'Vietnam' ~ 1542.70,
    TRUE ~ NA_real_  # fallback if country not matched
  ))

# Save the data frame to an RData file
save(city_list, file = "/data/observed/city_list.RData")
