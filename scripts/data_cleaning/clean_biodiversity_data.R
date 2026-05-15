# This script cleans the GEF allocation data
# This is a possible dataset for Criterion A

library(readxl)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(sf)

setwd("/Users/samaramanzin/Desktop/cali_fund")

country_list <- read.csv("data_processed/clean_data/clean_country_list.csv")

gef <- read_excel("data_raw/Thrown together GEF-8 numbers.xlsx")

gef_countries <- gef %>%
  select(`ISO3 country code`, `Country`, `...5`) %>%
  rename(country_name = `Country`, gef_allocation = `...5`)

# mergin with country list to get consistent country names and codes
gef_countries_filtered <- gef_countries %>%
  filter(`ISO3 country code` %in% country_list$ISO_A3)

# Save the aggregated data
write.csv(gef_countries_grouped, "data_processed/clean_data/gef_countries.csv", row.names = FALSE)

# missing countries
gef_countries_grouped <- read.csv("data_processed/clean_data/gef_countries.csv")
missing_countries <- setdiff(country_list$ISO_A3, gef_countries_grouped$ISO3.country.code)
print("Missing countries in GEF data:")
print(missing_countries)
