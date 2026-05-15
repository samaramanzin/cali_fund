# Code to clean capacity data from the UN scale of assessments dataset
# This is a possible dataset for Criterion C

library(readxl)
library(dplyr)
library(countrycode)
library(stringr)
library(readr)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Read in country list for filtering later
country_list <- read.csv("data_processed/clean_data/clean_country_list.csv")

# Read in UN scale of assessments data
raw <- read_excel("data_raw/Scale of Assessments for RB 1946-2027.xlsx", skip = 2)
head(raw)

# Rename first column
names(raw)[1] <- "country_name"

# Find most recent year column
year_cols <- names(raw)[str_detect(names(raw), "^\\d{4}$")]
most_recent_year <- max(as.numeric(year_cols), na.rm = TRUE)

clean <- raw %>%
  select(
    country_name,
    value = all_of(as.character(most_recent_year))
  ) %>%
  mutate(
    country_name = country_name %>% 
    str_remove_all("\\*") %>%
    str_remove_all("\\*") %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      str_remove_all("\\s+[a-z]/$") %>%
      str_squish(),
    value = as.character(value) %>%
      str_squish(),
    value = na_if(value, "-"),
    value = parse_number(value)
  ) %>%
  # remove notes/footer starting at Total
  filter(!(country_name == "Total")) %>%
  filter(
    !is.na(country_name),
    country_name != "",
    !is.na(value)
  )

# adding iso3 codes
clean_iso <- clean %>%
  mutate(iso3 = countrycode(country_name, origin = "country.name", 
                            destination = "iso3c", custom_match = c("Micronesia" = "FSM")))

# filtering
clean_filtered <- clean_iso %>%
  filter(iso3 %in% country_list$ISO_A3)

# inverting values for capacity needs (higher value = higher need)
inverse <- clean_filtered %>%
  mutate(inverse = 1/value) %>%
  select(iso3, country_name, inverse)

write.csv(inverse, "data_processed/clean_data/capacity.csv", row.names = FALSE)

# Check for missing countries
inverse <- read.csv("data_processed/clean_data/capacity.csv")
missing_countries <- setdiff(country_list$ISO_A3, inverse$iso3)
print("Missing countries in capacity data:")
print(missing_countries)
