# This script processes the CBD party languages data to create the IPLC TK indicator dataset.
# This is a possible dataset for Criterion D

library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Read the country list
country_list <- read.csv("data_processed/clean_data/clean_country_list.csv")

# Read CBD party languages data
languages <- read.csv("data_raw/cbd_party_languages.csv")

# Extract and process linguistic diversity data (Indicator 21.CT.3)
# Count unique languages per country
linguistic_diversity <- languages %>%
  filter(core.countries != "") %>%
  # Separate multiple countries into rows (e.g., "GE | RU" becomes two rows)
  separate_rows(core.countries, sep = "\\|") %>%
  mutate(core.countries = trimws(core.countries)) %>%
  # Convert ISO2 to ISO3 country codes
  mutate(iso3 = countrycode(core.countries, origin = "iso2c", destination = "iso3c")) %>%
  # Count unique languages per country
  group_by(iso3) %>%
  summarise(
    indicator_21_ct_3 = n_distinct(glottocode, na.rm = TRUE),
    .groups = "drop"
  )

# Create placeholders for the other three indicators
# These will be NA/empty for now as they should be sourced from other data
indicator_22_1 <- country_list %>%
  select(ISO_A3) %>%
  mutate(indicator_22_1 = NA_real_) %>%
  rename(iso3 = ISO_A3)

indicator_9_2 <- country_list %>%
  select(ISO_A3) %>%
  mutate(indicator_9_2 = NA_real_) %>%
  rename(iso3 = ISO_A3)

indicator_22_b <- country_list %>%
  select(ISO_A3) %>%
  mutate(indicator_22_b = NA_real_) %>%
  rename(iso3 = ISO_A3)

# Combine all indicators
iplc_indicators <- indicator_22_1 %>%
  left_join(linguistic_diversity, by = "iso3") %>%
  left_join(indicator_9_2, by = "iso3") %>%
  left_join(indicator_22_b, by = "iso3")

# Normalize each indicator to 0-1 range (only for non-NA values)
iplc_normalized <- iplc_indicators %>%
  mutate(
    # Normalize 21.CT.3 (linguistic diversity index)
    indicator_21_ct_3_norm = (indicator_21_ct_3 - min(indicator_21_ct_3, na.rm = TRUE)) / 
                              (max(indicator_21_ct_3, na.rm = TRUE) - min(indicator_21_ct_3, na.rm = TRUE)),
    # For now, use NA for others (to be filled with actual data later)
    indicator_22_1_norm = NA_real_,
    indicator_9_2_norm = NA_real_,
    indicator_22_b_norm = NA_real_
  )

# Calculate weighted average (all four indicators have equal weight = 0.25)
iplc_weighted <- iplc_normalized %>%
  mutate(
    # Since only indicator_21_ct_3_norm has data, use it directly
    # When other indicators are available, this can be updated to include them
    iplc_tk = indicator_21_ct_3_norm
  ) %>%
  select(iso3, iplc_tk)

# Join with country list to get country names and align with other data files
iplc_data <- country_list %>%
  left_join(iplc_weighted, by = c("ISO_A3" = "iso3")) %>%
  select(ISO_A3, country_name, iplc_tk) %>%
  rename("iso3" = "ISO_A3")

# Write output CSV
write.csv(iplc_data, "data_processed/clean_data/iplc_tk_data.csv", row.names = FALSE)

# Check for missing countries
iplc_data <- read.csv("data_processed/clean_data/iplc_tk_data.csv")
missing_countries <- filter(iplc_data, is.na(iplc_tk)) %>% pull(iso3)
print("Missing countries in IPLC TK data:")
print(missing_countries)
