# This script reads in the list of countries eligible for funding and cleans it

library(readxl)
library(dplyr)
library(purrr)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Read in the list of countries eligible for funding, which is in a weird format (numbers and country names are in adjacent columns)
country_list <- read_xls("data_raw/List of countries eligible for funding_April-2026.xls")
head(country_list)

# Function to test if something is a number
is_number <- function(x) {
  !is.na(suppressWarnings(as.numeric(x)))
}

# Look across every adjacent column pair:
# left column = number, right column = country name
clean_country_list <- map_dfr(2:ncol(country_list), function(i) {
  left_col <- country_list[[i - 1]]
  right_col <- country_list[[i]]

  tibble(
    number = left_col,
    country_name = right_col
  ) %>%
    mutate(
      number = suppressWarnings(as.numeric(number)),
      country_name = country_name %>%
        as.character() %>%
        str_remove_all("\\*") %>% # remove asterisks
        str_trim()
    ) %>%
    filter(
      !is.na(number),
      !is.na(country_name),
      country_name != "",
      str_detect(country_name, "[A-Za-z]")
    )
}) %>%
  select(country_name) %>%
  distinct() %>%
  arrange(country_name)

clean_country_list <- clean_country_list %>%
  mutate(
    country_name_clean = country_name %>%
      str_trim() %>%
      str_remove_all("\\*") %>%
      str_remove_all("\\s*\\([^\\)]+\\)") %>%
      str_squish()
  ) %>%
  mutate(
    ISO_A3 = countrycode(
      country_name_clean,
      origin = "country.name",
      destination = "iso3c",
      custom_match = c("Micronesia" = "FSM"))) %>%
  select(-country_name_clean)

# Save checklist
write.csv(clean_country_list, "data_processed/clean_data/clean_country_list.csv", row.names = FALSE)
