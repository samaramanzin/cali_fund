# This script reads the list of countries eligible for funding and cleans it
# into a country list with eligibility categories for the allocation models.

library(readxl)
library(dplyr)
library(tidyr)
library(countrycode)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Read the country list, which has country categories spread across adjacent
# rank/name column pairs.
raw_country_list <- read_xls(
  "data_raw/List of countries eligible for funding_April-2026.xls",
  sheet = "List of countries",
  col_names = FALSE
)

category_columns <- c(2, 4, 6, 8)
category_names <- as.character(unlist(raw_country_list[1, category_columns], use.names = FALSE))

country_categories_raw <- bind_rows(lapply(seq_along(category_names), function(i) {
  cols <- c(1, 2) + (i - 1) * 2

  tibble(
    raw_rank = as.numeric(unlist(raw_country_list[-c(1, 2), cols[1]], use.names = FALSE)),
    country_name_raw = as.character(unlist(raw_country_list[-c(1, 2), cols[2]], use.names = FALSE)),
    category_raw = category_names[i]
  )
}))

custom_country_names <- c(
  "Burkino Faso" = "Burkina Faso",
  "Dem. Rep.Congo" = "Democratic Republic of the Congo",
  "Lao People's Dem. Rep." = "Lao People's Democratic Republic",
  "United Rep. Tanzania" = "United Republic of Tanzania",
  "Micronesia (Fed. States of)" = "Micronesia, Federated States of",
  "Saint Vincent & Grenadines" = "Saint Vincent and the Grenadines",
  "Sao Tome y Principe" = "Sao Tome and Principe",
  "Timor Leste" = "Timor-Leste",
  "Cote d'Ivoire" = "Ivory Coast",
  "Dem. People's Rep.Korea" = "North Korea",
  "Iran (Islamic Republic of)" = "Iran",
  "Turkiye" = "Turkey",
  "State of Palestine" = "Palestine",
  "Syrian Arab Republic" = "Syria",
  "Bosnia-Herzegovina" = "Bosnia and Herzegovina",
  "Republic of Moldova" = "Moldova",
  "Russian Federation" = "Russia"
)

country_categories <- country_categories_raw %>%
  filter(
    !is.na(raw_rank),
    !is.na(country_name_raw),
    trimws(country_name_raw) != ""
  ) %>%
  mutate(
    # The workbook marks SIDS that are also LDCs with an asterisk.
    is_starred_ldc = grepl("*", country_name_raw, fixed = TRUE),
    country_name_raw = trimws(gsub("*", "", country_name_raw, fixed = TRUE)),
    country_name_for_iso = recode(country_name_raw, !!!custom_country_names),
    ISO_A3 = countrycode(
      country_name_for_iso,
      origin = "country.name",
      destination = "iso3c"
    ),
    country_category = recode(
      category_raw,
      "LEAST DEVELOPING COUNTRIES" = "Least developed countries",
      "SMALL ISLAND DEVELOPING STATES" = "Small island developing states",
      "OTHER DEVELOPING COUNTRIES" = "Other developing countries",
      "ECONOMIES IN TRANSITION" = "Economies in transition"
    )
  )

missing_iso <- country_categories %>%
  filter(is.na(ISO_A3)) %>%
  select(country_name_raw, category_raw)

if (nrow(missing_iso) > 0) {
  print(missing_iso)
  stop("Some countries could not be converted to ISO_A3 codes.")
}

clean_country_list <- read.csv("data_processed/clean_data/clean_country_list.csv")

clean_country_list_with_categories <- country_categories %>%
  group_by(ISO_A3) %>%
  summarise(
    country_name = first(country_name_raw),
    country_category = paste(unique(country_category), collapse = "; "),
    is_ldc = any(country_category == "Least developed countries" | is_starred_ldc),
    is_sids = any(country_category == "Small island developing states"),
    .groups = "drop"
  ) %>%
  left_join(clean_country_list, by = "ISO_A3", suffix = c("_raw", "_clean")) %>%
  transmute(
    country_name = coalesce(country_name_clean, country_name_raw),
    ISO_A3,
    country_category,
    is_ldc,
    is_sids
  )

missing_categories <- anti_join(clean_country_list, clean_country_list_with_categories, by = "ISO_A3")
extra_categories <- anti_join(clean_country_list_with_categories, clean_country_list, by = "ISO_A3")

if (nrow(missing_categories) > 0 || nrow(extra_categories) > 0) {
  print("Countries in clean_country_list.csv missing from category output:")
  print(missing_categories)
  print("Countries in category output missing from clean_country_list.csv:")
  print(extra_categories)
  stop("Category country list does not match clean_country_list.csv.")
}

write.csv(
  clean_country_list_with_categories,
  "data_processed/clean_data/clean_country_list_with_categories.csv",
  row.names = FALSE
)

print("Country category counts:")
print(table(clean_country_list_with_categories$is_ldc, clean_country_list_with_categories$is_sids))
print("Wrote data_processed/clean_data/clean_country_list_with_categories.csv")
