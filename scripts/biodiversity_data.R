library(readxl)
library(dplyr)
library(countrycode)
library(rnaturalearth)
library(sf)

setwd("/Users/samaramanzin/Desktop/cali_fund")

country_list <- read.csv("data_processed/clean_country_list.csv")

gef <- read_excel("data_raw/Thrown together GEF-8 numbers.xlsx")

gef_countries <- gef %>%
  select(`ISO3 country code`, `Country`, `...5`) %>%
  rename(country_name = `Country`, gef_allocation = `...5`)

####
world <- ne_countries(scale = "medium", returnclass = "sf")

iso_lookup <- world %>%
  st_drop_geometry() %>%
  transmute(
    iso3 = iso_a3,
    sovereign_iso3 = countrycode(sovereignt, "country.name", "iso3c"),
    territory_name = name,
    sovereign_name = sovereignt
  ) %>%
  distinct() %>%
  # Manual mappings for territories that don't convert automatically
  mutate(sovereign_iso3 = case_when(
    iso3 == "CCK" ~ "AUS",  # Cocos (Keeling) Islands -> Australia
    iso3 == "CXR" ~ "AUS",  # Christmas Island -> Australia
    iso3 == "HMD" ~ "AUS",  # Heard Island -> Australia
    iso3 == "NFK" ~ "AUS",  # Norfolk Island -> Australia
    iso3 == "ASH" ~ "AUS",  # Ashmore and Cartier Islands -> Australia
    iso3 == "COK" ~ "NZL",  # Cook Islands -> New Zealand
    iso3 == "NIU" ~ "NZL",  # Niue -> New Zealand
    iso3 == "TKL" ~ "NZL",  # Tokelau -> New Zealand
    iso3 == "GUF" ~ "FRA",  # French Guiana -> France
    iso3 == "PYF" ~ "FRA",  # French Polynesia -> France
    iso3 == "REU" ~ "FRA",  # Reunion -> France
    iso3 == "BLM" ~ "FRA",  # Saint Barthelemy -> France
    iso3 == "MAF" ~ "FRA",  # Saint Martin -> France
    iso3 == "SPM" ~ "FRA",  # Saint Pierre and Miquelon -> France
    iso3 == "ATF" ~ "FRA",  # French Southern Territories -> France
    iso3 == "GIB" ~ "GBR",  # Gibraltar -> UK
    iso3 == "GGY" ~ "GBR",  # Guernsey -> UK
    iso3 == "IMN" ~ "GBR",  # Isle of Man -> UK
    iso3 == "JEY" ~ "GBR",  # Jersey -> UK
    iso3 == "KYM" ~ "GBR",  # Cayman Islands -> UK
    iso3 == "FLK" ~ "GBR",  # Falkland Islands -> UK
    iso3 == "SHN" ~ "GBR",  # Saint Helena -> UK
    iso3 == "TCA" ~ "GBR",  # Turks and Caicos -> UK
    iso3 == "VGB" ~ "GBR",  # British Virgin Islands -> UK
    iso3 == "BGI" ~ "GBR",  # British Indian Ocean Territory -> UK
    iso3 == "AIA" ~ "GBR",  # Anguilla -> UK
    iso3 == "MNT" ~ "GBR",  # Montserrat -> UK
    iso3 == "FSK" ~ "GBR",  # Falkland Islands -> UK (duplicate)
    iso3 == "PCN" ~ "GBR",  # Pitcairn Islands -> UK
    iso3 == "GRL" ~ "DNK",  # Greenland -> Denmark
    iso3 == "FRO" ~ "DNK",  # Faroe Islands -> Denmark
    iso3 == "CUW" ~ "NLD",  # Curacao -> Netherlands
    iso3 == "SXM" ~ "NLD",  # Sint Maarten -> Netherlands
    iso3 == "BES" ~ "NLD",  # Bonaire, Sint Eustatius, Saba -> Netherlands
    iso3 == "ABW" ~ "NLD",  # Aruba -> Netherlands
    iso3 == "ESH" ~ "ESP",  # Western Sahara -> Spain
    iso3 == "MAC" ~ "PRT",  # Macau -> Portugal
    iso3 == "CPV" ~ "PRT",  # Cape Verde -> Portugal (if mapped)
    iso3 == "SJM" ~ "NOR",  # Svalbard and Jan Mayen -> Norway
    iso3 == "ALA" ~ "FIN",  # Åland Islands -> Finland
    iso3 == "KOS" ~ "SRB",  # Kosovo -> Serbia
    iso3 == "HKG" ~ "CHN",  # Hong Kong -> China
    iso3 == "TWN" ~ "CHN",  # Taiwan -> China
    iso3 == "ASM" ~ "USA",  # American Samoa -> USA
    iso3 == "GUM" ~ "USA",  # Guam -> USA
    iso3 == "MNP" ~ "USA",  # Northern Mariana Islands -> USA
    iso3 == "PRI" ~ "USA",  # Puerto Rico -> USA
    iso3 == "VIR" ~ "USA",  # US Virgin Islands -> USA
    TRUE ~ sovereign_iso3
  ))

gef_reconciled <- gef_countries %>%
  left_join(iso_lookup, by = c("ISO3 country code" = "iso3")) %>%
  mutate(
    join_iso3 = coalesce(sovereign_iso3, `ISO3 country code`)
  ) %>%
  filter(!grepl("^ZZ", `country_name`))

# Aggregate gef_allocation by country and its territories
gef_countries_grouped <- gef_reconciled %>%
  group_by(join_iso3) %>%
  summarise(
    country_name = first(na.omit(sovereign_name)) %||% first(na.omit(country_name)),
    ISO3 = first(join_iso3),
    gef_allocation = sum(gef_allocation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(`ISO3 country code` = ISO3) %>%
  select(`ISO3 country code`, country_name, gef_allocation) %>%
  filter(`ISO3 country code` %in% country_list$ISO_A3)  # Keep only countries in our list

# Save the aggregated data
write.csv(gef_countries_grouped, "data_processed/gef_countries.csv", row.names = FALSE)

