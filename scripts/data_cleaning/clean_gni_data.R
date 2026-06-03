# Code to clean data from GNI
# This is a possible dataset for Criterion C
# Here the year 2023 is used because it is the most recent and complete year of data

library(readxl)

setwd("/Users/samaramanzin/Desktop/cali_fund")

country_list <- read.csv("data_processed/clean_data/clean_country_list.csv")

## cleaning API_NY.GNP.PCAP.CD_DS2_en_excel_v2_463.xls

raw <- read_excel("data_raw/API_NY.GNP.PCAP.CD_DS2_en_excel_v2_463.xls", skip = 2) %>%
    select(c(`Country Name`, `Country Code`, `2023`)) %>%
    rename(country_name = `Country Name`, iso3 = `Country Code`, gni_per_capita = `2023`)
head(raw)

# filter to only countries in our list and invert values for capacity needs (higher GNI = lower need)
clean <- raw %>%
    filter(iso3 %in% country_list$ISO_A3) %>%
    mutate(inverse = 1/gni_per_capita)

write.csv(clean, "data_processed/clean_data/gni_data.csv", row.names = FALSE)

# Check for missing countries
missing_countries <- setdiff(country_list$ISO_A3, clean$iso3)
print("Missing countries in GNI data:")
print(missing_countries)


#######################################################
## cleaning API_NY.GNP.PCAP.CD_DS2_en_csv_v2_276956.csv

raw <- read.csv("data_raw/API_NY.GNP.PCAP.CD_DS2_en_csv_v2_276956.csv", skip = 4) %>%
    select(c(Country.Name, Country.Code, X2023)) %>%
    rename(country_name = Country.Name, iso3 = Country.Code, gni_per_capita = X2023)
head(raw)

# filter to only countries in our list and invert values for capacity needs (higher GNI = lower need)
clean <- raw %>%
    filter(iso3 %in% country_list$ISO_A3) %>%
    mutate(inverse = 1/gni_per_capita)

write.csv(clean, "data_processed/clean_data/gni_data_2.csv", row.names = FALSE)

# Check for missing countries
missing_countries <- setdiff(country_list$ISO_A3, clean$iso3)
print("Missing countries in GNI data:")
print(missing_countries)
