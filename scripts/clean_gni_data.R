library(readxl)

setwd("/Users/samaramanzin/Desktop/cali_fund")

country_list <- read.csv("data_processed/clean_country_list.csv")

raw <- read_excel("data_raw/API_NY.GNP.PCAP.CD_DS2_en_excel_v2_463.xls", skip = 2) %>%
    select(c(`Country Name`, `Country Code`, `2023`)) %>%
    rename(country_name = `Country Name`, iso3 = `Country Code`, gni_per_capita = `2023`)
head(raw)

clean <- raw %>%
    filter(iso3 %in% country_list$ISO_A3) %>%
    mutate(inverse = 1/gni_per_capita)

write.csv(clean, "data_processed/gni_data.csv", row.names = FALSE)
