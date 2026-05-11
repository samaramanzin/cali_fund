library(readxl)
library(tidyverse)

setwd("C:\\Users\\Samara\\OneDrive - McGill University\\cali_fund")

# Read in the list of countries eligible for funding, which is in a weird format (numbers and country names are in adjacent columns)
country_list <- read_xls("data_raw\\List of countries eligible for funding_April-2026.xls")
head(country_list)

# Read in the list of CBD parties, which havw counry codes etc.
cbd_parties <- read.csv("data_raw\\cbd_parties.csv") %>%
  rename(country_name = Country) %>%
  mutate(
    country_name = trimws(country_name),
    ISO_A3 = trimws(ISO_A3)
  )
head(cbd_parties)

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
  left_join(cbd_parties %>% select(country_name, ISO_A3), by = "country_name")
head(clean_country_list)

# Save checklist
write.csv(clean_country_list, "data_processed\\clean_country_list.csv", row.names = FALSE)
