# This script generates placeholder data for GR (Citerion B) and can be updated with real data when available.

library(dplyr)

country_list <- read.csv("data_processed/clean_data/clean_country_list.csv")

# generating gr data. All values should b 0
gr_data <- country_list %>%
  mutate(gr = 0)


write.csv(gr_data, "data_processed/clean_data/gr_data.csv", row.names = FALSE)
