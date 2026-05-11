# generating fake data for gr data. This is just for testing the Cali fund calculation and will be replaced with real data in the future.
library(dplyr)

country_list <- read.csv("data_processed\\clean_country_list.csv")

# generating gr data. All values should b 0
gr_data <- country_list %>%
  mutate(gr = 0)


write.csv(gr_data, "data_processed/gr_data.csv", row.names = FALSE)
