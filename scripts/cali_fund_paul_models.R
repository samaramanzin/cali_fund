library(dplyr)
library(tidyr)
library(countrycode)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Read criterion data
country_list <- read.csv("data_processed/clean_country_list.csv")
CA_data <- read.csv("data_processed/gef_countries.csv")
CB_data <- read.csv("data_processed/gr_data.csv")
CC_data <- read.csv("data_processed/gini-minimum-country-annex.csv")
CD_data <- read.csv("data_processed/iplc_tk_data.csv")

# add iso-codes to gini data for easier merging
CC_data <- CC_data %>%
    mutate(iso3 = countrycode(party, origin = "country.name", destination = "iso3c"))

# Combine all criterion data
country_data <- country_list %>%
  dplyr::left_join(
    CA_data %>% select(1, gef_allocation) %>%
      rename(iso3 = 1, criterion_A = gef_allocation),
    by = c("ISO_A3" = "iso3")
  ) %>%
  dplyr::left_join(
    CB_data %>% select(ISO_A3, gr) %>% 
      rename(criterion_B = gr),
    by = "ISO_A3"
  ) %>%
  dplyr::left_join(
    CC_data %>% select(iso3, total_allocation) %>% 
      rename(criterion_C = total_allocation),
    by = c("ISO_A3" = "iso3")
  ) %>%
  dplyr::left_join(
    CD_data %>% select(iso3, iplc_tk) %>% 
      rename(criterion_D = iplc_tk),
    by = c("ISO_A3" = "iso3")
  ) %>%
  dplyr::mutate(
    criterion_A = as.numeric(criterion_A),
    criterion_B = as.numeric(criterion_B),
    criterion_C = as.numeric(criterion_C),
    criterion_D = as.numeric(criterion_D)
  ) %>%
  dplyr::mutate(
    # Scale each criterion to 0-1 range
    criterion_A_scaled = (criterion_A - min(criterion_A, na.rm = TRUE)) / 
                         (max(criterion_A, na.rm = TRUE) - min(criterion_A, na.rm = TRUE)),
    criterion_B_scaled = ifelse(
      max(criterion_B, na.rm = TRUE) - min(criterion_B, na.rm = TRUE) == 0,
      0,
      (criterion_B - min(criterion_B, na.rm = TRUE)) / 
      (max(criterion_B, na.rm = TRUE) - min(criterion_B, na.rm = TRUE))
    ),
    criterion_C_scaled = (criterion_C - min(criterion_C, na.rm = TRUE)) / 
                         (max(criterion_C, na.rm = TRUE) - min(criterion_C, na.rm = TRUE)),
    criterion_D_scaled = (criterion_D - min(criterion_D, na.rm = TRUE)) / 
                         (max(criterion_D, na.rm = TRUE) - min(criterion_D, na.rm = TRUE))
  ) %>%
  tidyr::replace_na(
    list(criterion_A_scaled = 0, criterion_B_scaled = 0, 
         criterion_C_scaled = 0, criterion_D_scaled = 0)
  )

# Define weight models
weight_models <- list(
  paul_model_1 = list(
    name = "Paul model 1: A=30%, C=30%, D=30%, B=10%",
    A = 0.3, B = 0.1, C = 0.3, D = 0.3
  ),
  paul_model_2 = list(
    name = "Paul model 2: A=50%, C=0%, D=25%, B=25%",
    A = 0.50, B = 0.25, C = 0, D = 0.25
  )
)

# Function to calculate fund allocation using 50-50 split approach
allocate_fund_split <- function(country_data, total_fund, weights) {
  n <- nrow(country_data)
  
  # Split fund 50-50
  equal_portion <- total_fund / 2
  formula_portion <- total_fund / 2
  
  equal_share_per_country <- equal_portion / n
  
  # Calculate weighted composite score for formula portion
  country_data <- country_data %>%
    dplyr::mutate(
      composite_score = (weights$A * criterion_A_scaled +
                        weights$B * criterion_B_scaled +
                        weights$C * criterion_C_scaled +
                        weights$D * criterion_D_scaled),
      composite_score = tidyr::replace_na(composite_score, 0), # currently how I'm dealing with missing data
      # Formula-based allocation for second half
      formula_allocation = (composite_score / sum(composite_score, na.rm = TRUE)) * formula_portion,
      # Total allocation: equal + formula-based
      allocation = equal_share_per_country + formula_allocation
    )
  
  return(country_data)
}

# Run scenarios: ONLY APPROACH 2 and $200M fund
total_fund <- 200000000
fund_label <- "$200M"
results <- list()

for (model_name in names(weight_models)) {
  model <- weight_models[[model_name]]
  weights <- list(A = model$A, B = model$B, C = model$C, D = model$D)
  
  # Calculate allocation using approach 2 (50-50 split)
  result <- allocate_fund_split(country_data, total_fund, weights)
  
  # Add metadata
  result$scenario_name <- paste0(fund_label, " - ", model_name, " - approach_2")
  result$total_fund <- total_fund
  result$model <- model_name
  result$approach <- "approach_2"
  result$model_description <- model$name
  
  scenario_key <- paste(fund_label, model_name, "approach_2", sep = "_")
  results[[scenario_key]] <- result
}

# Save all results to CSV files
output_dir <- "data_processed/cali_fund_scenarios_200M_approach2_paul_models"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (scenario_name in names(results)) {
  result_data <- results[[scenario_name]] %>%
    select(ISO_A3, country_name, allocation) %>%
    mutate(allocation = round(allocation, 2))
  
  filename <- file.path(output_dir, paste0(scenario_name, ".csv"))
  write.csv(result_data, filename, row.names = FALSE)
}

