library(dplyr)
library(tidyr)

setwd("C:\\Users\\Samara\\OneDrive - McGill University\\cali_fund")

# Read criterion data
country_list <- read.csv("data_processed/clean_country_list.csv")
CA_data <- read.csv("data_processed/gef_countries.csv")  # Will handle column selection in join
CB_data <- read.csv("data_processed/gr_data.csv")  # Already has ISO_A3 column
CC_data <- read.csv("data_processed/capacity.csv")       # Criterion C: Capacity needs
CD_data <- read.csv("data_processed/iplc_tk_data.csv")   # Criterion D: IPLC/TK measures

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
    CC_data %>% select(iso3, inverse) %>% 
      rename(criterion_C = inverse),
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
      0,  # If no variance, set to 0
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

# Define three weight models
weight_models <- list(
  model_1 = list(
    name = "Equal weights: A=33.3%, C=33.3%, D=33.3%, B=0%",
    A = 1/3, B = 0, C = 1/3, D = 1/3
  ),
  model_2 = list(
    name = "Biodiversity emphasis: A=50%, C=25%, D=25%, B=0%",
    A = 0.50, B = 0, C = 0.25, D = 0.25
  ),
  model_3 = list(
    name = "Balanced with B: A=30%, C=30%, D=30%, B=10%",
    A = 0.30, B = 0.10, C = 0.30, D = 0.30
  )
)

# Fund distribution approaches
fund_approaches <- list(
  approach_1 = "formula",      # 100% formula-based
  approach_2 = "split_50_50"   # 50% equal distribution, 50% formula-based
)

# Fund scenarios
fund_scenarios <- c(10000000, 100000000, 250000000, 1000000000)  # $10M, $100M, $250M, $1B

# Function to calculate fund allocation using formula approach
allocate_fund_formula <- function(country_data, total_fund, weights) {
  n <- nrow(country_data)
  
  # Calculate weighted composite score for each country
  country_data <- country_data %>%
    dplyr::mutate(
      composite_score = (weights$A * criterion_A_scaled +
                        weights$B * criterion_B_scaled +
                        weights$C * criterion_C_scaled +
                        weights$D * criterion_D_scaled),
      composite_score = tidyr::replace_na(composite_score, 0),
      # Normalize scores
      allocation = (composite_score / sum(composite_score, na.rm = TRUE)) * total_fund
    )
  
  return(country_data)
}

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
      composite_score = tidyr::replace_na(composite_score, 0),
      # Formula-based allocation for second half
      formula_allocation = (composite_score / sum(composite_score, na.rm = TRUE)) * formula_portion,
      # Total allocation: equal + formula-based
      allocation = equal_share_per_country + formula_allocation
    )
  
  return(country_data)
}

# Run all scenarios
results <- list()

for (scenario_idx in seq_along(fund_scenarios)) {
  total_fund <- fund_scenarios[scenario_idx]
  fund_label <- paste0("$", total_fund / 1000000, "M")
  
  for (approach_name in names(fund_approaches)) {
    approach_type <- fund_approaches[[approach_name]]
    
    for (model_name in names(weight_models)) {
      model <- weight_models[[model_name]]
      weights <- list(A = model$A, B = model$B, C = model$C, D = model$D)
      
      # Calculate allocation based on approach
      if (approach_type == "formula") {
        result <- allocate_fund_formula(country_data, total_fund, weights)
      } else if (approach_type == "split_50_50") {
        result <- allocate_fund_split(country_data, total_fund, weights)
      }
      
      # Add metadata
      result$scenario_name <- paste0(fund_label, " - ", model_name, " - ", approach_name)
      result$total_fund <- total_fund
      result$model <- model_name
      result$approach <- approach_name
      result$model_description <- model$name
      
      scenario_key <- paste(fund_label, model_name, approach_name, sep = "_")
      results[[scenario_key]] <- result
    }
  }
}

# Save all results to CSV files for review
output_dir <- "data_processed/cali_fund_scenarios"
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

# Summary of scenarios tested
summary_data <- data.frame(
  scenario = character(),
  fund = character(),
  model = character(),
  approach = character(),
  model_description = character(),
  total_allocated = numeric(),
  mean_allocation = numeric(),
  median_allocation = numeric(),
  stringsAsFactors = FALSE
)

for (scenario_name in names(results)) {
  result <- results[[scenario_name]]
  allocations <- result$allocation
  
  summary_data <- rbind(summary_data, data.frame(
    scenario = result$scenario_name,
    fund = paste0("$", result$total_fund / 1000000, "M"),
    model = result$model,
    approach = result$approach,
    model_description = result$model_description,
    total_allocated = sum(allocations, na.rm = TRUE),
    mean_allocation = mean(allocations, na.rm = TRUE),
    median_allocation = median(allocations, na.rm = TRUE)
  ))
}

write.csv(summary_data, file.path(output_dir, "scenario_summary.csv"), row.names = FALSE)
