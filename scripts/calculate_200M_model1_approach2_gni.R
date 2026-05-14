library(dplyr)
library(tidyr)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Read criterion data
country_list <- read.csv("data_processed/clean_country_list.csv")
CA_data <- read.csv("data_processed/gef_countries.csv")  # Will handle column selection in join
CB_data <- read.csv("data_processed/gr_data.csv")  # Already has ISO_A3 column
CC_data <- read.csv("data_processed/gni_data.csv")       # Criterion C: Using GNI data for capacity needs
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

# Define model 1 weights
model <- list(
  name = "Equal weights: A=33.3%, C=33.3%, D=33.3%, B=0%",
  A = 1/3, B = 0, C = 1/3, D = 1/3
)

# Fund amount: $200M
total_fund <- 200000000
fund_label <- "$200M"

# Approach 2: 50-50 split
approach_name <- "approach_2"
approach_type <- "split_50_50"

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

# Calculate allocation
weights <- list(A = model$A, B = model$B, C = model$C, D = model$D)
result <- allocate_fund_split(country_data, total_fund, weights)

# Add metadata
result$scenario_name <- paste0(fund_label, " - model_1 - ", approach_name, " - GNI")
result$total_fund <- total_fund
result$model <- "model_1"
result$approach <- approach_name
result$model_description <- model$name

# Save result to CSV file
output_dir <- "data_processed/cali_fund_scenarios_gni"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

result_data <- result %>%
  select(ISO_A3, country_name, allocation) %>%
  mutate(allocation = round(allocation, 2))

filename <- file.path(output_dir, paste0(fund_label, "_model_1_", approach_name, "_GNI.csv"))
write.csv(result_data, filename, row.names = FALSE)

print(paste("Calculation complete. Results saved to:", filename))