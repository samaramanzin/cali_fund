# This script calculates fund allocations for the $200M fund using two model variants.
# Model 1 splits 50% of the fund equally across eligible countries and allocates
# the remaining 50% using equal weights across four criteria.
# Model 2 applies a 25% uplift to the base allocation for LDCs and SIDS, then
# allocates the remaining funds using equal weights across four criteria.
# Criterion B is a placeholder for country of origin of DSI in databases and is set to 0.
# outputs are saved in data_processed/outputs/cali_fund_scenarios_model1_model2

library(dplyr)
library(tidyr)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Read criterion data
country_list <- read.csv("data_processed/clean_data/clean_country_list_with_categories.csv")
CA_data <- read.csv("data_processed/clean_data/gef_countries.csv")  # Criterion A: GEF GBI Biodiversity
CB_data <- read.csv("data_processed/clean_data/gr_data.csv")        # Criterion B placeholder
CC_band_data <- read.csv("data_raw/country_allocations_full.csv") %>% # Criterion C for Model 1
  select(iso3c, party, band_weight)
CC_gni_data <- read.csv("data_processed/clean_data/gni_data_2.csv")   # Criterion C for Model 2
CD_data <- read.csv("data_processed/clean_data/iplc_tk_data.csv")   # Criterion D: linguistic diversity/TK

# function to scale values between 0 and 1, handling cases where all values are the same or NA
scale_0_1 <- function(x) {
  x <- as.numeric(x)
  range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

  if (is.na(range) || range == 0) {
    return(rep(0, length(x)))
  }

  (x - min(x, na.rm = TRUE)) / range
}

# Combine all criterion data
build_country_data <- function(country_list, capacity_data, capacity_column, capacity_iso_column) {
  country_list %>%
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
      capacity_data %>% select(all_of(c(capacity_iso_column, capacity_column))) %>%
        rename(iso3 = all_of(capacity_iso_column), criterion_C = all_of(capacity_column)),
      by = c("ISO_A3" = "iso3")
    ) %>%
    dplyr::left_join(
      CD_data %>% select(iso3, iplc_tk) %>%
        rename(criterion_D = iplc_tk),
      by = c("ISO_A3" = "iso3")
    ) %>%
    dplyr::mutate(
      criterion_A = as.numeric(criterion_A),
      criterion_B = 0,
      criterion_C = as.numeric(criterion_C),
      criterion_D = as.numeric(criterion_D)
    ) %>%
    dplyr::mutate(
      criterion_A_scaled = scale_0_1(criterion_A),
      criterion_B_scaled = 0,
      criterion_C_scaled = scale_0_1(criterion_C),
      criterion_D_scaled = scale_0_1(criterion_D)
    ) %>%
    tidyr::replace_na(
      list(criterion_A_scaled = 0, criterion_B_scaled = 0,
           criterion_C_scaled = 0, criterion_D_scaled = 0)
    )
}

# Define equal criterion weights across all four criteria
model_weights <- list(A = 0.25, B = 0.25, C = 0.25, D = 0.25)

# Fund amount: $200M
total_fund <- 200000000
fund_label <- "$200M"
approach_name <- "approach_2"
approach_type <- "split_equalizer_formula"

add_formula_allocation <- function(country_data, formula_portion, weights) {
  country_data <- country_data %>%
    dplyr::mutate(
      composite_score = (weights$A * criterion_A_scaled +
                         weights$B * criterion_B_scaled +
                         weights$C * criterion_C_scaled +
                         weights$D * criterion_D_scaled),
      composite_score = tidyr::replace_na(composite_score, 0)
    )

  total_score <- sum(country_data$composite_score, na.rm = TRUE)

  if (total_score == 0) {
    country_data$formula_allocation <- 0
  } else {
    country_data$formula_allocation <- (country_data$composite_score / total_score) * formula_portion
  }

  return(country_data)
}

# Model 1: 50% equal allocation and 50% formula allocation
allocate_model_1 <- function(country_data, total_fund, weights) {
  n <- nrow(country_data)
  equal_portion <- total_fund / 2
  formula_portion <- total_fund / 2
  equal_share_per_country <- equal_portion / n

  country_data %>%
    add_formula_allocation(formula_portion, weights) %>%
    dplyr::mutate(
      equalizer_multiplier = 1,
      equalizer_allocation = equal_share_per_country,
      allocation = equalizer_allocation + formula_allocation
    )
}

# Model 2: 50% base allocation, with LDCs and SIDS receiving 25% more than the base share.
# The formula allocation receives whatever remains after the adjusted base allocation.
allocate_model_2 <- function(country_data, total_fund, weights) {
  n <- nrow(country_data)
  base_portion <- total_fund / 2
  base_share_per_country <- base_portion / n

  country_data <- country_data %>%
    dplyr::mutate(
      equalizer_multiplier = ifelse(is_ldc | is_sids, 1.25, 1),
      equalizer_allocation = base_share_per_country * equalizer_multiplier
    )

  formula_portion <- total_fund - sum(country_data$equalizer_allocation, na.rm = TRUE)

  country_data %>%
    add_formula_allocation(formula_portion, weights) %>%
    dplyr::mutate(allocation = equalizer_allocation + formula_allocation)
}

model_1_data <- build_country_data(
  country_list = country_list,
  capacity_data = CC_band_data,
  capacity_column = "band_weight",
  capacity_iso_column = "iso3c"
)

model_2_data <- build_country_data(
  country_list = country_list,
  capacity_data = CC_gni_data,
  capacity_column = "inverse",
  capacity_iso_column = "iso3"
)

results <- list(
  model_1 = allocate_model_1(model_1_data, total_fund, model_weights) %>%
    dplyr::mutate(
      scenario_name = paste0(fund_label, " - model_1 - ", approach_name, " - band_language"),
      total_fund = total_fund,
      model = "model_1",
      approach = approach_name,
      approach_type = approach_type,
      model_description = "Model 1: 50% equalizer; equal weights A=25%, B=25%, C=25%, D=25%; capacity uses Paul UN Scale band weights"
    ),
  model_2 = allocate_model_2(model_2_data, total_fund, model_weights) %>%
    dplyr::mutate(
      scenario_name = paste0(fund_label, " - model_2 - ", approach_name, " - adjusted_equalizer_gni_language"),
      total_fund = total_fund,
      model = "model_2",
      approach = approach_name,
      approach_type = approach_type,
      model_description = "Model 2: adjusted equalizer with 25% LDC/SIDS uplift; equal weights A=25%, B=25%, C=25%, D=25%; capacity uses inverted GNI per capita"
    )
)

# Save results to CSV files
output_dir <- "data_processed/outputs/cali_fund_scenarios_model1_model2"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (model_name in names(results)) {
  result_data <- results[[model_name]] %>%
    select(ISO_A3, country_name, country_category, is_ldc, is_sids,
           equalizer_multiplier, equalizer_allocation, formula_allocation, allocation) %>%
    mutate(
      equalizer_allocation = round(equalizer_allocation, 2),
      formula_allocation = round(formula_allocation, 2),
      allocation = round(allocation, 2)
    )

  filename <- file.path(output_dir, paste0(fund_label, "_", model_name, "_", approach_name, ".csv"))
  write.csv(result_data, filename, row.names = FALSE)

  print(paste("Calculation complete. Results saved to:", filename))
  print(paste("Rounded allocation total:", round(sum(result_data$allocation), 2)))
}
