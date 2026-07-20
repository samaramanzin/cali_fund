# Calculate $200M allocations for full-formula and hybrid developed-baseline multiplier scenarios

library(dplyr)
library(tidyr)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# Config
total_fund <- 200000000
fund_label <- "$200M"
output_dir <- "data_processed/outputs/iplc_scenarios"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Read country lists
overlay_path <- if (file.exists("data_processed/country_overlay.csv")) "data_processed/country_overlay.csv" else if (file.exists("data_raw/country_overlay.csv")) "data_raw/country_overlay.csv" else NULL
if (is.null(overlay_path)) stop("country_overlay.csv not found in data_processed/ or data_raw/")
overlay <- read.csv(overlay_path, stringsAsFactors = FALSE)
clean_non_dev <- read.csv("data_processed/clean_data/clean_country_list.csv", stringsAsFactors = FALSE)

# Map overlay columns: iso3c -> ISO_A3, cbd_party_name -> country_name
if ("iso3c" %in% names(overlay)) names(overlay)[names(overlay)=="iso3c"] <- "ISO_A3"
if ("cbd_party_name" %in% names(overlay)) names(overlay)[names(overlay)=="cbd_party_name"] <- "country_name"
if (!"ISO_A3" %in% names(overlay)) stop("country_overlay.csv must contain an 'iso3c' or 'ISO_A3' column")
if (!"country_name" %in% names(overlay)) overlay$country_name <- overlay$ISO_A3

overlay <- overlay %>% mutate(ISO_A3 = trimws(as.character(ISO_A3)), country_name = trimws(as.character(country_name)))

# Mark development: clean_non_dev lists non-developed countries (ISO_A3)
if (!"ISO_A3" %in% names(clean_non_dev)) {
  if ("iso3c" %in% names(clean_non_dev)) names(clean_non_dev)[names(clean_non_dev)=="iso3c"] <- "ISO_A3"
}
clean_non_dev <- clean_non_dev %>% mutate(ISO_A3 = trimws(as.character(ISO_A3)))

country_list <- overlay %>% distinct(ISO_A3, country_name) %>%
  mutate(is_developing = ISO_A3 %in% clean_non_dev$ISO_A3, is_developed = !is_developing)

# Read criteria data
# Read criteria from clean_data folder
# Read criterion data
CA_data <- read.csv("data_processed/clean_data/gef_countries.csv")  # Criterion A: GEF GBI Biodiversity
CB_data <- read.csv("data_processed/clean_data/gr_data.csv")        # Criterion B placeholder
CC_gni_data <- read.csv("data_processed/clean_data/gni_data_2.csv")   # Criterion C for Model 2
CD_data <- read.csv("data_processed/clean_data/cbd_party_language_count.csv")   # Criterion D: linguistic diversity/TK

# function to scale values between 0 and 1, handling cases where all values are the same or NA
scale_0_1 <- function(x) {
  x <- as.numeric(x)
  range <- max(x, na.rm = TRUE) - min(x, na.rm = TRUE)

  if (is.na(range) || range == 0) {
    return(rep(0, length(x)))
  }

  (x - min(x, na.rm = TRUE)) / range
}

# Combine all criterion data (inline, matching structure in `cali_fund_calculation.R`)
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
    CC_gni_data %>% select(iso3, inverse) %>%
      rename(criterion_C = inverse),
    by = c("ISO_A3" = "iso3")
  ) %>%
  dplyr::left_join(
    CD_data %>% select(iso_a3, language_count) %>%
      rename(criterion_D = language_count),
    by = c("ISO_A3" = "iso_a3")
  ) %>%
  dplyr::mutate(
    criterion_A = as.numeric(criterion_A),
    criterion_B = 0,
    criterion_C = as.numeric(criterion_C),
    criterion_D = as.numeric(criterion_D)
  ) %>%
  dplyr::mutate(
    # scale each criterion to 0-1 safely
    criterion_A_scaled = scale_0_1(criterion_A),
    criterion_B_scaled = 0,
    criterion_C_scaled = scale_0_1(criterion_C),
    criterion_D_scaled = scale_0_1(criterion_D)
  ) %>%
  tidyr::replace_na(
    list(criterion_A_scaled = 0, criterion_B_scaled = 0,
         criterion_C_scaled = 0, criterion_D_scaled = 0)
  )

# weights
weights <- list(A = 0.25, B = 0.25, C = 0.25, D = 0.25)

add_formula_allocation <- function(country_data, formula_portion, weights) {
  country_data <- country_data %>%
    mutate(
      composite_score = (weights$A * criterion_A_scaled + weights$B * criterion_B_scaled +
                         weights$C * criterion_C_scaled + weights$D * criterion_D_scaled),
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

allocate_formula <- function(country_data, total_fund, weights) {
  cd <- add_formula_allocation(country_data, total_fund, weights)
  cd <- cd %>% mutate(allocation = formula_allocation)
  return(cd)
}

allocate_hybrid <- function(country_data, total_fund, weights, developed_baseline_multiplier = 1.0, baseline_share = 0.5) {
  base_portion <- total_fund * baseline_share
  cd <- country_data %>% mutate(baseline_multiplier = ifelse(is_developed, developed_baseline_multiplier, 1.0))
  sum_m <- sum(cd$baseline_multiplier, na.rm = TRUE)
  if (sum_m == 0) stop("Sum of baseline multipliers is zero")
  cd <- cd %>% mutate(equalizer_allocation = base_portion * (baseline_multiplier / sum_m))
  formula_portion <- total_fund - sum(cd$equalizer_allocation, na.rm = TRUE)
  cd <- add_formula_allocation(cd, formula_portion, weights)
  cd <- cd %>% mutate(allocation = equalizer_allocation + formula_allocation)
  return(cd)
}

# country_data already built inline above (using GNI as criterion C)
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

allocate_model_2 <- function(country_data, total_fund, weights, developed_baseline_multiplier = 1.0) {
  n <- nrow(country_data)
  base_portion <- total_fund / 2
  base_share_per_country <- base_portion / n

  country_data <- country_data %>%
    dplyr::mutate(
      equalizer_multiplier = ifelse(is_developed, developed_baseline_multiplier, 1),
      equalizer_allocation = base_share_per_country * equalizer_multiplier
    )

  formula_portion <- total_fund - sum(country_data$equalizer_allocation, na.rm = TRUE)

  country_data %>%
    add_formula_allocation(formula_portion, weights) %>%
    dplyr::mutate(allocation = equalizer_allocation + formula_allocation)
}

# Run models and write outputs (model_1 + model_2 with dev multipliers)
dev_multipliers <- c(1.0, 0.75, 0.5)

results <- list(
  model_1 = allocate_model_1(country_data, total_fund, weights) %>%
    dplyr::mutate(
      scenario_name = paste0(fund_label, " - model_1 - approach_2"),
      total_fund = total_fund,
      model = "model_1",
      approach = "approach_2",
      model_description = "Model 1: 50% equalizer; equal weights A=25%,B=25%,C=25%,D=25; GNI used for criterion C"
    )
)

for (m in dev_multipliers) {
  key <- paste0("model_2_dev", gsub("\\.", "_", as.character(m)))
  results[[key]] <- allocate_model_2(country_data, total_fund, weights, developed_baseline_multiplier = m) %>%
    dplyr::mutate(
      scenario_name = paste0(fund_label, " - model_2 - dev_mult_", m, " - approach_2"),
      total_fund = total_fund,
      model = "model_2",
      approach = "approach_2",
      model_description = paste0("Model 2: developed baseline multiplier = ", m, "; equal weights A=25%,B=25%,C=25%,D=25; GNI used for criterion C")
    )
}

# Ensure output dir exists
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

for (nm in names(results)) {
  df <- results[[nm]] %>%
    dplyr::select(ISO_A3, country_name, is_developed, equalizer_multiplier, equalizer_allocation, formula_allocation, allocation)
  fname <- file.path(output_dir, paste0(fund_label, "_", nm, "_approach_2.csv"))
  write.csv(df, fname, row.names = FALSE)
  message("Wrote: ", fname)
}

message("All model outputs written to ", output_dir)
