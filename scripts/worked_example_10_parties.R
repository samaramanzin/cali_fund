library(dplyr)
library(tidyr)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# =============================================================================
# WORKED EXAMPLE: Fund allocation calculation for 10 sample parties
# Shows step-by-step process from input data to final allocations
# =============================================================================

# Read full criterion data
CA_data <- read.csv("data_processed/gef_countries.csv")
CB_data <- read.csv("data_processed/gr_data.csv")
CC_data <- read.csv("data_processed/capacity.csv")
CD_data <- read.csv("data_processed/iplc_tk_data.csv")

# Select 10 sample countries for demonstration
sample_iso3 <- c("BRA", "IND", "IDN", "COD", "ZAF", "MEX", "NGA", "PER", "PNG", "VNM")

# Build sample dataset
sample_countries <- tibble(
  ISO_A3 = sample_iso3,
  country_name = c("Brazil", "India", "Indonesia", "Democratic Republic of Congo",
                   "South Africa", "Mexico", "Nigeria", "Peru", "Papua New Guinea", "Vietnam")
)

# Combine with criterion data
sample_data <- sample_countries %>%
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
  )

# Display raw input data
message("\n=== STEP 1: RAW INPUT DATA ===")
print(sample_data %>% select(ISO_A3, country_name, criterion_A, criterion_B, criterion_C, criterion_D))

# =============================================================================
# STEP 2: NORMALIZE CRITERIA TO 0-1 SCALE
# =============================================================================
sample_data <- sample_data %>%
  dplyr::mutate(
    # Scale each criterion to 0-1 range using min-max normalization
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

message("\n=== STEP 2: NORMALIZED CRITERIA (0-1 Scale) ===")
print(sample_data %>% select(ISO_A3, country_name, criterion_A_scaled, criterion_B_scaled, 
                             criterion_C_scaled, criterion_D_scaled) %>%
        mutate(across(c(criterion_A_scaled:criterion_D_scaled), ~round(., 4))))

# =============================================================================
# STEP 3: APPLY WEIGHTING MODEL (MODEL 1: Equal Weights)
# =============================================================================
weights_model1 <- list(A = 1/3, B = 0, C = 1/3, D = 1/3)

sample_data <- sample_data %>%
  dplyr::mutate(
    # Calculate weighted composite score
    composite_score = (weights_model1$A * criterion_A_scaled +
                      weights_model1$B * criterion_B_scaled +
                      weights_model1$C * criterion_C_scaled +
                      weights_model1$D * criterion_D_scaled),
    composite_score = tidyr::replace_na(composite_score, 0)
  )

message("\n=== STEP 3: COMPOSITE SCORES (Model 1 - Equal Weights: A=33.3%, C=33.3%, D=33.3%) ===")
print(sample_data %>% select(ISO_A3, country_name, composite_score) %>%
        mutate(composite_score = round(composite_score, 4)))

# =============================================================================
# STEP 4: NORMALIZE SCORES & ALLOCATE FUNDS
# Total Fund: $50 Million (for easy demonstration)
# =============================================================================
total_fund <- 50000000

sample_data <- sample_data %>%
  dplyr::mutate(
    # Calculate proportional allocation
    allocation = (composite_score / sum(composite_score, na.rm = TRUE)) * total_fund,
    proportion = composite_score / sum(composite_score, na.rm = TRUE)
  )

message("\n=== STEP 4: FUND ALLOCATION ($50M Total) ===")
allocation_table <- sample_data %>% 
  select(ISO_A3, country_name, composite_score, proportion, allocation) %>%
  mutate(
    composite_score = round(composite_score, 4),
    proportion = round(proportion, 4),
    allocation = round(allocation, 2)
  ) %>%
  arrange(desc(allocation))

print(allocation_table)

# Verify total
total_allocated <- sum(allocation_table$allocation)
message(sprintf("\nTotal Allocated: $%.2f", total_allocated))
message(sprintf("Variance from $50M: $%.2f", total_fund - total_allocated))

# =============================================================================
# STEP 5: COMPARISON WITH MODEL 2 (Biodiversity Emphasis)
# =============================================================================
weights_model2 <- list(A = 0.50, B = 0, C = 0.25, D = 0.25)

sample_data_model2 <- sample_data %>%
  dplyr::mutate(
    composite_score_m2 = (weights_model2$A * criterion_A_scaled +
                         weights_model2$B * criterion_B_scaled +
                         weights_model2$C * criterion_C_scaled +
                         weights_model2$D * criterion_D_scaled),
    composite_score_m2 = tidyr::replace_na(composite_score_m2, 0),
    allocation_m2 = (composite_score_m2 / sum(composite_score_m2, na.rm = TRUE)) * total_fund
  )

message("\n=== STEP 5: COMPARISON - MODEL 2 (Biodiversity Emphasis: A=50%, C=25%, D=25%) ===")
comparison_table <- sample_data_model2 %>%
  select(ISO_A3, country_name, allocation, allocation_m2) %>%
  mutate(
    allocation = round(allocation, 2),
    allocation_m2 = round(allocation_m2, 2),
    difference = round(allocation_m2 - allocation, 2)
  ) %>%
  arrange(desc(allocation_m2))

print(comparison_table)


