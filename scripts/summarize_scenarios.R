library(dplyr)
library(tidyr)

setwd("/Users/samaramanzin/Desktop/cali_fund")

# ============================================================================
# CALI FUND SCENARIOS SUMMARY
# ============================================================================

# Read all scenario files
scenario_files <- list.files("data_processed/cali_fund_scenarios/",
                           pattern = "\\$\\d+M_model_\\d+_approach_\\d+\\.csv$",
                           full.names = TRUE)

# Function to read and add metadata to each scenario
read_scenario <- function(file_path) {
  # Extract metadata from filename
  filename <- basename(file_path)
  parts <- strsplit(filename, "_")[[1]]

  fund_amount <- parts[1]
  model <- paste0("model_", parts[2])
  approach <- paste0("approach_", parts[3])

  # Read data using base R
  data <- read.csv(file_path) %>%
    mutate(
      fund_scenario = fund_amount,
      model = model,
      approach = approach,
      scenario_id = paste(fund_amount, model, approach, sep = "_")
    )

  return(data)
}

# Read all scenarios
all_scenarios <- lapply(scenario_files, read_scenario) %>%
  bind_rows()

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

# Overall summary by scenario
scenario_summary <- all_scenarios %>%
  group_by(scenario_id, fund_scenario, model, approach) %>%
  summarise(
    total_allocated = sum(allocation, na.rm = TRUE),
    mean_allocation = mean(allocation, na.rm = TRUE),
    median_allocation = median(allocation, na.rm = TRUE),
    min_allocation = min(allocation, na.rm = TRUE),
    max_allocation = max(allocation, na.rm = TRUE),
    countries_funded = sum(allocation > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fund_scenario, model, approach)

print("=== SCENARIO SUMMARY ===")
print(scenario_summary)

# ============================================================================
# TOP 10 COUNTRIES BY ALLOCATION (AVERAGE ACROSS ALL SCENARIOS)
# ============================================================================

country_avg_allocation <- all_scenarios %>%
  group_by(ISO_A3, country_name) %>%
  summarise(
    avg_allocation = mean(allocation, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_allocation))

print("\n=== TOP 10 COUNTRIES BY AVERAGE ALLOCATION ===")
print(head(country_avg_allocation, 10))

# ============================================================================
# FUND SIZE COMPARISON
# ============================================================================

fund_comparison <- all_scenarios %>%
  group_by(fund_scenario) %>%
  summarise(
    total_allocated = sum(allocation, na.rm = TRUE),
    mean_per_country = mean(allocation, na.rm = TRUE),
    countries_receiving_funds = sum(allocation > 0, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fund_scenario)

print("\n=== FUND SIZE COMPARISON ===")
print(fund_comparison)

# ============================================================================
# MODEL COMPARISON (AVERAGE ACROSS FUND SIZES)
# ============================================================================

model_comparison <- all_scenarios %>%
  group_by(model) %>%
  summarise(
    avg_allocation = mean(allocation, na.rm = TRUE),
    total_countries = n_distinct(ISO_A3),
    countries_above_median = sum(allocation > median(allocation), na.rm = TRUE),
    .groups = "drop"
  )

print("\n=== MODEL COMPARISON ===")
print(model_comparison)

# ============================================================================
# APPROACH COMPARISON
# ============================================================================

approach_comparison <- all_scenarios %>%
  group_by(approach) %>%
  summarise(
    avg_allocation = mean(allocation, na.rm = TRUE),
    allocation_variance = var(allocation, na.rm = TRUE),
    countries_above_median = sum(allocation > median(allocation), na.rm = TRUE),
    .groups = "drop"
  )

print("\n=== APPROACH COMPARISON ===")
print(approach_comparison)

# ============================================================================
# EXPORT SUMMARY TABLES
# ============================================================================

write.csv(scenario_summary, "data_processed/scenario_summary_detailed.csv", row.names = FALSE)
write.csv(country_avg_allocation, "data_processed/country_avg_allocations.csv", row.names = FALSE)
write.csv(fund_comparison, "data_processed/fund_comparison.csv", row.names = FALSE)

# ============================================================================
# QUICK VISUALIZATIONS (if ggplot2 available)
# ============================================================================

# Fund size comparison plot
if (require(ggplot2, quietly = TRUE)) {
  p1 <- ggplot(fund_comparison, aes(x = fund_scenario, y = mean_per_country)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = "Average Allocation per Country by Fund Size",
         x = "Fund Size", y = "Average Allocation ($)") +
    theme_minimal()

  ggsave("data_processed/fund_size_comparison.png", p1, width = 8, height = 6)

  # Top 10 countries
  p2 <- ggplot(head(country_avg_allocation, 10),
               aes(x = reorder(country_name, avg_allocation), y = avg_allocation)) +
    geom_bar(stat = "identity", fill = "darkgreen") +
    coord_flip() +
    labs(title = "Top 10 Countries by Average Allocation",
         x = "Country", y = "Average Allocation ($)") +
    theme_minimal()

  ggsave("data_processed/top_countries.png", p2, width = 10, height = 6)

  print("\n=== VISUALIZATIONS CREATED ===")
  print("- fund_size_comparison.png")
  print("- top_countries.png")
}

print("\n=== SUMMARY FILES CREATED ===")
print("- scenario_summary_detailed.csv")
print("- country_avg_allocations.csv")
print("- fund_comparison.csv")

# ============================================================================
# KEY INSIGHTS
# ============================================================================

print("\n=== KEY INSIGHTS ===")

# Most generous scenario
most_generous <- scenario_summary %>%
  arrange(desc(mean_allocation)) %>%
  head(1)

print(sprintf("Most generous scenario: %s (avg $%.0f per country)",
              most_generous$scenario_id, most_generous$mean_allocation))

# Most equitable scenario (lowest variance)
most_equitable <- all_scenarios %>%
  group_by(scenario_id) %>%
  summarise(variance = var(allocation, na.rm = TRUE), .groups = "drop") %>%
  arrange(variance) %>%
  head(1)

print(sprintf("Most equitable scenario: %s (lowest allocation variance)", most_equitable$scenario_id))

# Countries that consistently rank high
consistent_top <- all_scenarios %>%
  group_by(ISO_A3, country_name) %>%
  summarise(
    rank_avg = mean(dense_rank(desc(allocation))),
    times_top_10 = sum(dense_rank(desc(allocation)) <= 10),
    .groups = "drop"
  ) %>%
  arrange(rank_avg) %>%
  head(5)

print("\nMost consistently highly ranked countries:")
print(consistent_top %>% select(country_name, rank_avg, times_top_10))