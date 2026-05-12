# =============================================================================
# Ranked dollar allocation per country
#   (a) one PNG per CSV (24 individual plots)
#   (b) one PNG per budget tier  ($10M, $100M, $250M, $1000M)
# =============================================================================
# Expects CSV files named like:  $10M_model_1_approach_1.csv
#                                ^budget ^model    ^approach
# Each file has columns: ISO_A3, country_name, allocation
# -----------------------------------------------------------------------------

# ---- 1. Packages ------------------------------------------------------------
# install.packages(c("ggplot2", "dplyr", "readr", "tidyr",
#                    "stringr", "forcats", "scales", "tidytext"))

library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(forcats)
library(scales)
library(tidytext)

# ---- 2. Config --------------------------------------------------------------
data_dir <- "/Users/samaramanzin/Desktop/cali_fund/data_processed/cali_fund_scenarios"                       # folder containing the 24 CSVs
out_dir  <- "plots"                   # where PNGs will be written
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Budget tiers, in the order you want them displayed
budget_levels <- c("10M", "100M", "250M", "1000M")

# ---- 3. Discover + read files ----------------------------------------------
files <- list.files(
  path       = data_dir,
  pattern    = "^\\$.*_model_\\d+_approach_\\d+\\.csv$",
  full.names = TRUE
)
stopifnot(length(files) > 0)
message("Found ", length(files), " files.")

parse_scenario <- function(path) {
  base <- basename(path)
  m <- str_match(base, "^\\$([^_]+)_model_(\\d+)_approach_(\\d+)\\.csv$")
  tibble(
    file     = base,
    budget   = m[, 2],
    model    = as.integer(m[, 3]),
    approach = as.integer(m[, 4])
  )
}

all_data <- files |>
  lapply(function(f) {
    read_csv(f, show_col_types = FALSE) |>
      mutate(file = basename(f)) |>
      left_join(parse_scenario(f), by = "file")
  }) |>
  bind_rows() |>
  mutate(
    budget   = factor(budget, levels = budget_levels),
    scenario = sprintf("model %d | approach %d", model, approach)
  )

# Reusable axis formatter
dollar_axis <- scale_x_continuous(
  labels = label_dollar(scale_cut = cut_short_scale())
)

# =============================================================================
# (a) ONE PNG PER FILE  ------------------------------------------------------
# =============================================================================
make_single_plot <- function(df, title) {
  df <- df |>
    arrange(allocation) |>
    mutate(country_name = factor(country_name, levels = country_name))

  ggplot(df, aes(x = allocation, y = country_name, fill = allocation)) +
    geom_col() +
    dollar_axis +
    scale_fill_viridis_c(option = "C", guide = "none") +
    labs(
      title = title,
      x     = "Allocation (USD)",
      y     = NULL
    ) +
    theme_minimal(base_size = 18) +
    theme(
      panel.grid.major.y = element_blank(),
      plot.title = element_text(face = "bold", size = 28),
      axis.title.x = element_text(size = 18),
      axis.text.y = element_text(size = 8)
    )
}

for (f in files) {
  meta <- parse_scenario(f)
  df   <- all_data |>
    filter(file == meta$file)

  ttl  <- sprintf("$%s  |  model %d  |  approach %d",
                  meta$budget, meta$model, meta$approach)

  p <- make_single_plot(df, ttl)

  # Height scales with number of countries so labels stay legible.
  h <- max(6, 0.18 * nrow(df))

  out_file <- file.path(
    out_dir,
    sprintf("allocation_%s_model%d_approach%d.png",
            meta$budget, meta$model, meta$approach)
  )

  ggsave(out_file, p, width = 9, height = h, dpi = 150, limitsize = FALSE)
}
message("Wrote ", length(files), " individual PNGs to ", out_dir, "/")

# =============================================================================
# (b) ONE PNG PER BUDGET TIER  -----------------------------------------------
#     Each PNG facets the scenarios (model x approach) for that budget.
# =============================================================================
for (b in budget_levels) {
  df_b <- all_data |>
    filter(budget == b, model %in% c(1, 2)) |>
    group_by(model, approach) |>
    mutate(country_ranked = reorder(country_name, -allocation)) |>
    ungroup()

  if (nrow(df_b) == 0) {
    message("No files for budget $", b, " -- skipping.")
    next
  }

  n_models    <- length(unique(df_b$model))
  n_approaches <- length(unique(df_b$approach))
  ncol_b      <- max(1, n_models)
  nrow_b      <- max(1, n_approaches)

  p <- ggplot(df_b,
              aes(x = country_ranked, y = allocation, fill = allocation)) +
    geom_col() +
    scale_x_discrete() +
    scale_y_continuous(labels = label_dollar(scale_cut = cut_short_scale())) +
      scale_fill_viridis_c(option = "C", guide = "none") +
    facet_grid(approach ~ model, scales = "free_y", labeller = labeller(
      model = label_both,
      approach = label_both
    )) +
    labs(
      title    = sprintf("Ranked dollar allocation per country  --  $%s budget", b),
      subtitle = "Models 1 & 2",
      x        = "Country",
      y        = "Allocation"
    ) +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.x        = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 8),
      plot.title         = element_text(face = "bold", size = 32),
      strip.text         = element_text(size = 18),
      plot.subtitle      = element_text(color = "grey40", size = 16)
    )

  # Height scales with panel rows AND countries per panel.
  countries_per_panel <- df_b |>
    count(scenario) |>
    pull(n) |>
    max()
  h <- max(8, 0.035 * countries_per_panel * nrow_b)
  w <- max(30, 11 * ncol_b)

  out_file <- file.path(out_dir, sprintf("budget_%s_grouped.png", b))
  ggsave(out_file, p, width = w, height = h, dpi = 150, limitsize = FALSE)
  message("Wrote ", out_file)
}
message("Done.")
