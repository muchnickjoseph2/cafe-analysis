# ============================================================
# 02_descriptive.R
# Reads analysis/dataset.csv and describes the variables used in A1-A10.
# Decides parametric vs non-parametric for each variable and group.
# Run from the project root (open cafe-analysis.Rproj first).
# ============================================================

library(tidyverse)

if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork")
library(patchwork)

# ---- 1. Load the cleaned data ----
dat <- read_csv("analysis/dataset.csv", show_col_types = FALSE) %>%
  mutate(
    cafe_type = factor(cafe_type,
                       levels = c("Independent", "Local Chain", "International Chain")),
    # first level = first group in R's tests: "Above median minus Below median"
    n_group_median = factor(n_group_median, levels = c("Above median", "Below median")),
    n_group_3 = factor(n_group_3,
                       levels = c("Few (<100)", "Medium (100-1,000)", "Many (>1,000)")),
    n_group_2 = factor(n_group_2, levels = c("Under 100", "100 or more")),
    diff_adj  = adj_rating - rating          # paired difference for A3
  )

glimpse(dat)

# ---- 2. Count and summarise: numeric variables, all cafes ----
num_summary <- function(x) {
  tibble(n         = sum(!is.na(x)),
         n_missing = sum(is.na(x)),
         mean      = mean(x, na.rm = TRUE),
         sd        = sd(x, na.rm = TRUE),
         median    = median(x, na.rm = TRUE),
         iqr       = IQR(x, na.rm = TRUE),
         min       = min(x, na.rm = TRUE),
         max       = max(x, na.rm = TRUE))
}

overall <- bind_rows(
  rating     = num_summary(dat$rating),
  adj_rating = num_summary(dat$adj_rating),
  n_ratings  = num_summary(dat$n_ratings),
  diff_adj   = num_summary(dat$diff_adj),
  .id = "variable"
)
print(overall, width = Inf)

# ---- 3. Numeric variables by group ----
by_group <- function(data, value, group) {
  data %>%
    filter(!is.na({{ group }}), !is.na({{ value }})) %>%
    group_by(group = {{ group }}) %>%
    summarise(n = n(), mean = mean({{ value }}), sd = sd({{ value }}),
              median = median({{ value }}), iqr = IQR({{ value }}),
              min = min({{ value }}), max = max({{ value }}),
              .groups = "drop")
}

print(by_group(dat, rating, n_group_median), width = Inf)   # A2
print(by_group(dat, rating, n_group_3),      width = Inf)   # A4
print(by_group(dat, rating, cafe_type),      width = Inf)   # A8
print(by_group(dat, rating, n_group_2),      width = Inf)   # A9
print(by_group(dat, adj_rating, n_group_2),  width = Inf)   # A9

# ---- 4. Categorical variables: counts and percentages ----
dat %>% count(top_rated) %>% mutate(pct = round(100 * n / sum(n), 1)) %>% print()
dat %>% count(n_group_median, top_rated) %>% print()          # A5

# ---- 5. Descriptive plots: histogram, boxplot and normal Q-Q plot ----
dir.create("report/figures", showWarnings = FALSE, recursive = TRUE)

desc_plots <- function(data, value, group = NULL, label, file, binwidth = NULL) {
  d <- data %>% filter(!is.na(.data[[value]]))
  if (!is.null(group)) {
    d <- d %>% filter(!is.na(.data[[group]]))
    d$g <- d[[group]]
  } else {
    d$g <- factor("All cafes")
  }
  
  p_hist <- ggplot(d, aes(x = .data[[value]])) +
    geom_histogram(binwidth = binwidth, bins = 20,
                   fill = "grey70", colour = "white") +
    facet_wrap(~ g, nrow = 1, scales = "free_y") +
    labs(x = label, y = "Number of cafes")
  
  p_box <- ggplot(d, aes(x = g, y = .data[[value]])) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(width = 0.15, alpha = 0.3, size = 0.8) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    labs(x = NULL, y = label)
  
  p_qq <- ggplot(d, aes(sample = .data[[value]])) +
    stat_qq() + stat_qq_line() +
    facet_wrap(~ g, nrow = 1) +
    labs(x = "Theoretical normal quantile", y = paste("Sample:", label))
  
  fig <- (p_hist / p_box / p_qq) + plot_annotation(title = label)
  ggsave(file.path("report/figures", file), fig, width = 9, height = 9)
}

# All cafes
desc_plots(dat, "rating",     NULL, "Google rating (points)",        "D_rating.pdf", binwidth = 0.1)
desc_plots(dat, "adj_rating", NULL, "Adjusted rating (points)",      "D_adj_rating.pdf")
desc_plots(dat, "n_ratings",  NULL, "Number of ratings",             "D_n_ratings.pdf")
desc_plots(dat, "diff_adj",   NULL, "Adjusted minus raw rating (points)", "D_diff_adj.pdf")

# By group
desc_plots(dat, "rating", "n_group_median", "Google rating (points)", "D_rating_by_median.pdf", binwidth = 0.1)  # A2
desc_plots(dat, "rating", "n_group_3",      "Google rating (points)", "D_rating_by_n3.pdf",     binwidth = 0.1)  # A4
desc_plots(dat, "rating", "cafe_type",      "Google rating (points)", "D_rating_by_type.pdf",   binwidth = 0.1)  # A8
desc_plots(dat, "rating", "n_group_2",      "Google rating (points)", "D_rating_by_n2.pdf",     binwidth = 0.1)  # A9
desc_plots(dat, "adj_rating", "n_group_2",  "Adjusted rating (points)", "D_adj_rating_by_n2.pdf")                # A9

