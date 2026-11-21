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

# ---- 6. Normality numbers and the central limit theorem ----
group_levels <- c("All cafes", "Above median", "Below median",
                  "Few (<100)", "Medium (100-1,000)", "Many (>1,000)",
                  "Independent", "Local Chain", "International Chain",
                  "Under 100", "100 or more")

long <- bind_rows(
  dat %>% transmute(set = "Rating",                            group = "All cafes",                  value = rating),
  dat %>% transmute(set = "Adjusted rating",                   group = "All cafes",                  value = adj_rating),
  dat %>% transmute(set = "Number of ratings",                 group = "All cafes",                  value = n_ratings),
  dat %>% transmute(set = "Adjusted minus raw (A3)",           group = "All cafes",                  value = diff_adj),
  dat %>% transmute(set = "Rating by median split (A2)",       group = as.character(n_group_median), value = rating),
  dat %>% transmute(set = "Rating by ratings group (A4)",      group = as.character(n_group_3),      value = rating),
  dat %>% transmute(set = "Rating by cafe type (A8)",          group = as.character(cafe_type),      value = rating),
  dat %>% transmute(set = "Rating by 100 split (A9)",          group = as.character(n_group_2),      value = rating),
  dat %>% transmute(set = "Adjusted rating by 100 split (A9)", group = as.character(n_group_2),      value = adj_rating)
) %>%
  filter(!is.na(group), !is.na(value)) %>%
  mutate(set   = factor(set, levels = unique(set)),
         group = factor(group, levels = group_levels))

# Skewness: 0 = symmetric, negative = left tail, positive = right tail
calc_skew <- function(x) {
  x <- x[!is.na(x)]
  m <- mean(x)
  mean((x - m)^3) / mean((x - m)^2)^1.5
}

# Bootstrap of the mean: 5,000 resamples, seed fixed so results are reproducible
set.seed(2026)
B <- 5000
boot_means <- function(x, B) replicate(B, mean(sample(x, length(x), replace = TRUE)))

res <- long %>%
  group_by(set, group) %>%
  summarise(n         = n(),
            skew      = calc_skew(value),
            shapiro_p = shapiro.test(value)$p.value,   # optional; does not override the plots
            boot      = list(boot_means(value, B)),
            .groups   = "drop") %>%
  mutate(skew_boot = map_dbl(boot, calc_skew),         # skewness of the bootstrapped means
         panel     = factor(paste0(set, ": ", group), levels = paste0(set, ": ", group)))

res %>%
  select(panel, n, skew, skew_boot, shapiro_p) %>%
  mutate(across(c(skew, skew_boot), ~ round(.x, 2)),
         shapiro_p = round(shapiro_p, 3)) %>%
  print(n = Inf, width = Inf)

# ---- 7. Distribution of the bootstrapped means ----
boot_long <- res %>% select(panel, boot) %>% unnest(boot)

p_bh <- ggplot(boot_long, aes(x = boot)) +
  geom_histogram(bins = 40, fill = "grey70", colour = "white") +
  facet_wrap(~ panel, scales = "free", ncol = 4, labeller = label_wrap_gen(28)) +
  labs(x = "Bootstrapped mean", y = "Count")
ggsave("report/figures/D_boot_hist.pdf", p_bh, width = 12, height = 12)

p_bq <- ggplot(boot_long, aes(sample = boot)) +
  stat_qq(size = 0.3) + stat_qq_line() +
  facet_wrap(~ panel, scales = "free", ncol = 4, labeller = label_wrap_gen(28)) +
  labs(x = "Theoretical normal quantile", y = "Bootstrapped mean")
ggsave("report/figures/D_boot_qq.pdf", p_bq, width = 12, height = 12)

# ---- 8. The descriptive table (D_table.tex) ----
# YOUR judgement from the histograms and Q-Q plots of each variable/group,
# in the same order as the printed table. Check each one against the figures.
normal_plots <- c(
  "No, left-skewed",              #  1 Rating: All cafes
  "No, left-skewed",              #  2 Adjusted rating: All cafes
  "No, strongly right-skewed",    #  3 Number of ratings: All cafes
  "No, heavy right tail",         #  4 Adjusted minus raw (A3)
  "No, left-skewed",              #  5 A2: Above median
  "No, left-skewed",              #  6 A2: Below median
  "Roughly normal",               #  7 A4: Few
  "No, left-skewed",              #  8 A4: Medium
  "No, left-skewed",              #  9 A4: Many
  "No, left-skewed",              # 10 A8: Independent
  "No, left-skewed",              # 11 A8: Local Chain
  "Cannot judge (n = 8)",         # 12 A8: International Chain
  "Roughly normal",               # 13 A9 rating: Under 100
  "No, left-skewed",              # 14 A9 rating: 100 or more
  "Roughly normal",               # 15 A9 adjusted rating: Under 100
  "No, left-skewed"               # 16 A9 adjusted rating: 100 or more
)
stopifnot(length(normal_plots) == nrow(res))

# Rule for the central limit theorem: smallest group has n >= 30 AND the
# skewness of the bootstrapped means is below 0.5 in absolute value
# (which the Q-Q plots of the bootstrapped means confirm).
tab <- res %>%
  mutate(normal_plots = normal_plots,
         clt = case_when(n < 30 ~ "No (n < 30)",
                         abs(skew_boot) < 0.5 ~ "Yes",
                         .default = "No (means skewed)"),
         conclusion = case_when(
           normal_plots == "Roughly normal" ~ "Parametric",
           clt == "Yes"                     ~ "Parametric (CLT)",
           n < 30                           ~ "Undecided (n too small)",
           .default                         = "Non-parametric"))

tab %>% select(panel, n, normal_plots, clt, conclusion) %>% print(n = Inf, width = Inf)

# Write the table as LaTeX (booktabs). The caption is added in the report source.
tex_escape <- function(x) {
  x <- str_replace_all(x, fixed("&"), "\\&")
  x <- str_replace_all(x, fixed("<"), "\\textless{}")
  x <- str_replace_all(x, fixed(">"), "\\textgreater{}")
  x
}

row_tex <- tab %>%
  transmute(line = paste(tex_escape(as.character(panel)), n,
                         sprintf("%.2f", skew), sprintf("%.2f", skew_boot),
                         tex_escape(normal_plots), tex_escape(clt),
                         tex_escape(conclusion), sep = " & ")) %>%
  pull(line)

tex <- c("\\begin{tabular}{p{5.2cm}rrrp{2.6cm}p{2.2cm}p{2.6cm}}",
         "\\toprule",
         "Variable: group & $n$ & Skew & Skew of boot. means & Normal (plots) & CLT applies & Conclusion \\\\",
         "\\midrule",
         paste0(row_tex, " \\\\"),
         "\\bottomrule",
         "\\end{tabular}")

dir.create("report/tables", showWarnings = FALSE, recursive = TRUE)
writeLines(tex, "report/tables/D_table.tex")

# ---- 9. Full descriptive statistics for the appendix ----
fmt <- function(x) sprintf("%.2f", x)

# Numeric variables and groups (reuses `long` from section 6)
stats <- long %>%
  group_by(set, group) %>%
  summarise(n = n(), mean = mean(value), sd = sd(value),
            median = median(value), iqr = IQR(value),
            min = min(value), max = max(value), .groups = "drop") %>%
  mutate(panel = paste0(set, ": ", group))

stats_tex <- c(
  "\\begin{tabular}{p{5.6cm}rrrrrrr}",
  "\\toprule",
  "Variable: group & $n$ & Mean & SD & Median & IQR & Min & Max \\\\",
  "\\midrule",
  paste0(tex_escape(stats$panel), " & ", stats$n, " & ", fmt(stats$mean), " & ",
         fmt(stats$sd), " & ", fmt(stats$median), " & ", fmt(stats$iqr), " & ",
         fmt(stats$min), " & ", fmt(stats$max), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}")
writeLines(stats_tex, "report/tables/D_stats.tex")

# Categorical variables: counts and percentages (missing values shown as a row)
cat_vars <- c(cafe_type = "Cafe type", top_rated = "Top-rated (4.7 or higher)",
              n_group_median = "Number of ratings vs median",
              n_group_3 = "Ratings group (A4)", n_group_2 = "Ratings group (A9)")

counts <- map_dfr(names(cat_vars), function(v) {
  dat %>%
    count(level = as.character(.data[[v]]), name = "n") %>%
    mutate(variable = cat_vars[[v]],
           level = replace_na(level, "Missing"),
           pct = 100 * n / sum(n))
})
print(counts, n = Inf)

counts_tex <- c(
  "\\begin{tabular}{p{4.6cm}p{4.2cm}rr}",
  "\\toprule",
  "Variable & Category & $n$ & \\% \\\\",
  "\\midrule",
  paste0(tex_escape(counts$variable), " & ", tex_escape(counts$level), " & ",
         counts$n, " & ", sprintf("%.1f", counts$pct), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}")
writeLines(counts_tex, "report/tables/D_counts.tex")
