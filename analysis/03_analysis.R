# ============================================================
# 03_analysis.R
# Answers the research questions A1-A10 (topic: Ratings).
# Reads analysis/dataset.csv. Run from the project root.
# alpha = .05 throughout. The hypotheses were written in
# hypotheses.md before the data were cleaned or looked at.
# ============================================================

library(tidyverse)
set.seed(2026)                       # for every bootstrap
dir.create("report/tables",  showWarnings = FALSE, recursive = TRUE)
dir.create("report/figures", showWarnings = FALSE, recursive = TRUE)

dat <- read_csv("analysis/dataset.csv", show_col_types = FALSE) %>%
  mutate(cafe_type = factor(cafe_type,
                            levels = c("Independent", "Local Chain", "International Chain")),
         n_group_median = factor(n_group_median, levels = c("Above median", "Below median")),
         n_group_3 = factor(n_group_3,
                            levels = c("Few (<100)", "Medium (100-1,000)", "Many (>1,000)")),
         n_group_2 = factor(n_group_2, levels = c("Under 100", "100 or more")))

# ---- Helpers: number formatting and LaTeX tables (booktabs) ----
fmt   <- function(x, d = 2) sprintf(paste0("%.", d, "f"), x)
fmt_p <- function(p) ifelse(p < .001, "< .001", sub("^0", "", sprintf("%.3f", p)))
ci_txt <- function(lo, hi, d = 2) paste0("[", fmt(lo, d), ", ", fmt(hi, d), "]")

tex_escape <- function(x) {
  x <- str_replace_all(x, fixed("&"), "\\&")
  x <- str_replace_all(x, fixed("%"), "\\%")
  x <- str_replace_all(x, fixed("<"), "\\textless{}")
  x <- str_replace_all(x, fixed(">"), "\\textgreater{}")
  x
}

write_tex_table <- function(df, file, align = NULL) {
  df <- mutate(df, across(everything(), as.character))
  if (is.null(align)) align <- paste0("l", strrep("r", ncol(df) - 1))
  lines <- c(paste0("\\begin{tabular}{", align, "}"), "\\toprule",
             paste0(paste(tex_escape(names(df)), collapse = " & "), " \\\\"),
             "\\midrule",
             apply(df, 1, function(r) paste0(paste(tex_escape(r), collapse = " & "), " \\\\")),
             "\\bottomrule", "\\end{tabular}")
  writeLines(lines, file.path("report/tables", file))
}

# two-column table: Quantity | Value
vtab <- function(...) {
  x <- list(...)
  tibble(Quantity = names(x), Value = unname(unlist(x)))
}

# ---- Regression helpers: HC3 robust SEs, Breusch-Pagan test, tables ----
hc3 <- function(m) {                                   # HC3 robust covariance matrix
  X <- model.matrix(m); w <- resid(m) / (1 - hatvalues(m))
  XtXi <- solve(crossprod(X))
  XtXi %*% crossprod(X * w) %*% XtXi
}
bp_p <- function(m) {                                  # Breusch-Pagan (Koenker): e^2 on fitted values
  aux <- lm(resid(m)^2 ~ fitted(m))
  pchisq(length(resid(m)) * summary(aux)$r.squared, df = 1, lower.tail = FALSE)
}
coef_table <- function(m, V) {
  est <- unname(coef(m)); se <- unname(sqrt(diag(V))); dfr <- df.residual(m)
  tibble(term = names(coef(m)), est = est, se = se,
         lo = est - qt(.975, dfr) * se, hi = est + qt(.975, dfr) * se,
         p = 2 * pt(abs(est / se), dfr, lower.tail = FALSE))
}
write_reg_table <- function(m, V, file, labels, unadj = NULL, extra = NULL) {
  tab <- coef_table(m, V)
  if (!is.null(unadj)) tab$unadj <- unname(unadj[tab$term])
  tab$term <- unname(labels[tab$term])
  out <- tab %>% transmute(Term = term, `$b$` = fmt(est, 3), SE = fmt(se, 3),
                           `95% CI` = ci_txt(lo, hi, 3), `$p$` = fmt_p(p))
  if (!is.null(unadj)) out <- out %>%
    mutate(`Unadjusted $b$` = ifelse(is.na(tab$unadj), "", fmt(tab$unadj, 3)), .after = Term)
  foot <- tibble(Term = c("$n$", "$R^2$"),
                 `$b$` = c(as.character(nobs(m)), fmt(summary(m)$r.squared, 3)))
  out <- bind_rows(out, extra, foot) %>%
    mutate(across(everything(), ~ replace_na(as.character(.x), "")))
  write_tex_table(out, file, align = paste0("l", strrep("r", ncol(out) - 1)))
}
diag_pdf <- function(m, file) {                        # residuals vs fitted + normal Q-Q
  pdf(file.path("report/figures", file), width = 8, height = 4)
  par(mfrow = c(1, 2)); plot(m, which = 1); plot(m, which = 2)
  invisible(dev.off())
}

# ============================================================
# A1  One sample: is the mean Google rating different from 4.5?
# H0: mean = 4.5   H1: mean != 4.5 (two-tailed)
# Method: one-sample t-test (CLT applies, see D_table)
# ============================================================
a1_x  <- dat$rating[!is.na(dat$rating)]
a1_tt <- t.test(a1_x, mu = 4.5)
a1 <- tibble(n = length(a1_x), mean = mean(a1_x), sd = sd(a1_x),
             lo = a1_tt$conf.int[1], hi = a1_tt$conf.int[2],
             t = unname(a1_tt$statistic), df = unname(a1_tt$parameter),
             p = a1_tt$p.value)
print(a1, width = Inf)

write_tex_table(vtab(
  "Cafes (n)"                      = a1$n,
  "Mean rating (points)"           = fmt(a1$mean),
  "SD (points)"                    = fmt(a1$sd),
  "95% CI for the mean"            = ci_txt(a1$lo, a1$hi),
  "Mean minus 4.5 (points)"        = fmt(a1$mean - 4.5),
  "$t$ (df)"                       = paste0(fmt(a1$t), " (", a1$df, ")"),
  "$p$ (two-tailed)"               = fmt_p(a1$p)
), "A1_table.tex", align = "lr")

a1_fig <- ggplot(tibble(rating = a1_x), aes(x = "All cafes", y = rating)) +
  geom_jitter(width = 0.15, alpha = 0.3) +
  geom_hline(yintercept = 4.5, linetype = 2) +
  geom_pointrange(data = a1, aes(x = "All cafes", y = mean, ymin = lo, ymax = hi),
                  inherit.aes = FALSE, colour = "red", size = 0.8) +
  labs(x = NULL, y = "Google rating (points)")
ggsave("report/figures/A1_figure.pdf", a1_fig, width = 5, height = 4)

# ============================================================
# A2  Two groups: do cafes with an above-median number of ratings
#     have LOWER ratings than cafes with a below-median number?
# H0: equal means   H1: mean(above) - mean(below) < 0 (one-tailed)
# Method: Welch t-test (CLT applies in both groups, ~94 cafes each)
# ============================================================
a2 <- dat %>% filter(!is.na(rating), !is.na(n_group_median))
g1 <- a2$rating[a2$n_group_median == "Above median"]
g2 <- a2$rating[a2$n_group_median == "Below median"]
n1 <- length(g1); n2 <- length(g2)

# first level = "Above median", so R computes Above minus Below
a2_two <- t.test(rating ~ n_group_median, data = a2)                        # two-sided: CI
a2_one <- t.test(rating ~ n_group_median, data = a2, alternative = "less")  # one-tailed p
diff2  <- mean(g1) - mean(g2)
sp     <- sqrt(((n1 - 1) * var(g1) + (n2 - 1) * var(g2)) / (n1 + n2 - 2))

# equality of variances (Brown-Forsythe): Welch's test is used regardless
a2$absdev <- abs(a2$rating - ave(a2$rating, a2$n_group_median, FUN = median))
lev_p <- anova(lm(absdev ~ n_group_median, data = a2))$`Pr(>F)`[1]

print(tibble(diff = diff2, lo = a2_two$conf.int[1], hi = a2_two$conf.int[2],
             d = diff2 / sp, t = unname(a2_one$statistic),
             df = unname(a2_one$parameter), p_one = a2_one$p.value, lev_p = lev_p),
      width = Inf)

write_tex_table(vtab(
  "Above median: n, mean (SD)"         = paste0(n1, ", ", fmt(mean(g1)), " (", fmt(sd(g1)), ")"),
  "Below median: n, mean (SD)"         = paste0(n2, ", ", fmt(mean(g2)), " (", fmt(sd(g2)), ")"),
  "Difference, above minus below (points)" = fmt(diff2),
  "95% CI for the difference"          = ci_txt(a2_two$conf.int[1], a2_two$conf.int[2]),
  "Cohen $d$"                          = fmt(diff2 / sp),
  "Welch $t$ (df)"                     = paste0(fmt(a2_one$statistic), " (", fmt(a2_one$parameter), ")"),
  "$p$ (one-tailed, fixed in advance)" = fmt_p(a2_one$p.value),
  "Equal variances (Brown-Forsythe) $p$" = fmt_p(lev_p)
), "A2_table.tex", align = "lr")

a2_ci <- a2 %>% group_by(n_group_median) %>%
  summarise(m = mean(rating), se = sd(rating) / sqrt(n()),
            lo = m - qt(.975, n() - 1) * se, hi = m + qt(.975, n() - 1) * se,
            .groups = "drop")
a2_fig <- ggplot(a2, aes(n_group_median, rating)) +
  geom_jitter(width = 0.15, alpha = 0.3) +
  geom_pointrange(data = a2_ci, aes(y = m, ymin = lo, ymax = hi),
                  colour = "red", size = 0.8) +
  labs(x = "Number of Google ratings", y = "Google rating (points)")
ggsave("report/figures/A2_figure.pdf", a2_fig, width = 5, height = 4)

# ============================================================
# A3  Paired: is the Adjusted Rating different from the raw rating?
# H0: mean difference = 0   H1: mean difference != 0 (two-tailed)
# Difference = adjusted minus raw. Method: paired t-test (CLT applies,
# but the differences are very skewed: a few cafes drive the mean).
# ============================================================
a3   <- dat %>% filter(!is.na(rating), !is.na(adj_rating))
a3_d <- a3$adj_rating - a3$rating
a3_tt <- t.test(a3$adj_rating, a3$rating, paired = TRUE)
dz   <- mean(a3_d) / sd(a3_d)

print(tibble(n = nrow(a3), mean_diff = mean(a3_d), sd_diff = sd(a3_d),
             lo = a3_tt$conf.int[1], hi = a3_tt$conf.int[2], dz = dz,
             t = unname(a3_tt$statistic), df = unname(a3_tt$parameter),
             p = a3_tt$p.value, median_diff = median(a3_d)), width = Inf)

write_tex_table(vtab(
  "Cafes (n)"                               = nrow(a3),
  "Mean difference, adjusted minus raw (points)" = fmt(mean(a3_d)),
  "SD of the differences (points)"          = fmt(sd(a3_d)),
  "95% CI for the mean difference"          = ci_txt(a3_tt$conf.int[1], a3_tt$conf.int[2]),
  "$d_z$"                                   = fmt(dz),
  "$t$ (df)"                                = paste0(fmt(a3_tt$statistic), " (", a3_tt$parameter, ")"),
  "$p$ (two-tailed)"                        = fmt_p(a3_tt$p.value)
), "A3_table.tex", align = "lr")

a3_est <- tibble(m = mean(a3_d), lo = a3_tt$conf.int[1], hi = a3_tt$conf.int[2])
a3_fig <- ggplot(tibble(d = a3_d), aes(d)) +
  geom_histogram(binwidth = 0.02, fill = "grey70", colour = "white") +
  geom_vline(xintercept = 0, linetype = 2) +
  geom_pointrange(data = a3_est, aes(x = m, xmin = lo, xmax = hi, y = -4),
                  inherit.aes = FALSE, colour = "red") +
  labs(x = "Adjusted minus raw rating (points)", y = "Number of cafes")
ggsave("report/figures/A3_figure.pdf", a3_fig, width = 6, height = 4)

# ============================================================
# A4  More groups: does the rating differ between cafes with few (<100),
#     medium (100-1,000) and many (>1,000) ratings?
# H0: all three means are equal   H1: at least one mean differs (two-tailed)
# Method: Welch's ANOVA (parametric per D_table; variances may differ),
#         pairwise comparisons with Games-Howell.
# ============================================================
a4 <- dat %>% filter(!is.na(rating), !is.na(n_group_3))

# equality of variances (Brown-Forsythe); Welch's ANOVA is used regardless
a4$absdev <- abs(a4$rating - ave(a4$rating, a4$n_group_3, FUN = median))
a4_lev_p  <- anova(lm(absdev ~ n_group_3, data = a4))$`Pr(>F)`[1]

a4_w <- oneway.test(rating ~ n_group_3, data = a4, var.equal = FALSE)

a4_g <- a4 %>%
  group_by(n_group_3) %>%
  summarise(n = n(), m = mean(rating), s = sd(rating), .groups = "drop") %>%
  mutate(se = s / sqrt(n),
         lo = m - qt(.975, n - 1) * se,
         hi = m + qt(.975, n - 1) * se)

# Games-Howell pairwise comparisons (second-listed group minus first-listed group)
k <- nrow(a4_g)
a4_gh <- map_dfr(combn(as.character(a4_g$n_group_3), 2, simplify = FALSE), function(pr) {
  i <- a4_g[a4_g$n_group_3 == pr[1], ]
  j <- a4_g[a4_g$n_group_3 == pr[2], ]
  d    <- j$m - i$m
  v_i  <- i$s^2 / i$n
  v_j  <- j$s^2 / j$n
  se   <- sqrt(v_i + v_j)
  df   <- (v_i + v_j)^2 / (v_i^2 / (i$n - 1) + v_j^2 / (j$n - 1))
  q    <- abs(d) / se * sqrt(2)
  crit <- qtukey(.95, nmeans = k, df = df) / sqrt(2)
  tibble(pair = paste(pr[2], "minus", pr[1]), diff = d,
         lo = d - crit * se, hi = d + crit * se,
         p_adj = ptukey(q, nmeans = k, df = df, lower.tail = FALSE))
})
print(a4_g, width = Inf)
print(a4_gh, width = Inf)
print(c(F = unname(a4_w$statistic), df1 = unname(a4_w$parameter[1]),
        df2 = unname(a4_w$parameter[2]), p = a4_w$p.value, lev_p = a4_lev_p))

write_tex_table(vtab(
  "Cafes (n)"                           = nrow(a4),
  "Welch $F$ ($df_1$, $df_2$)"          = paste0(fmt(a4_w$statistic), " (", a4_w$parameter[1], ", ",
                                                 fmt(a4_w$parameter[2]), ")"),
  "$p$ (two-tailed)"                    = fmt_p(a4_w$p.value),
  "Equal variances (Brown-Forsythe) $p$" = fmt_p(a4_lev_p)
), "A4_table.tex", align = "lr")

write_tex_table(a4_g %>% transmute(Group = as.character(n_group_3), n = n,
                                   `Mean (SD)` = paste0(fmt(m), " (", fmt(s), ")"),
                                   `95% CI` = ci_txt(lo, hi)),
                "A4_groups.tex", align = "lrrr")

write_tex_table(a4_gh %>% transmute(Comparison = pair, `Difference (points)` = fmt(diff),
                                    `95% CI` = ci_txt(lo, hi), `Adjusted $p$` = fmt_p(p_adj)),
                "A4_pairs.tex", align = "lrrr")

a4_fig <- ggplot(a4, aes(n_group_3, rating)) +
  geom_jitter(width = 0.15, alpha = 0.3) +
  geom_pointrange(data = a4_g, aes(y = m, ymin = lo, ymax = hi),
                  colour = "red", size = 0.8) +
  labs(x = "Number of Google ratings", y = "Google rating (points)")
ggsave("report/figures/A4_figure.pdf", a4_fig, width = 6, height = 4)

# ============================================================
# A5  Two categorical: is a cafe with a below-median number of ratings
#     more likely to be top-rated (4.7 or higher), and by how many pp?
# H0: equal shares   H1: share(below median) > share(above median) (one-tailed)
# Method: chi-square on the 2x2 table (one-tailed p from the z-test for two
#         proportions, which is the same test); Fisher if any expected count < 5.
# ============================================================
a5   <- dat %>% filter(!is.na(top_rated), !is.na(n_group_median))
tab5 <- table(a5$n_group_median, a5$top_rated)
print(tab5)

x_b <- tab5["Below median", "1"]; n_b <- sum(tab5["Below median", ])
x_a <- tab5["Above median", "1"]; n_a <- sum(tab5["Above median", ])

a5_two <- prop.test(c(x_b, x_a), c(n_b, n_a), correct = FALSE)                          # CI
a5_one <- prop.test(c(x_b, x_a), c(n_b, n_a), correct = FALSE, alternative = "greater") # one-tailed p
a5_chi <- chisq.test(tab5, correct = FALSE)
a5_V   <- sqrt(unname(a5_chi$statistic) / sum(tab5))        # Cramer's V for a 2x2 table
a5_min <- min(a5_chi$expected)
if (a5_min < 5) print(fisher.test(tab5))                    # only needed if an expected count < 5
print(round(a5_chi$stdres, 2))

print(tibble(share_below = x_b / n_b, share_above = x_a / n_a,
             diff_pp = 100 * (x_b / n_b - x_a / n_a),
             lo_pp = 100 * a5_two$conf.int[1], hi_pp = 100 * a5_two$conf.int[2],
             chi2 = unname(a5_chi$statistic), N = sum(tab5), p_one = a5_one$p.value,
             min_expected = a5_min, V = a5_V), width = Inf)

write_tex_table(vtab(
  "Below median: top-rated / cafes (%)" = paste0(x_b, " / ", n_b, " (", fmt(100 * x_b / n_b, 1), "%)"),
  "Above median: top-rated / cafes (%)" = paste0(x_a, " / ", n_a, " (", fmt(100 * x_a / n_a, 1), "%)"),
  "Difference, below minus above (percentage points)" = fmt(100 * (x_b / n_b - x_a / n_a), 1),
  "95% CI for the difference (percentage points)" = ci_txt(100 * a5_two$conf.int[1], 100 * a5_two$conf.int[2], 1),
  "$\\chi^2$ ($df$, $N$)"                 = paste0(fmt(a5_chi$statistic), " (1, ", sum(tab5), ")"),
  "$p$ (one-tailed, fixed in advance)"    = fmt_p(a5_one$p.value),
  "Smallest expected count"               = fmt(a5_min, 1),
  "Cram\\'er's $V$"                       = fmt(a5_V)
), "A5_table.tex", align = "lr")

a5_p <- bind_rows(
  tibble(group = "Below median", x = x_b, n = n_b, lo = prop.test(x_b, n_b, correct = FALSE)$conf.int[1],
         hi = prop.test(x_b, n_b, correct = FALSE)$conf.int[2]),
  tibble(group = "Above median", x = x_a, n = n_a, lo = prop.test(x_a, n_a, correct = FALSE)$conf.int[1],
         hi = prop.test(x_a, n_a, correct = FALSE)$conf.int[2])
) %>% mutate(share = 100 * x / n, lo = 100 * lo, hi = 100 * hi)
a5_fig <- ggplot(a5_p, aes(group, share)) +
  geom_col(fill = "grey70", width = 0.5) +
  geom_pointrange(aes(ymin = lo, ymax = hi), colour = "red") +
  labs(x = "Number of Google ratings", y = "Top-rated cafes (4.7 or higher), %")
ggsave("report/figures/A5_figure.pdf", a5_fig, width = 5, height = 4)

# ============================================================
# A6  Correlation: is the Google rating correlated with the number of ratings?
# H0: rho = 0   H1: rho != 0 (two-tailed)
# Method: Spearman (the number of ratings is not rescued by the CLT, see D_table),
#         with a bootstrap percentile CI (5,000 resamples, seed 2026).
# ============================================================
a6    <- dat %>% filter(!is.na(rating), !is.na(n_ratings))
a6_ct <- cor.test(a6$n_ratings, a6$rating, method = "spearman", exact = FALSE)
rho   <- unname(a6_ct$estimate)
set.seed(2026)
a6_boot <- replicate(5000, {
  i <- sample(nrow(a6), replace = TRUE)
  cor(a6$n_ratings[i], a6$rating[i], method = "spearman")
})
a6_ci <- unname(quantile(a6_boot, c(.025, .975)))
print(tibble(n = nrow(a6), rho = rho, lo = a6_ci[1], hi = a6_ci[2],
             rho2 = rho^2, p = a6_ct$p.value), width = Inf)

write_tex_table(vtab(
  "Cafes (n)"                       = nrow(a6),
  "Spearman $\\rho$"                = fmt(rho),
  "95% bootstrap CI"                = ci_txt(a6_ci[1], a6_ci[2]),
  "$\\rho^2$ (shared variance)"     = fmt(rho^2),
  "$p$ (two-tailed)"                = fmt_p(a6_ct$p.value)
), "A6_table.tex", align = "lr")

a6_fig <- ggplot(a6, aes(n_ratings, rating)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", formula = y ~ x, colour = "red") +
  scale_x_log10(labels = scales::comma) +
  labs(x = "Number of Google ratings (log scale)", y = "Google rating (points)")
ggsave("report/figures/A6_figure.pdf", a6_fig, width = 6, height = 4)

# ============================================================
# A7  Simple regression: by how many points does the rating change per 1,000 ratings?
# H0: slope = 0   H1: slope != 0 (two-tailed)
# Method: OLS, robust (HC3) SEs if Breusch-Pagan rejects constant variance.
# ============================================================
a7  <- dat %>% filter(!is.na(rating), !is.na(ratings_k))
m7  <- lm(rating ~ ratings_k, data = a7)
a7_bp <- bp_p(m7)
a7_V  <- if (a7_bp < .05) hc3(m7) else vcov(m7)
print(coef_table(m7, a7_V))
cat("A7: n =", nobs(m7), " R2 =", summary(m7)$r.squared, " Breusch-Pagan p =", a7_bp,
    " robust SEs used:", a7_bp < .05, "\n")
a7 %>% mutate(cooks = cooks.distance(m7), leverage = hatvalues(m7)) %>%
  arrange(desc(cooks)) %>% select(cafe_name, n_ratings, rating, cooks, leverage) %>%
  slice_head(n = 4) %>% print(width = Inf)
# Exploratory (not planned): the same model without the cafe with the most ratings
m7x <- lm(rating ~ ratings_k, data = filter(a7, ratings_k < max(ratings_k)))
print(coef_table(m7x, vcov(m7x)))

write_reg_table(m7, a7_V, "A7_table.tex",
                labels = c("(Intercept)" = "Intercept", "ratings_k" = "Ratings (per 1,000)"))
diag_pdf(m7, "A7_diagnostics.pdf")
a7_fig <- ggplot(a7, aes(ratings_k, rating)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", formula = y ~ x, colour = "red") +
  labs(x = "Number of Google ratings (thousands)", y = "Google rating (points)")
ggsave("report/figures/A7_figure.pdf", a7_fig, width = 6, height = 4)

# ============================================================
# A8  Multiple regression: does the rating differ by cafe type once the
#     number of ratings is accounted for, and how much of the difference remains?
# H0: both cafe-type coefficients = 0   H1: at least one differs (joint Wald F test)
# Reference group: Independent.
# ============================================================
a8  <- dat %>% filter(!is.na(rating), !is.na(ratings_k), !is.na(cafe_type))
m8  <- lm(rating ~ cafe_type + ratings_k, data = a8)
m8u <- lm(rating ~ cafe_type, data = a8)               # unadjusted, same cafes
a8_bp <- bp_p(m8)
a8_V  <- if (a8_bp < .05) hc3(m8) else vcov(m8)
idx   <- grep("^cafe_type", names(coef(m8)))
b8    <- coef(m8)[idx]
a8_F  <- drop(t(b8) %*% solve(a8_V[idx, idx]) %*% b8) / length(idx)
a8_p  <- pf(a8_F, length(idx), df.residual(m8), lower.tail = FALSE)
print(left_join(coef_table(m8, a8_V),
                coef_table(m8u, vcov(m8u)) %>% select(term, est_unadj = est), by = "term"))
cat("A8: n =", nobs(m8), " R2 =", summary(m8)$r.squared, " Breusch-Pagan p =", a8_bp,
    " robust:", a8_bp < .05, " joint F(", length(idx), ",", df.residual(m8), ") =", a8_F,
    " p =", a8_p, "\n")

write_reg_table(m8, a8_V, "A8_table.tex",
                labels = c("(Intercept)" = "Intercept", "cafe_typeLocal Chain" = "Local chain (vs independent)",
                           "cafe_typeInternational Chain" = "International chain (vs independent)",
                           "ratings_k" = "Ratings (per 1,000)"),
                unadj = coef(m8u),
                extra = tibble(Term = "Joint test of cafe type: $F$ ($df_1$, $df_2$), $p$",
                               `$b$` = paste0(fmt(a8_F), " (", length(idx), ", ", df.residual(m8), "), ", fmt_p(a8_p))))
diag_pdf(m8, "A8_diagnostics.pdf")

# adjusted mean rating by type (at the mean number of ratings), with CI from the chosen covariance matrix
nd8 <- tibble(cafe_type = factor(levels(a8$cafe_type), levels = levels(a8$cafe_type)),
              ratings_k = mean(a8$ratings_k))
X0  <- model.matrix(~ cafe_type + ratings_k, data = nd8)
nd8 <- nd8 %>% mutate(fit = drop(X0 %*% coef(m8)),
                      se  = sqrt(rowSums((X0 %*% a8_V) * X0)),
                      lo  = fit - qt(.975, df.residual(m8)) * se,
                      hi  = fit + qt(.975, df.residual(m8)) * se)
a8_fig <- ggplot(a8, aes(cafe_type, rating)) +
  geom_jitter(width = 0.15, alpha = 0.3) +
  geom_pointrange(data = nd8, aes(y = fit, ymin = lo, ymax = hi), colour = "red", size = 0.8) +
  labs(x = "Cafe type", y = "Google rating (points)")
ggsave("report/figures/A8_figure.pdf", a8_fig, width = 6, height = 4)

# ============================================================
# A9  Interaction: does the relationship between the raw and the Adjusted Rating
#     differ between cafes with under 100 and with 100 or more ratings?
# H0: interaction = 0   H1: interaction != 0 (two-tailed). Reference: under 100.
# ============================================================
a9  <- dat %>% filter(!is.na(rating), !is.na(adj_rating), !is.na(n_group_2))
m9  <- lm(adj_rating ~ rating * n_group_2, data = a9)
a9_bp <- bp_p(m9)
a9_V  <- if (a9_bp < .05) hc3(m9) else vcov(m9)
print(coef_table(m9, a9_V))
b9 <- coef(m9); i_r <- "rating"; i_x <- "rating:n_group_2100 or more"
dfr9 <- df.residual(m9); tq <- qt(.975, dfr9)
sl_u <- b9[[i_r]];             se_u <- sqrt(a9_V[i_r, i_r])
sl_o <- b9[[i_r]] + b9[[i_x]]; se_o <- sqrt(a9_V[i_r, i_r] + a9_V[i_x, i_x] + 2 * a9_V[i_r, i_x])
cat("A9: n =", nobs(m9), " R2 =", summary(m9)$r.squared, " Breusch-Pagan p =", a9_bp,
    " robust:", a9_bp < .05, "\n slope under 100 =", sl_u, "+-", se_u,
    " | slope 100 or more =", sl_o, "+-", se_o, "\n")

write_reg_table(m9, a9_V, "A9_table.tex",
                labels = c("(Intercept)" = "Intercept", "rating" = "Raw rating (under 100 ratings)",
                           "n_group_2100 or more" = "100 or more ratings (vs under 100)",
                           "rating:n_group_2100 or more" = "Raw rating x 100 or more ratings"),
                extra = tibble(Term = c("Slope of adjusted on raw rating, under 100 ratings",
                                        "Slope of adjusted on raw rating, 100 or more ratings"),
                               `$b$`  = c(fmt(sl_u, 3), fmt(sl_o, 3)),
                               SE     = c(fmt(se_u, 3), fmt(se_o, 3)),
                               `95% CI` = c(ci_txt(sl_u - tq * se_u, sl_u + tq * se_u, 3),
                                            ci_txt(sl_o - tq * se_o, sl_o + tq * se_o, 3))))
diag_pdf(m9, "A9_diagnostics.pdf")
a9_fig <- ggplot(a9, aes(rating, adj_rating, colour = n_group_2, shape = n_group_2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "black") +
  labs(x = "Google rating (points)", y = "Adjusted rating (points)",
       colour = "Number of ratings", shape = "Number of ratings")
ggsave("report/figures/A9_figure.pdf", a9_fig, width = 6.5, height = 4.5)

# ============================================================
# A10  Logistic regression: does the probability of being top-rated (4.7 or higher)
#      change with each additional 1,000 ratings, and by how many percentage points?
# H0: coefficient = 0   H1: coefficient != 0 (two-tailed)
# Marginal effect: average marginal effect (AME) in percentage points,
# bootstrap percentile CI (5,000 resamples, seed 2026).
# ============================================================
a10 <- dat %>% filter(!is.na(top_rated), !is.na(ratings_k))
m10 <- glm(top_rated ~ ratings_k, data = a10, family = binomial)
ame <- function(m, d) {
  p <- predict(m, newdata = d, type = "response")
  100 * mean(coef(m)[["ratings_k"]] * p * (1 - p))
}
ame_hat <- ame(m10, a10)
set.seed(2026)
a10_boot <- replicate(5000, {
  d  <- a10[sample(nrow(a10), replace = TRUE), ]
  mb <- suppressWarnings(glm(top_rated ~ ratings_k, data = d, family = binomial))
  ame(mb, d)
})
a10_ci <- unname(quantile(a10_boot, c(.025, .975)))
b10  <- summary(m10)$coefficients["ratings_k", ]
ci10 <- unname(confint.default(m10)["ratings_k", ])
print(tibble(n = nrow(a10), events = sum(a10$top_rated), b = unname(b10[["Estimate"]]),
             se = unname(b10[["Std. Error"]]), lo = ci10[1], hi = ci10[2],
             OR = exp(unname(b10[["Estimate"]])), z = unname(b10[["z value"]]),
             p = unname(b10[["Pr(>|z|)"]]), ame_pp = ame_hat,
             ame_lo = a10_ci[1], ame_hi = a10_ci[2],
             mcfadden = 1 - m10$deviance / m10$null.deviance), width = Inf)
a10 %>% mutate(cooks = cooks.distance(m10)) %>% arrange(desc(cooks)) %>%
  select(cafe_name, n_ratings, top_rated, cooks) %>% slice_head(n = 4) %>% print(width = Inf)
# Exploratory (not planned): without the cafe with the most ratings
a10x <- filter(a10, ratings_k < max(ratings_k))
m10x <- glm(top_rated ~ ratings_k, data = a10x, family = binomial)
cat("A10 exploratory, without the largest cafe: AME =", ame(m10x, a10x), "pp\n")

write_tex_table(vtab(
  "Cafes (n); top-rated (n)"                       = paste0(nrow(a10), "; ", sum(a10$top_rated)),
  "Coefficient, log-odds per 1,000 ratings (SE)"   = paste0(fmt(b10[["Estimate"]], 3), " (", fmt(b10[["Std. Error"]], 3), ")"),
  "95% CI for the coefficient"                     = ci_txt(ci10[1], ci10[2], 3),
  "Odds ratio per 1,000 ratings"                   = fmt(exp(b10[["Estimate"]]), 3),
  "95% CI for the odds ratio"                      = ci_txt(exp(ci10[1]), exp(ci10[2]), 3),
  "Average marginal effect (percentage points per 1,000 ratings)" = fmt(ame_hat, 2),
  "95% bootstrap CI for the marginal effect"       = ci_txt(a10_ci[1], a10_ci[2], 2),
  "$z$"                                            = fmt(b10[["z value"]]),
  "$p$ (two-tailed)"                               = fmt_p(b10[["Pr(>|z|)"]]),
  "McFadden $R^2$"                                 = fmt(1 - m10$deviance / m10$null.deviance, 3)
), "A10_table.tex", align = "lr")

nd10 <- tibble(ratings_k = seq(0, max(a10$ratings_k), length.out = 200))
pl   <- predict(m10, nd10, type = "link", se.fit = TRUE)
nd10 <- nd10 %>% mutate(fit = plogis(pl$fit),
                        lo  = plogis(pl$fit - 1.96 * pl$se.fit),
                        hi  = plogis(pl$fit + 1.96 * pl$se.fit))
a10_fig <- ggplot(nd10, aes(ratings_k, 100 * fit)) +
  geom_jitter(data = a10, aes(y = 100 * top_rated), height = 2, width = 0, alpha = 0.3) +
  geom_ribbon(aes(ymin = 100 * lo, ymax = 100 * hi), alpha = 0.2) +
  geom_line(colour = "red") +
  labs(x = "Number of Google ratings (thousands)",
       y = "Probability of being top-rated (4.7 or higher), %")
ggsave("report/figures/A10_figure.pdf", a10_fig, width = 6, height = 4)