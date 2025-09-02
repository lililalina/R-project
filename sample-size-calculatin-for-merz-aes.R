

library(TrialSize)
library(epiR)
library(pROC)
library(pwr)

#One Sample Proportion
#OneSampleProportion.NIS(alpha, beta, p, delta, differ)
OneSampleProportion.NIS(0.05, 0.20, 0.70, -0.83, -1.00)


OneSampleProportion.NIS(0.05, 0.20, 0.86, 0.50, 0.40)


# GAIS sample size calculations (one-sample proportion) - step-by-step in R
# Purpose: show how to calculate sample sizes for GAIS responder rate (>=1-grade improvement)

# --------------------------
# 1) Inputs / assumptions
# --------------------------
alpha <- 0.05        # significance level
power <- 0.80        # desired power (1 - beta)
p0 <- 0.50           # null / benchmark responder proportion (e.g., clinically uninteresting rate)

# Pilot data: 30/30 responders -> compute exact (Clopper-Pearson) CI
x_pilot <- 30
n_pilot <- 30
pilot_ci <- binom.test(x_pilot, n_pilot, conf.level = 0.95)$conf.int
pilot_ci

# Use a conservative working p1 based on pilot lower CI (e.g. round-up)
p1_from_pilot <- pilot_ci[1]   # lower 95% bound from the pilot
p1_conservative <- round(p1_from_pilot, 2)   # e.g., 0.88 -> 0.88
p1_conservative

# Alternatively pick p1 = 0.90 for planning (example)
p1 <- 0.90

# --------------------------
# 2) Classic normal-approximation one-sample formula (two-sided)
#    n = [ (Z_{1-alpha/2} * sqrt(p0(1-p0)) + Z_{1-beta} * sqrt(p1(1-p1)))^2 ] / (p1 - p0)^2
# --------------------------
z_alpha_2sided <- qnorm(1 - alpha/2)
z_beta <- qnorm(power)

n_formula_2sided <- ((z_alpha_2sided * sqrt(p0*(1-p0)) + z_beta * sqrt(p1*(1-p1)))^2) / ((p1 - p0)^2)
n_formula_2sided <- ceiling(n_formula_2sided)

# One-sided version (use Z_{1-alpha} instead of Z_{1-alpha/2})
z_alpha_1sided <- qnorm(1 - alpha)
n_formula_1sided <- ((z_alpha_1sided * sqrt(p0*(1-p0)) + z_beta * sqrt(p1*(1-p1)))^2) / ((p1 - p0)^2)
n_formula_1sided <- ceiling(n_formula_1sided)

# Print results
cat("Normal-approx formula (two-sided): n =", n_formula_2sided, "subjects\n")
cat("Normal-approx formula (one-sided): n =", n_formula_1sided, "subjects\n\n")

# --------------------------
# 3) Using the pwr package (Cohen's h) for one-proportion test
#    h = 2*asin(sqrt(p1)) - 2*asin(sqrt(p0))
#    pwr.p.test(h = h, power = power, sig.level = alpha, alternative = "greater")
# --------------------------
if (!requireNamespace("pwr", quietly = TRUE)) {
  message("Package 'pwr' not installed. You can install it with install.packages('pwr') to use pwr.p.test().")
} else {
  library(pwr)
  h <- ES.h(p1, p0)
  pwr_res <- pwr.p.test(h = h, power = power, sig.level = alpha, alternative = "greater")
  n_pwr <- ceiling(pwr_res$n)
  cat("pwr::pwr.p.test() gives n =", n_pwr, "subjects (one-sample)\n\n")
}

# --------------------------
# 4) Subgroup-driven calculation (rule of thumb: ~30 per level)
#    Factors: skin type (6), age (2), BMI (2), ethnicity (5)
#    We take the largest driver (skin type with 6 levels)
# --------------------------
n_per_level <- 30
levels_skin <- 6
baseline_n <- n_per_level * levels_skin
cat("Baseline (30 per skin-type level):", baseline_n, "subjects\n")

# --------------------------
# 5) Adjust for missing/unusable data (10%, 15%, 20%)
# --------------------------
missing_pcts <- c(0.10, 0.15, 0.20)
for (m in missing_pcts) {
  adj <- ceiling(baseline_n * (1 + m))
  cat(sprintf("With %.0f%% missing -> n = %d\n", m*100, adj))
}
cat("\n")

# --------------------------
# 6) (Optional) Design effect for multi-site clustering
#    DE = 1 + (m - 1) * ICC, where m = average patients per site
# --------------------------
sites <- 8
m_avg <- baseline_n / sites
ICC_values <- c(0.01, 0.02)
for (icc in ICC_values) {
  DE <- 1 + (m_avg - 1) * icc
  n_de <- ceiling(baseline_n * DE)
  n_de_with_10pct <- ceiling(n_de * 1.10)
  cat(sprintf("ICC=%.3f -> DE=%.3f -> n= %d; with 10%% missing -> %d\n", icc, DE, n_de, n_de_with_10pct))
}

# --------------------------
# 7) Final recommended target (based on no clustering + 10% missing)
# --------------------------
final_target <- ceiling(baseline_n * 1.10)
cat("\nFinal recommended target (baseline 180 + 10% missing):", final_target, "(≈200)\n")

# --------------------------
# 8) Quick summary outputs you may want to paste into protocol
# --------------------------
cat("\nSummary:\n")
cat("- Pilot 30/30 exact 95% CI lower bound:", round(p1_from_pilot,2), "-> used as conservative p1.\n")
cat(sprintf("- Example param: p0=%.2f, p1=%.2f, alpha=%.2f, power=%.2f\n", p0, p1, alpha, power))
cat("- One-sample formula and pwr approach both yield very small n when p1 is large vs p0 (this is expected).\n")
cat("- For subgroup descriptives (30 per skin-type level) baseline n=180; with 10% missing -> ~198 (~200).\n")

# End of script

