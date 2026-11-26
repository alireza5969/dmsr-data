# =============================================================================
# 93_protocol_sample-size.r
# Distribution-free planning quantities for DMSR, plus a simulation template
# that will be re-run once pilot data exist.
#
# Parts A-D need NO pilot data: they follow from order statistics and binomial
# probabilities. Part E needs the pilot distribution of U_j; until then it runs
# on an INVENTED Beta(1.6, 0.9) distribution and its output is illustrative only.
# =============================================================================

source("R/90_protocol_functions.r")
suppressPackageStartupMessages(library(tidyr))

# ---- A. Minimum calibration reviews for the tolerance (Wilks) guarantee ----
wilks_table <- function(ks, a, d) {
  n_min <- vapply(ks, min_n_tolerance, numeric(1), alpha = a, delta = d)
  tibble(k = ks, alpha = a, delta = d, n_min = n_min)
}
tab_A <- bind_rows(
  wilks_table(1:5, 0.05, 0.05),   # 95/95
  wilks_table(1:5, 0.01, 0.01),   # 99/99
  wilks_table(1:5, 0.01, 0.05)    # 99% assurance, 95% confidence
)
print(tab_A, n = Inf)

# ---- B. Reliability of the marginal guarantee -----------------------------
tab_B <- tibble(n = c(19, 59, 99, 199, 399, 999)) %>%
  mutate(k = floor(0.05 * (n + 1)),
         mean_coverage = 1 - k / (n + 1),
         p_cov_below_90 = pbeta(0.90, n - k + 1, k),
         p_cov_below_95 = pbeta(0.95, n - k + 1, k))
print(tab_B)

# ---- C. Zero observed misses: included records needed to claim sensitivity >= s
tab_C <- expand_grid(conf = c(0.95, 0.99), s = c(0.95, 0.98, 0.99)) %>%
  mutate(n_included_needed = ceiling(log(1 - conf) / log(s)))
print(tab_C)

# ---- D. Verification set: exact one-sided lower bounds ---------------------
tab_D <- tibble(x = c(190, 380, 388, 396), n = c(200, 400, 400, 400)) %>%
  mutate(rate = x / n, cp_lower_95 = cp_lower(x, n, 0.95), cp_lower_99 = cp_lower(x, n, 0.99))
print(tab_D)

# ---- D2. Record-level sensitivity with clustering (design effect) ----------
# m_bar and rho are PLACEHOLDERS until the pilot (example values only).
deff_n <- function(sens, half_width, m_bar, rho, conf = 0.95) {
  z <- qnorm(1 - (1 - conf) / 2)
  n_srs <- z^2 * sens * (1 - sens) / half_width^2
  deff <- 1 + (m_bar - 1) * rho
  tibble(sens, half_width, m_bar, rho, deff, n_included = ceiling(n_srs * deff),
         n_reviews = ceiling(n_srs * deff / m_bar))
}
print(bind_rows(deff_n(0.98, 0.01, 15, 0.05), deff_n(0.95, 0.02, 15, 0.05)))

# ---- D3. McNemar (paired no-loss at fixed q), Connor (1987) approximation --
mcnemar_n <- function(diff, discord, alpha = 0.05, power = 0.80) {
  za <- qnorm(1 - alpha / 2); zb <- qnorm(power)
  ceiling((za * sqrt(discord) + zb * sqrt(discord - diff^2))^2 / diff^2)
}
print(tibble(diff = c(0.10, 0.05), discord = c(0.20, 0.15)) %>%
        mutate(n_reviews = mcnemar_n(diff, discord)))

# ---- E. Precision of q-hat by simulation (REPLACE r_U WITH PILOT DATA) -----
# r_U: a function(n) returning n draws of U. After the pilot, use e.g. the
# empirical pilot U values: r_U <- function(n) sample(pilot_U, n, replace = TRUE)
r_U <- function(n) rbeta(n, 1.6, 0.9)   # INVENTED, illustrative only
sim_qhat_sd <- function(n, alpha = 0.05, guarantee = "marginal", delta = 0.05,
                        reps = 4000, seed = 20261118) {
  set.seed(seed)
  k <- if (guarantee == "marginal") k_marginal(n, alpha) else k_tolerance(n, alpha, delta)
  q <- replicate(reps, sort(r_U(n))[k])
  tibble(n = n, guarantee = guarantee, k = k, mean_qhat = mean(q), sd_qhat = sd(q))
}
tab_E <- map_dfr(c(59, 100, 200, 400, 800, 1600), sim_qhat_sd)
print(tab_E)

# ---- F. Compute budget -------------------------------------------------------
# records_per_review and decisions_per_min are PLACEHOLDERS (prompt figures).
compute_days <- function(n_reviews, records_per_review = 600, decisions_per_min = 100,
                         n_models = 1) {
  n_reviews * records_per_review * n_models / decisions_per_min / 60 / 24
}
print(tibble(n_reviews = c(1000, 2000, 5000, 10000)) %>%
        mutate(days_per_model = compute_days(n_reviews)))

# ---- G. Reviews needed when strata must each meet the guarantee -------------
# The smallest stratum drives the total: n_total = n_min_stratum / share_smallest.
# share_smallest is a PLACEHOLDER until the frozen data are described.
needed_total <- function(n_min_stratum, share_smallest, p_cal) {
  ceiling(n_min_stratum / share_smallest / p_cal)
}
print(expand_grid(n_min_stratum = c(59, 459), share_smallest = c(0.2, 0.33), p_cal = 0.5) %>%
        mutate(analysable_reviews_needed = needed_total(n_min_stratum, share_smallest, p_cal)))
