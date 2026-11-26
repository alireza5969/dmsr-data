# =============================================================================
# 91_protocol_illustrations.r
# Figures for the protocol. Panels marked "simulated" use SHAM data from
# simulate_reviews() and illustrate the METHOD only; they are not results.
# Panels marked "exact" are pure probability calculations (no data).
#
# Run from the dmsr-data root. Output goes to the protocol's content/images/
# (path set in OUT_DIR below; change if your folders differ).
# =============================================================================

source("R/90_protocol_functions.r")
suppressPackageStartupMessages({
  library(ggplot2)
  library(tidyr)
})

OUT_DIR <- "../dmsr-protocol/content/images"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

theme_set(theme_minimal(base_size = 11) +
            theme(plot.title.position = "plot",
                  panel.grid.minor = element_blank()))

# ---- Figure 1 (exact): why record-level sensitivity is not review-level safety
# If misses were independent, P(no loss) = sens^n_incl. They are not independent
# in practice, so this is a didactic bound only.
fig1 <- expand_grid(sens = c(0.95, 0.98, 0.99, 0.995), n_incl = 1:80) %>%
  mutate(p_no_loss = sens^n_incl,
         sens_lab = factor(scales::percent(sens, accuracy = 0.1),
                           levels = scales::percent(c(0.95, 0.98, 0.99, 0.995), accuracy = 0.1))) %>%
  ggplot(aes(n_incl, p_no_loss, colour = sens_lab)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 25, linetype = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  labs(title = "Probability that a review loses no included study",
       subtitle = "Exact calculation assuming independent misses (didactic only). Dotted line: 25 included studies.",
       x = "Number of included studies in the review",
       y = "P(no included study lost)", colour = "Record-level\nsensitivity")
ggsave(file.path(OUT_DIR, "fig_independence.png"), fig1, width = 7, height = 4, dpi = 200, bg = "white")

# ---- Figure 2 (simulated): distribution of U_j and the calibrated cut-offs
d <- simulate_reviews(n_reviews = 300)
rq <- review_quantities(d)
cuts <- bind_rows(
  calibrate_rule(rq, 0.05, guarantee = "marginal") %>% mutate(lab = "Marginal 95%"),
  calibrate_rule(rq, 0.05, 0.05, "tolerance") %>% mutate(lab = "Tolerance 95/95")
)
fig2 <- ggplot(rq, aes(U)) +
  geom_histogram(binwidth = 0.025, boundary = 0, fill = "grey70", colour = "white") +
  geom_vline(data = cuts, aes(xintercept = q_hat, linetype = lab), linewidth = 0.8) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "SIMULATED: per-review oracle exclusion proportion U_j (300 sham reviews)",
       subtitle = "Each U_j is what could have been excluded without loss in that review; q-hat is a low order statistic of U_j",
       x = "U_j (proportion of records ranked strictly below the lowest-ranked included record)",
       y = "Number of reviews", linetype = "Calibrated q-hat")
ggsave(file.path(OUT_DIR, "fig_U_distribution_sham.png"), fig2, width = 7, height = 4, dpi = 200, bg = "white")

# ---- Figure 3 (simulated): sensitivity versus proportion excluded across q
curve <- map_dfr(seq(0, 0.9, by = 0.02), function(q) {
  ro <- review_outcomes(apply_rule(d, q = q))
  s <- summarise_outcomes(ro)
  tibble(q = q, `Record-level sensitivity (macro)` = s$sens_macro,
         `Review-level no-loss rate` = s$no_loss_rate)
}) %>% pivot_longer(-q)
fig3 <- ggplot(curve, aes(q, value, colour = name)) +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "SIMULATED: safety against proportion of records excluded",
       subtitle = "Review-level no-loss falls much faster than record-level sensitivity",
       x = "Proportion of each review's records excluded (q)", y = NULL, colour = NULL) +
  theme(legend.position = "bottom")
ggsave(file.path(OUT_DIR, "fig_sens_vs_excluded_sham.png"), fig3, width = 7, height = 4, dpi = 200, bg = "white")

# ---- Figure 4 (exact): reliability of the marginal guarantee
# Coverage of one particular calibration set ~ Beta(n - k + 1, k), k = floor(0.05(n+1)).
fig4 <- expand_grid(n = c(19, 59, 199, 399), cov = seq(0.80, 1, by = 0.001)) %>%
  mutate(k = floor(0.05 * (n + 1)),
         dens = dbeta(cov, n - k + 1, k),
         n_lab = factor(paste0("n = ", n), levels = paste0("n = ", c(19, 59, 199, 399)))) %>%
  ggplot(aes(cov, dens, colour = n_lab)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 0.95, linetype = 3) +
  scale_x_continuous(labels = scales::percent) +
  labs(title = "Exact: realised coverage of the marginal 95% rule for one calibration set",
       subtitle = "Mean coverage is >= 95% for every n, but small calibration sets are often unlucky",
       x = "Coverage (probability a new review loses nothing)", y = "Density",
       colour = "Calibration\nreviews")
ggsave(file.path(OUT_DIR, "fig_marginal_coverage.png"), fig4, width = 7, height = 4, dpi = 200, bg = "white")

# ---- Table (exact): the worked 5,000-record example
worked <- tibble(
  strategy = c("Reject everything", "Discard 80% at random", "Good model (hypothetical)"),
  TP = c(0, 5, 24), FN = c(25, 20, 1), FP = c(0, 995, 976), TN = c(4975, 3980, 3999)
) %>% mutate(
  sensitivity = TP / (TP + FN),
  specificity = TN / (TN + FP),
  NPV = TN / (TN + FN),
  accuracy = (TP + TN) / 5000,
  prop_excluded = (TN + FN) / 5000
)
print(worked, width = Inf)
message("Figures written to ", normalizePath(OUT_DIR))
