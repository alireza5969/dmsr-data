# =============================================================================
# 90_protocol_functions.r
# Core estimators for the DMSR protocol (rank-based rule-out calibration).
#
# Status: PROTOCOL CODE. Written to accompany the protocol so that every
# formula in the Methods has an executable counterpart. Tested in R 4.3.3 on
# simulated (sham) data only; it has not seen real study data.
#
# Conventions
#   * One row per (review, record) in `records`, with columns
#       review_id  : character, the systematic review (unit of analysis)
#       record_id  : character, PubMed PMID of the retrieved record
#       included   : logical, TRUE if the record is in the review's final
#                    included list (reference standard)
#       score      : numeric in [0, 1], the model's *inclusion score*
#                    (see make_inclusion_score()); higher = more likely included
#   * Everything is computed per model. Run once per model (or per combination).
#   * "Sensitivity" is used in the DTA sense (= recall = TPR).
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

# -----------------------------------------------------------------------------
# 1. Inclusion score from (decision, certainty)
# -----------------------------------------------------------------------------
# The model returns a decision ("include"/"exclude") and a certainty in its
# *chosen* decision. Orient it so that higher always means "more likely include".
make_inclusion_score <- function(decision, certainty) {
  decision <- tolower(trimws(decision))
  stopifnot(all(decision %in% c("include", "exclude")))
  stopifnot(all(certainty >= 0 & certainty <= 1, na.rm = TRUE))
  ifelse(decision == "include", certainty, 1 - certainty)
}

# -----------------------------------------------------------------------------
# 2. Per-review quantities: B_j, N_j, n_j, U_j, m_j
# -----------------------------------------------------------------------------
# B_j = number of records whose score is STRICTLY below the lowest score of any
#       included record (ties with that record are NOT counted as excludable).
# U_j = B_j / N_j  (the oracle exclusion proportion; descriptive upper bound)
# m_j = lowest inclusion score among included records (for the score-based rule)
review_quantities <- function(records) {
  records %>%
    group_by(review_id) %>%
    summarise(
      N = n(),
      n_incl = sum(included),
      m = if (any(included)) min(score[included]) else NA_real_,
      B = if (any(included)) sum(score < min(score[included])) else NA_integer_,
      .groups = "drop"
    ) %>%
    mutate(U = B / N)
}

# -----------------------------------------------------------------------------
# 3. Order-statistic index k for the two guarantee types
# -----------------------------------------------------------------------------
# alpha = tolerated probability that a new review loses >= 1 included study
#         (0.05 for "95% assurance", 0.01 for "99% assurance").
# delta = tolerated probability that the calibration set itself is unlucky
#         (only for the tolerance / "95/95" guarantee).
k_marginal <- function(n, alpha) floor(alpha * (n + 1))

k_tolerance <- function(n, alpha, delta) {
  # largest k with P(Binomial(n, alpha) >= k) >= 1 - delta  (Wilks)
  ks <- 0:n
  ok <- pbinom(ks - 1, n, alpha, lower.tail = FALSE) >= 1 - delta
  if (!any(ok[-1])) return(0L)          # k = 0: no valid nonzero rule
  max(ks[ok])
}

min_n_tolerance <- function(k, alpha, delta) {
  n <- k
  while (pbinom(k - 1, n, alpha, lower.tail = FALSE) < 1 - delta) n <- n + 1
  n
}

# -----------------------------------------------------------------------------
# 4. Calibrated rules
# -----------------------------------------------------------------------------
# Rank-based: q_hat = k-th smallest U_j. A new review j' then excludes its
#             floor(q_hat * N_j') lowest-ranked records.
# Score-based: t_hat = k-th smallest m_j. A new review excludes records with
#             score < t_hat (strict).
# If k = 0 the calibration set is too small for a nonzero rule: return 0
# (exclude nothing), which is always safe.
calibrate_rule <- function(rq, alpha = 0.05, delta = 0.05,
                           guarantee = c("tolerance", "marginal")) {
  guarantee <- match.arg(guarantee)
  n <- nrow(rq)
  k <- if (guarantee == "marginal") k_marginal(n, alpha) else
    k_tolerance(n, alpha, delta)
  tibble(
    guarantee = guarantee, alpha = alpha,
    delta = if (guarantee == "tolerance") delta else NA_real_,
    n_cal = n, k = k,
    q_hat = if (k >= 1) sort(rq$U)[k] else 0,
    t_hat = if (k >= 1) sort(rq$m)[k] else -Inf,
    # for the marginal rule: the realised coverage of THIS calibration set is
    # Beta(n - k + 1, k); report P(coverage < 1 - 2*alpha) as a reliability check
    p_cov_below = if (k >= 1) pbeta(1 - 2 * alpha, n - k + 1, k) else NA_real_
  )
}

# -----------------------------------------------------------------------------
# 5. Apply a rule to (held-out) reviews: record- and review-level outcomes
# -----------------------------------------------------------------------------
# Ties in the ranking are broken at random with a fixed seed. Because B_j is
# defined with a strict inequality, safety (no loss) does not depend on how
# ties are broken whenever floor(q * N_j) <= B_j.
apply_rule <- function(records, q = NULL, t = NULL, seed = 20261118) {
  stopifnot(xor(is.null(q), is.null(t)))
  set.seed(seed)
  records %>%
    group_by(review_id) %>%
    mutate(
      tiebreak = runif(n()),
      rank_low = order(order(score, tiebreak)),            # 1 = lowest score
      excluded = if (!is.null(q)) rank_low <= floor(q * n()) else score < t
    ) %>%
    ungroup() %>%
    select(-tiebreak, -rank_low)
}

review_outcomes <- function(applied) {
  applied %>%
    group_by(review_id) %>%
    summarise(
      N = n(),
      n_incl = sum(included),
      TP = sum(included & !excluded),   # included and retained
      FN = sum(included & excluded),    # included but discarded (the miss)
      TN = sum(!included & excluded),   # excluded and discarded (work saved)
      FP = sum(!included & !excluded),  # excluded but retained (human reads it)
      .groups = "drop"
    ) %>%
    mutate(
      no_loss = FN == 0,
      sens = TP / (TP + FN),
      spec = TN / (TN + FP),
      prop_excluded = (TN + FN) / N
    )
}

# -----------------------------------------------------------------------------
# 6. Summary measures (micro and macro) and LR-
# -----------------------------------------------------------------------------
summarise_outcomes <- function(ro) {
  tibble(
    n_reviews = nrow(ro),
    no_loss_rate = mean(ro$no_loss),
    n_missed = sum(ro$FN),
    n_included = sum(ro$n_incl),
    sens_macro = mean(ro$sens),
    sens_micro = sum(ro$TP) / sum(ro$TP + ro$FN),
    spec_micro = sum(ro$TN) / sum(ro$TN + ro$FP),
    prop_excl_macro = mean(ro$prop_excluded),
    prop_excl_micro = sum(ro$TN + ro$FN) / sum(ro$N),
    lr_neg_micro = (1 - sens_micro) / spec_micro,
    false_omission_micro = sum(ro$FN) / sum(ro$TN + ro$FN),
    ppv_micro = sum(ro$TP) / sum(ro$TP + ro$FP),
    nnr_micro = 1 / ppv_micro                # number needed to read
  )
}

# -----------------------------------------------------------------------------
# 7. Exact one-sided lower bound (Clopper-Pearson) for a review-level rate
# -----------------------------------------------------------------------------
# Reviews are independent units, so the exact binomial bound applies directly
# to the review-level no-loss rate (x safe reviews out of n).
cp_lower <- function(x, n, conf = 0.95) ifelse(x == 0, 0, qbeta(1 - conf, x, n - x + 1))
cp_two_sided <- function(x, n, conf = 0.95) {
  a <- 1 - conf
  c(lower = ifelse(x == 0, 0, qbeta(a / 2, x, n - x + 1)),
    upper = ifelse(x == n, 1, qbeta(1 - a / 2, x + 1, n - x)))
}

# -----------------------------------------------------------------------------
# 8. Cluster (review-level) bootstrap
# -----------------------------------------------------------------------------
# Resamples whole reviews with replacement; never individual records.
# `stat_fun` receives a data frame of review-level rows and returns a number.
cluster_boot <- function(review_df, stat_fun, B = 2000, seed = 20261118,
                         conf = 0.95) {
  set.seed(seed)
  n <- nrow(review_df)
  est <- stat_fun(review_df)
  reps <- vapply(seq_len(B), function(b) {
    stat_fun(review_df[sample.int(n, n, replace = TRUE), , drop = FALSE])
  }, numeric(1))
  a <- (1 - conf) / 2
  tibble(estimate = est,
         lower = unname(quantile(reps, a, na.rm = TRUE)),
         upper = unname(quantile(reps, 1 - a, na.rm = TRUE)),
         B = B)
}

# Paired cluster bootstrap for the difference in q_hat between two models.
# rq_a, rq_b: review_quantities() for models A and B on the SAME reviews.
paired_q_diff_boot <- function(rq_a, rq_b, alpha = 0.05, delta = 0.05,
                               guarantee = "tolerance", B = 2000,
                               seed = 20261118) {
  stopifnot(identical(sort(rq_a$review_id), sort(rq_b$review_id)))
  rq_b <- rq_b[match(rq_a$review_id, rq_b$review_id), ]
  q_of <- function(rq) calibrate_rule(rq, alpha, delta, guarantee)$q_hat
  set.seed(seed)
  n <- nrow(rq_a)
  est <- q_of(rq_a) - q_of(rq_b)
  reps <- replicate(B, {
    i <- sample.int(n, n, replace = TRUE)
    q_of(rq_a[i, ]) - q_of(rq_b[i, ])
  })
  tibble(diff = est, lower = unname(quantile(reps, 0.025)),
         upper = unname(quantile(reps, 0.975)), B = B)
}

# -----------------------------------------------------------------------------
# 9. Simulated (sham) data generator, for illustration and testing ONLY
# -----------------------------------------------------------------------------
# Produces reviews whose included records tend to score higher. NOT a model of
# any real System 1 model; parameters are arbitrary.
simulate_reviews <- function(n_reviews = 200, mean_records = 600,
                             mean_included = 12, shift = 2.5,
                             seed = 20261118) {
  set.seed(seed)
  map_dfr(seq_len(n_reviews), function(j) {
    N <- max(20, rpois(1, mean_records))
    n_inc <- min(N - 1, max(1, rpois(1, mean_included)))
    review_effect <- rnorm(1, 0, 0.7)       # induces within-review correlation
    lin <- c(rnorm(n_inc, shift + review_effect, 1.2), rnorm(N - n_inc, 0, 1))
    tibble(review_id = sprintf("R%04d", j),
           record_id = sprintf("R%04d_%05d", j, seq_len(N)),
           included = c(rep(TRUE, n_inc), rep(FALSE, N - n_inc)),
           score = round(plogis(lin - 2), 2))  # rounding creates ties on purpose
  })
}
