# =============================================================================
# 92_protocol_flow-diagram.r
# STARD/PRISMA-style flow diagram of reviews from the PubMed query to the
# analytic set, drawn with ggplot2 only (no extra packages).
#
# Fill `flow` with real counts as they become available. Until then every
# count is a placeholder string, so the figure can never show invented numbers.
# Known counts: n_fetched is written by 01_fetch-data_pubmed.r to
# notes/number_fetched.txt.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
})

OUT_DIR <- "../dmsr-protocol/content/images"
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

read_count <- function(path) {
  if (file.exists(path)) trimws(readLines(path, warn = FALSE)[1]) else NA_character_
}
ph <- function(x, label) ifelse(is.na(x), paste0("[", label, "]"), format(as.numeric(x), big.mark = ","))

# ---- Main boxes (top to bottom) and exclusion boxes (right) ----------------
flow <- tribble(
  ~step, ~main_label,                                                        ~n_main,                                  ~excl_label,
  1,  "PubMed records: 'systematic review'[Title], free full text,\n2021-01-01 to 2026-10-05", read_count("notes/number_fetched.txt"), NA,
  2,  "Candidate reviews after record cleaning",                             NA, "Book chapters; no PMCID, DOI or abstract;\nyear missing or < 2021; protocol titles",
  3,  "Randomly selected candidate reviews",                                 NA, "Not selected (outside feasible workload)",
  4,  "Full text obtained from PMC Open Access Subset",                       NA, "Not in PMC OA bucket; download or conversion error",
  5,  "Reviews after automated pre-screen (script 03)",                      NA, "No abstract or received date; non-research title;\nno 'PubMed/MEDLINE' in text; < 1,500 body words",
  6,  "Systematic reviews with a PubMed search",                              NA, "(a) Not a systematic review with a PubMed search",
  7,  "PubMed syntax extractable and reproducible",                           NA, "(b) Syntax missing, incomplete or not reproducible",
  8,  "Re-run record count within tolerance",                                NA, "(c) Re-run count outside tolerance [tau]",
  9,  "At least one included study retrievable",                              NA, "(d) No included study in the re-run record set",
  10, "ANALYTIC SET (frozen before model runs)",                             NA, "(e) Fewer than [n_min] retrievable included studies"
) %>%
  mutate(n_excl = NA,
         main_text = paste0(main_label, "\nn = ", ph(n_main, paste0("N", step))),
         excl_text = paste0(excl_label, "\nn = ", ph(n_excl, paste0("E", step))),
         y = -step * 1.25)
excl <- filter(flow, !is.na(excl_label)) %>% mutate(ye = y + 0.625)

split_boxes <- tibble(
  x = c(-1.6, 1.6), y = -11 * 1.25 - 0.4,
  text = c("Calibration (development) reviews\nn = [N_cal]",
           "Verification (held-out) reviews\nn = [N_ver]")
)

box_w <- 3.6; box_h <- 0.7
fig <- ggplot() +
  # arrows down the main column
  geom_segment(data = head(flow, -1),
               aes(x = 0, xend = 0, y = y - box_h / 2, yend = y - 1.25 + box_h / 2),
               arrow = arrow(length = unit(2, "mm"))) +
  # arrows to the exclusion boxes
  geom_segment(data = excl, aes(x = 0, xend = 4.2 - 0.05, y = ye, yend = ye),
               arrow = arrow(length = unit(2, "mm"))) +
  geom_tile(data = flow, aes(x = 0, y = y), width = box_w, height = box_h,
            fill = "white", colour = "black") +
  geom_text(data = flow, aes(x = 0, y = y, label = main_text), size = 2.6, lineheight = 0.9) +
  geom_tile(data = excl, aes(x = 6.2, y = ye), width = 4, height = 0.8,
            fill = "grey95", colour = "grey40") +
  geom_text(data = excl, aes(x = 6.2, y = ye, label = excl_text), size = 2.3, lineheight = 0.9) +
  # split
  geom_segment(aes(x = 0, xend = c(-1.6, 1.6), y = -10 * 1.25 - box_h / 2,
                   yend = -11 * 1.25 - 0.4 + box_h / 2),
               arrow = arrow(length = unit(2, "mm"))) +
  geom_tile(data = split_boxes, aes(x = x, y = y), width = 3, height = box_h,
            fill = "white", colour = "black") +
  geom_text(data = split_boxes, aes(x = x, y = y, label = text), size = 2.6, lineheight = 0.9) +
  coord_cartesian(xlim = c(-3.4, 8.4)) +
  theme_void() +
  labs(caption = "Placeholders in [brackets] are filled from the frozen dataset. Stratified random split by review (seed reported).")

ggsave(file.path(OUT_DIR, "fig_flow_diagram.png"), fig, width = 9, height = 12, dpi = 200, bg = "white")
message("Flow diagram written to ", normalizePath(OUT_DIR))
