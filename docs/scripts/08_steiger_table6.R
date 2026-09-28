# Study 2 -- Social vs parasocial comparison  ->  published Table 6
#
# In:  outputs/study2_clean.rds
# Out: outputs/study2_table7_steiger_adults.csv   (Panel A, Young Adults)
#      outputs/study2_table8_steiger_teens.csv    (Panel B, Adolescents)
#
# The argument this script carries: if the SSMU-risk link were just screen time,
# it would not matter what kind of platform the time was spent on. It does. Risk
# tracks reciprocal platforms (Instagram, Snapchat, Facebook) and not one-sided
# ones (YouTube, TikTok, Pinterest, Twitter).
#
# Steiger's z is the right test here because the two correlations being compared
# share a variable (the risk subtype) and come from the same people. A plain
# comparison of two r values would ignore that dependency and overstate the
# difference.

script_dir <- getwd()
.ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (!is.null(.ofile) && nzchar(.ofile)) script_dir <- dirname(normalizePath(.ofile))
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  .ctx <- rstudioapi::getActiveDocumentContext()
  if (nzchar(.ctx$path)) script_dir <- dirname(normalizePath(.ctx$path))
}
rm(.ofile)
source(file.path(script_dir, "00_setup.R"))

clean_path <- file.path(output_dir, "study2_clean.rds")
if (!file.exists(clean_path)) source(file.path(script_dir, "01_data_prep.R"))
df <- readRDS(clean_path)

predictors <- c("risk_high_neg", "risk_low_neg", "risk_low_pos")

run_steiger <- function(data) {
  rows <- list()
  for (pred in predictors) {
    r_social            <- cor(data[[pred]],       data$social_use,     use = "pairwise.complete.obs")
    r_parasocial        <- cor(data[[pred]],       data$parasocial_use, use = "pairwise.complete.obs")
    # The third correlation is what makes the test "dependent" -- social and
    # parasocial use are themselves correlated, and the test needs to know that.
    r_social_parasocial <- cor(data$social_use,    data$parasocial_use, use = "pairwise.complete.obs")
    n <- nrow(na.omit(data[, c(pred, "social_use", "parasocial_use")]))

    test <- cocor.dep.groups.overlap(
      r.jk = r_social,             # risk x social
      r.jh = r_parasocial,         # risk x parasocial
      r.kh = r_social_parasocial,  # social x parasocial
      n    = n
    )
    steiger <- test@steiger1980

    rows[[length(rows) + 1]] <- data.frame(
      Risk_Subtype = pred,
      r_social     = round(r_social, 2),
      r_parasocial = round(r_parasocial, 2),
      # Sign is flipped so a NEGATIVE z means social > parasocial, matching the
      # direction printed in Table 6. cocor orders the comparison the other way.
      steiger_z    = round(-1 * steiger$statistic, 2),
      p_value      = round(steiger$p.value, 3),
      n            = n
    )
  }
  bind_rows(rows)
}

write_csv(run_steiger(df %>% filter(is_young_adult(.))),
          file.path(output_dir, "study2_table7_steiger_adults.csv"))
write_csv(run_steiger(df %>% filter(is_adolescent(.))),
          file.path(output_dir, "study2_table8_steiger_teens.csv"))

cat("Table 6 (both panels) written to outputs/.\n")

# Published values to check against:
#   Young adults  high-stakes neg  .27 / .01  z = -2.21  p = .027
#                 low-stakes neg   .27 / .11  z = -1.35  p = .178
#                 low-stakes pos   .21 / -.06 z = -2.18  p = .029
#   Adolescents   high-stakes neg  .17 / .04  z = -1.29  p = .198
#                 low-stakes neg   .31 / .10  z = -2.05  p = .041
#                 low-stakes pos   .28 / .00  z = -2.62  p = .009
#
# COHORT 1: without YRBS, drop "risk_high_neg" from predictors above. The
# low-stakes rows are the ones doing the work anyway.
