# Study 1: Steiger Tests (Table 3) -----------------------------------------
# Inputs:
#   - outputs/study1_clean.rds
# Outputs:
#   - outputs/study1_table3_steiger.csv

script_dir <- getwd()
.ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (!is.null(.ofile) && nzchar(.ofile)) {
  script_dir <- dirname(normalizePath(.ofile))
}
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  .ctx <- rstudioapi::getActiveDocumentContext()
  if (nzchar(.ctx$path)) {
    script_dir <- dirname(normalizePath(.ctx$path))
  }
}
rm(.ofile)
source(file.path(script_dir, "00_setup.R"))

clean_path <- file.path(output_dir, "study1_clean.rds")
if (!file.exists(clean_path)) {
  source(file.path(script_dir, "01_data_prep.R"))
}

df <- readRDS(clean_path)

predictors <- c("risk_high_neg", "risk_low_neg", "risk_low_pos")

run_steiger <- function(predictor) {
  r_social <- cor(df[[predictor]], df$social_use, use = "pairwise.complete.obs")
  r_parasocial <- cor(df[[predictor]], df$parasocial_use, use = "pairwise.complete.obs")
  r_social_parasocial <- cor(df$social_use, df$parasocial_use, use = "pairwise.complete.obs")
  n <- nrow(na.omit(df[, c(predictor, "social_use", "parasocial_use")]))

  test <- cocor.dep.groups.overlap(r.jk = r_social,
                                  r.jh = r_parasocial,
                                  r.kh = r_social_parasocial,
                                  n = n)
  steiger <- test@steiger1980

  data.frame(
    Risk_Subtype = predictor,
    r_social = round(r_social, 2),
    r_parasocial = round(r_parasocial, 2),
    steiger_z = round(-1 * steiger$statistic, 2),
    p_value = round(steiger$p.value, 3),
    n = n
  )
}

results <- bind_rows(lapply(predictors, run_steiger))
write_csv(results, file.path(output_dir, "study1_table3_steiger.csv"))

cat("Study 1 Table 3 Steiger results written to outputs/.\n")
