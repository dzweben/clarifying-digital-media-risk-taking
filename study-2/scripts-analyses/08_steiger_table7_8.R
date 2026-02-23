# Study 2: Steiger Tests (Tables 7 and 8) ----------------------------------
# Inputs:
#   - outputs/study2_clean.rds
# Outputs:
#   - outputs/study2_table7_steiger_adults.csv
#   - outputs/study2_table8_steiger_teens.csv

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

clean_path <- file.path(output_dir, "study2_clean.rds")
if (!file.exists(clean_path)) {
  source(file.path(script_dir, "01_data_prep.R"))
}

df <- readRDS(clean_path)

predictors <- c("risk_high_neg", "risk_low_neg", "risk_low_pos")

run_steiger <- function(data) {
  rows <- list()
  for (pred in predictors) {
    r_social <- cor(data[[pred]], data$social_use, use = "pairwise.complete.obs")
    r_parasocial <- cor(data[[pred]], data$parasocial_use, use = "pairwise.complete.obs")
    r_social_parasocial <- cor(data$social_use, data$parasocial_use, use = "pairwise.complete.obs")
    n <- nrow(na.omit(data[, c(pred, "social_use", "parasocial_use")]))

    test <- cocor.dep.groups.overlap(r.jk = r_social,
                                    r.jh = r_parasocial,
                                    r.kh = r_social_parasocial,
                                    n = n)
    steiger <- test@steiger1980

    rows[[length(rows) + 1]] <- data.frame(
      Risk_Subtype = pred,
      r_social = round(r_social, 2),
      r_parasocial = round(r_parasocial, 2),
      steiger_z = round(-1 * steiger$statistic, 2),
      p_value = round(steiger$p.value, 3),
      n = n
    )
  }
  bind_rows(rows)
}

# Adults
adults <- df %>% filter(pid > 3000)
res_adults <- run_steiger(adults)
write_csv(res_adults, file.path(output_dir, "study2_table7_steiger_adults.csv"))

# Adolescents
teens <- df %>% filter(pid < 3000)
res_teens <- run_steiger(teens)
write_csv(res_teens, file.path(output_dir, "study2_table8_steiger_teens.csv"))

cat("Study 2 Steiger tables written to outputs/.\n")
