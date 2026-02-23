# Study 1: Data Preparation -----------------------------------------------
# Inputs:
#   - data/study1_analysis_data.csv
# Outputs:
#   - outputs/study1_clean.csv
#   - outputs/study1_clean.rds
#   - outputs/study1_outlier_summary.csv

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

# Read analysis-ready dataset
raw_path <- file.path(data_dir, "study1_analysis_data.csv")
if (!file.exists(raw_path)) stop("Missing data file: ", raw_path)

df_raw <- readr::read_csv(raw_path, show_col_types = FALSE)

# Standardize column names used in analysis scripts
# (Keeping original values; renaming for clarity/consistency)
df <- df_raw %>%
  rename(
    mtes = MTES,
    impulsivity = impulsivity,
    reward_sensitivity = ss,
    exploration = exploration,
    risk_variety = `Risk Variety`,
    risk_low_pos = pnrt_pos,
    risk_low_neg = pnrt_neg,
    risk_high_neg = brp_hsn,
    gender = Gender,
    social_use = social_score,
    parasocial_use = parasocial_score
  )

analysis_vars <- c(
  "mtes",
  "impulsivity",
  "reward_sensitivity",
  "exploration",
  "risk_variety",
  "risk_low_pos",
  "risk_low_neg",
  "risk_high_neg",
  "social_use",
  "parasocial_use"
)

# Outlier removal: replace values with NA if |z| > 3 (per variable)
df_clean <- df
for (v in analysis_vars) {
  df_clean[[v]] <- remove_outliers(df_clean[[v]])
}

# Outlier removal summary
removed_cells <- sum(sapply(analysis_vars, function(v) {
  sum(!is.na(df[[v]]) & is.na(df_clean[[v]]))
}))

total_cells <- sum(sapply(analysis_vars, function(v) sum(!is.na(df[[v]]))))

outlier_summary <- data.frame(
  removed_cells = removed_cells,
  total_cells = total_cells,
  percent_removed = ifelse(total_cells == 0, NA_real_, (removed_cells / total_cells) * 100)
)

write_csv(outlier_summary, file.path(output_dir, "study1_outlier_summary.csv"))
write_csv(df_clean, file.path(output_dir, "study1_clean.csv"))
saveRDS(df_clean, file.path(output_dir, "study1_clean.rds"))

cat("Study 1 data prep complete.\n")
cat("Outliers removed:", removed_cells, "of", total_cells, "cells (",
    round(outlier_summary$percent_removed, 3), "%).\n")
