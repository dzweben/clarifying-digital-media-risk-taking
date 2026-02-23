# Study 2: Shapley Value Regression ----------------------------------------
# Inputs:
#   - outputs/study2_clean.rds
# Outputs:
#   - outputs/study2_shapley_adolescents.csv
#   - outputs/study2_shapley_adults.csv

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

run_shapley <- function(data, cohort_label) {
  shap_df <- data %>%
    dplyr::select(mtes_zscore, risk_variety, reward_sensitivity, impulsivity, exploration, gender) %>%
    na.omit()

  shap_values <- shapleyvalue(y = shap_df$mtes_zscore,
                              x = shap_df %>% dplyr::select(-mtes_zscore) %>% as.data.frame())

  shap_out <- as.data.frame(shap_values)
  shap_out <- rbind(
    Shapley_Value = shap_out[1, ],
    Percent_R2 = round(as.numeric(shap_out[2, ]) * 100, 2)
  )

  out_path <- file.path(output_dir, paste0("study2_shapley_", cohort_label, ".csv"))
  write_csv(as.data.frame(shap_out), out_path)
}

# Adolescents
run_shapley(df %>% filter(pid < 3000), "adolescents")
# Adults
run_shapley(df %>% filter(pid > 3000), "adults")

cat("Study 2 Shapley results written to outputs/.\n")
