# Study 1: Shapley Value Regression ----------------------------------------
# Inputs:
#   - outputs/study1_clean.rds
# Outputs:
#   - outputs/study1_shapley.csv

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

# Prepare data for Shapley
shap_df <- df %>%
  dplyr::select(mtes, risk_variety, reward_sensitivity, impulsivity, exploration, gender) %>%
  na.omit()

# Shapley value analysis
shap_values <- shapleyvalue(y = shap_df$mtes,
                            x = shap_df %>% dplyr::select(-mtes) %>% as.data.frame())

shap_out <- as.data.frame(shap_values)
shap_out <- rbind(
  Shapley_Value = shap_out[1, ],
  Percent_R2 = round(as.numeric(shap_out[2, ]) * 100, 2)
)

write_csv(as.data.frame(shap_out), file.path(output_dir, "study1_shapley.csv"))

cat("Study 1 Shapley results written to outputs/.\n")
