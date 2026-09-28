# Study 2 -- Shapley value regression  ->  reported in text (section 3.2.3)
#
# In:  outputs/study2_clean.rds
# Out: outputs/study2_shapley_adults.csv
#      outputs/study2_shapley_adolescents.csv
#
# Why this and not just the regression: the predictors are correlated, so
# ordinary betas depend on entry order and each predictor's apparent importance
# shifts with what else is in the model. Shapley averages each predictor's
# contribution to R2 across every possible ordering, which gives a decomposition
# that sums to the model R2 and does not depend on how you ordered Step 1-4.
#
# Read the output as SHARE OF EXPLAINED VARIANCE, not effect size. The published
# adolescent model explains 20.8% of variance in MTES; risk variety accounts for
# 54% of that 20.8%, not 54% of MTES.

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

run_shapley <- function(data, cohort_label) {
  # Uses risk_variety (the composite), not the three subtypes. Feeding in three
  # heavily overlapping risk terms would split one construct's contribution
  # across them and make every risk share look small.
  #
  # na.omit is listwise here by necessity -- Shapley refits the model over every
  # subset of predictors and needs one constant sample to compare them.
  shap_df <- data %>%
    dplyr::select(mtes_zscore, risk_variety, reward_sensitivity,
                  impulsivity, exploration, gender) %>%
    na.omit()

  shap_values <- shapleyvalue(
    y = shap_df$mtes_zscore,
    x = shap_df %>% dplyr::select(-mtes_zscore) %>% as.data.frame()
  )

  # Row 1 = raw Shapley value; row 2 = proportion of model R2, printed as percent.
  shap_out <- as.data.frame(shap_values)
  shap_out <- rbind(
    Shapley_Value = shap_out[1, ],
    Percent_R2    = round(as.numeric(shap_out[2, ]) * 100, 2)
  )

  write_csv(as.data.frame(shap_out),
            file.path(output_dir, paste0("study2_shapley_", cohort_label, ".csv")))
}

run_shapley(df %>% filter(is_adolescent(.)),  "adolescents")
run_shapley(df %>% filter(is_young_adult(.)), "adults")

cat("Shapley results written to outputs/.\n")

# Published values to check against:
#   Young adults  model R2 = 12.9%  | impulsivity 50.3, risk variety 34.1,
#                                     sex 9.5, exploration 3.7, reward sens 2.4
#   Adolescents   model R2 = 20.8%  | risk variety 54.0, sex 36.9,
#                                     impulsivity 4.9, reward sens 4.0, exploration 3.7
