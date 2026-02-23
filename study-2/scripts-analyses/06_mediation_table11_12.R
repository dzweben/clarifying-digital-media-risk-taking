# Study 2: Mediation (Tables 11 and 12 / S3) -------------------------------
# Inputs:
#   - outputs/study2_clean.rds
# Outputs:
#   - outputs/study2_mediation_adolescents.csv
#   - outputs/study2_mediation_adults.csv

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
set.seed(2025)

run_mediation_set <- function(data, cohort_label) {
  med_df <- data %>%
    dplyr::select(mtes_zscore, risk_variety_raw, reward_sensitivity, impulsivity, exploration) %>%
    mutate(
      risk_variety_z = zscore(risk_variety_raw),
      reward_sensitivity_z = zscore(reward_sensitivity),
      impulsivity_z = zscore(impulsivity),
      exploration_z = zscore(exploration)
    ) %>%
    na.omit()

  # Impulsivity
  med_model_imp <- lm(risk_variety_z ~ impulsivity_z, data = med_df)
  out_model_imp <- lm(mtes_zscore ~ impulsivity_z + risk_variety_z, data = med_df)
  med_imp <- mediate(med_model_imp, out_model_imp,
                     treat = "impulsivity_z", mediator = "risk_variety_z",
                     boot = TRUE, sims = 5000)

  # Reward sensitivity
  med_model_rs <- lm(risk_variety_z ~ reward_sensitivity_z, data = med_df)
  out_model_rs <- lm(mtes_zscore ~ reward_sensitivity_z + risk_variety_z, data = med_df)
  med_rs <- mediate(med_model_rs, out_model_rs,
                    treat = "reward_sensitivity_z", mediator = "risk_variety_z",
                    boot = TRUE, sims = 5000)

  # Exploration
  med_model_exp <- lm(risk_variety_z ~ exploration_z, data = med_df)
  out_model_exp <- lm(mtes_zscore ~ exploration_z + risk_variety_z, data = med_df)
  med_exp <- mediate(med_model_exp, out_model_exp,
                     treat = "exploration_z", mediator = "risk_variety_z",
                     boot = TRUE, sims = 5000)

  extract_results <- function(med_obj, predictor_name) {
    data.frame(
      Predictor = predictor_name,
      Indirect = sprintf("%.2f (%.2f, %.2f), %.3f", med_obj$d0, med_obj$d0.ci[1], med_obj$d0.ci[2], med_obj$d0.p),
      Direct   = sprintf("%.2f (%.2f, %.2f), %.2f", med_obj$z0, med_obj$z0.ci[1], med_obj$z0.ci[2], med_obj$z0.p),
      Prop     = sprintf("%.1f%%, %.2f", med_obj$n0 * 100, med_obj$n0.p),
      stringsAsFactors = FALSE
    )
  }

  results <- bind_rows(
    extract_results(med_imp, "impulsivity"),
    extract_results(med_rs, "reward_sensitivity"),
    extract_results(med_exp, "exploration")
  )

  out_path <- file.path(output_dir, paste0("study2_mediation_", cohort_label, ".csv"))
  write_csv(results, out_path)
}

# Adolescents (<3000)
run_mediation_set(df %>% filter(pid < 3000), "adolescents")

# Adults (>3000)
run_mediation_set(df %>% filter(pid > 3000), "adults")

cat("Study 2 mediation results written to outputs/.\n")
