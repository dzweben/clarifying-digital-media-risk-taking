# Study 1: Mediation (Table 9 / S1) ----------------------------------------
# Inputs:
#   - outputs/study1_clean.rds
# Outputs:
#   - outputs/study1_table9_mediation.csv

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

set.seed(2025)

med_df <- df %>%
  dplyr::select(mtes, reward_sensitivity, impulsivity, exploration, risk_variety) %>%
  na.omit()

# Impulsivity
med_model_imp <- lm(risk_variety ~ impulsivity, data = med_df)
out_model_imp <- lm(mtes ~ impulsivity + risk_variety, data = med_df)
med_imp <- mediate(med_model_imp, out_model_imp,
                   treat = "impulsivity", mediator = "risk_variety",
                   boot = TRUE, sims = 5000)

# Reward sensitivity
med_model_rs <- lm(risk_variety ~ reward_sensitivity, data = med_df)
out_model_rs <- lm(mtes ~ reward_sensitivity + risk_variety, data = med_df)
med_rs <- mediate(med_model_rs, out_model_rs,
                  treat = "reward_sensitivity", mediator = "risk_variety",
                  boot = TRUE, sims = 5000)

# Exploration
med_model_exp <- lm(risk_variety ~ exploration, data = med_df)
out_model_exp <- lm(mtes ~ exploration + risk_variety, data = med_df)
med_exp <- mediate(med_model_exp, out_model_exp,
                   treat = "exploration", mediator = "risk_variety",
                   boot = TRUE, sims = 5000)

# Format output
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

write_csv(results, file.path(output_dir, "study1_table9_mediation.csv"))

cat("Study 1 mediation results written to outputs/.\n")
