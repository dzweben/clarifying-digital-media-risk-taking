# Study 2 -- Indirect effects  ->  published Table S3 (supplement)
#
# In:  outputs/study2_clean.rds
# Out: outputs/study2_mediation_adults.csv
#      outputs/study2_mediation_adolescents.csv
#
# Model: cognitive trait -> risk variety -> SSMU, run separately for each trait
# and each cohort. Six mediations total.
#
# Direction matters and is worth stating plainly: risk taking is the MEDIATOR
# and SSMU is the OUTCOME. The data are cross-sectional, so this is a test of
# statistical accounting, not of causal sequence. The paper words these as
# "accounted for", never "led to" -- keep that.

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

# Bootstrap draws are random. Without this seed the CIs move slightly on every
# run and you will not reproduce the published values exactly.
set.seed(2025)

run_mediation_set <- function(data, cohort_label) {
  # Standardize AFTER subsetting and AFTER na.omit, so every variable is
  # z-scored within the exact sample the models are fit on. Standardizing
  # earlier (in 01_data_prep.R, across both cohorts) would put adolescents and
  # young adults on a shared scale and change the coefficients.
  #
  # This is also why risk_variety_raw is carried through data prep: the
  # already-z-scored risk_variety would be standardized against the wrong sample.
  med_df <- data %>%
    dplyr::select(mtes_zscore, risk_variety_raw, reward_sensitivity, impulsivity, exploration) %>%
    mutate(
      risk_variety_z       = zscore(risk_variety_raw),
      reward_sensitivity_z = zscore(reward_sensitivity),
      impulsivity_z        = zscore(impulsivity),
      exploration_z        = zscore(exploration)
    ) %>%
    na.omit()

  # Each mediation needs two models: predictor -> mediator, and
  # predictor + mediator -> outcome. Written out longhand rather than looped --
  # mediate() re-evaluates the model calls internally, and building the formulas
  # programmatically is a known way to break it.
  #
  # sims = 5000 resamples, as reported in the paper.

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

  # d0 = indirect (ACME), z0 = direct (ADE), n0 = proportion mediated.
  extract_results <- function(m, predictor_name) {
    data.frame(
      Predictor = predictor_name,
      Indirect  = sprintf("%.2f (%.2f, %.2f), %.3f", m$d0, m$d0.ci[1], m$d0.ci[2], m$d0.p),
      Direct    = sprintf("%.2f (%.2f, %.2f), %.2f", m$z0, m$z0.ci[1], m$z0.ci[2], m$z0.p),
      Prop      = sprintf("%.1f%%, %.2f", m$n0 * 100, m$n0.p),
      stringsAsFactors = FALSE
    )
  }

  results <- bind_rows(
    extract_results(med_imp, "impulsivity"),
    extract_results(med_rs,  "reward_sensitivity"),
    extract_results(med_exp, "exploration")
  )

  write_csv(results, file.path(output_dir, paste0("study2_mediation_", cohort_label, ".csv")))
}

run_mediation_set(df %>% filter(is_adolescent(.)),  "adolescents")
run_mediation_set(df %>% filter(is_young_adult(.)), "adults")

cat("Table S3 written to outputs/.\n")

# Proportion mediated exceeds 100% for reward sensitivity in both cohorts
# (112.4% young adults, 110.6% adolescents). That happens when the direct and
# indirect paths have opposite signs; it is not an error, and the paper reports
# it alongside a nonsignificant p for the proportion. Do not "fix" it.
#
# Slowest script in the pipeline -- six mediations x 5000 bootstraps.
