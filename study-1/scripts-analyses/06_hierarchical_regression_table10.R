# Study 1: Hierarchical Regression (Table 10 / S2) -------------------------
# Inputs:
#   - outputs/study1_clean.rds
# Outputs:
#   - outputs/study1_table10_hierarchical_regression.csv

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

model_vars <- c("mtes", "gender", "impulsivity", "reward_sensitivity",
                "exploration", "risk_low_pos", "risk_low_neg", "risk_high_neg")

df_model <- df %>% dplyr::select(dplyr::all_of(model_vars)) %>% na.omit()

m1 <- lm(mtes ~ gender + impulsivity, data = df_model)
m2 <- lm(mtes ~ gender + impulsivity + reward_sensitivity, data = df_model)
m3 <- lm(mtes ~ gender + impulsivity + reward_sensitivity + exploration, data = df_model)
m4 <- lm(mtes ~ gender + impulsivity + reward_sensitivity + exploration +
           risk_low_pos + risk_low_neg + risk_high_neg, data = df_model)

models <- list(m1, m2, m3, m4)
models_std <- lapply(models, lm.beta)

anova_res <- anova(m1, m2, m3, m4)

format_beta <- function(beta, p) {
  stars <- if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else if (p < .10) "+" else ""
  sprintf("%.2f%s", beta, stars)
}

format_p_num <- function(p) {
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

rows <- list()
for (i in seq_along(models)) {
  mod <- models[[i]]
  mod_std <- models_std[[i]]
  summ <- summary(mod)
  summ_std <- summary(mod_std)

  # Step header
  rows[[length(rows) + 1]] <- data.frame(
    Step = paste0("Step ", i),
    Predictor = "",
    Beta = "",
    SE = "",
    t = "",
    p = "",
    stringsAsFactors = FALSE
  )

  # Predictors (skip intercept)
  for (j in 2:nrow(summ$coefficients)) {
    pred <- rownames(summ$coefficients)[j]
    beta <- summ_std$coefficients[j, "Standardized"]
    se <- summ$coefficients[j, "Std. Error"]
    tval <- summ$coefficients[j, "t value"]
    pval <- summ$coefficients[j, "Pr(>|t|)"]

    rows[[length(rows) + 1]] <- data.frame(
      Step = "",
      Predictor = pred,
      Beta = format_beta(beta, pval),
      SE = sprintf("%.2f", se),
      t = sprintf("%.2f", tval),
      p = format_p_num(pval),
      stringsAsFactors = FALSE
    )
  }

  # R2 row
  r2 <- summ$r.squared
  fstat <- summ$fstatistic
  fval <- fstat[1]
  df1 <- fstat[2]
  df2 <- fstat[3]
  pval <- pf(fval, df1, df2, lower.tail = FALSE)

  delta_r2 <- ""
  if (i > 1) {
    prev_r2 <- summary(models[[i - 1]])$r.squared
    delta <- r2 - prev_r2
    delta_p <- anova_res$`Pr(>F)`[i]
    delta_r2 <- paste0(", Delta R2 = ", sprintf("%.3f", delta),
                       ", p = ", format_p_num(delta_p))
  }

  r2_text <- paste0("R2 = ", sprintf("%.3f", r2),
                    ", F(", df1, ", ", df2, ") = ", sprintf("%.2f", fval),
                    ", p = ", format_p_num(pval), delta_r2)

  rows[[length(rows) + 1]] <- data.frame(
    Step = "",
    Predictor = r2_text,
    Beta = "",
    SE = "",
    t = "",
    p = "",
    stringsAsFactors = FALSE
  )
}

out <- bind_rows(rows)
write_csv(out, file.path(output_dir, "study1_table10_hierarchical_regression.csv"))

cat("Study 1 hierarchical regression table written to outputs/.\n")
