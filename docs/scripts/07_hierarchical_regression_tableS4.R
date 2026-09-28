# Study 2 -- Hierarchical regression  ->  published Table S4 (supplement)
#
# In:  outputs/study2_clean.rds
# Out: outputs/study2_table13_hierarchical_regression_adults.csv
#      outputs/study2_table14_hierarchical_regression_adolescents.csv
#
# The question this answers: once you account for who someone is
# dispositionally, does what they DO still explain anything about their social
# media engagement? Risk enters last, so its delta R2 is the test.
#
# Step 1  sex + impulsivity
# Step 2  + reward sensitivity
# Step 3  + exploration
# Step 4  + the three risk subtypes
#
# Sex is in from Step 1 as a covariate, not a predictor of interest. Entering
# the cognitive traits before risk is the conservative ordering -- it gives the
# dispositional account first claim on shared variance, so anything risk picks
# up in Step 4 is over and above traits.

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

build_table <- function(data, cohort_label, out_name) {
  model_vars <- c("mtes_zscore", "gender", "impulsivity", "reward_sensitivity",
                  "exploration", "risk_low_pos", "risk_low_neg", "risk_high_neg")

  # Listwise deletion ONCE, up front, so all four steps are fit on the same
  # participants. Without this the delta R2 comparisons would be across
  # different samples and the anova() below would be meaningless.
  d <- data %>% dplyr::select(dplyr::all_of(model_vars)) %>% na.omit()

  m1 <- lm(mtes_zscore ~ gender + impulsivity, data = d)
  m2 <- lm(mtes_zscore ~ gender + impulsivity + reward_sensitivity, data = d)
  m3 <- lm(mtes_zscore ~ gender + impulsivity + reward_sensitivity + exploration, data = d)
  m4 <- lm(mtes_zscore ~ gender + impulsivity + reward_sensitivity + exploration +
             risk_low_pos + risk_low_neg + risk_high_neg, data = d)

  models     <- list(m1, m2, m3, m4)
  models_std <- lapply(models, lm.beta)   # betas in the table are standardized
  anova_res  <- anova(m1, m2, m3, m4)     # step-to-step delta R2 tests

  format_beta <- function(beta, p) {
    stars <- if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else if (p < .10) "+" else ""
    sprintf("%.2f%s", beta, stars)
  }
  format_p_num <- function(p) if (p < .001) "< .001" else sprintf("%.3f", p)

  rows <- list()
  for (i in seq_along(models)) {
    mod <- models[[i]]; mod_std <- models_std[[i]]
    summ <- summary(mod); summ_std <- summary(mod_std)

    rows[[length(rows) + 1]] <- data.frame(Step = paste0("Step ", i), Predictor = "",
                                           Beta = "", SE = "", t = "", p = "", stringsAsFactors = FALSE)

    for (j in 2:nrow(summ$coefficients)) {          # start at 2 to skip the intercept
      rows[[length(rows) + 1]] <- data.frame(
        Step = "",
        Predictor = rownames(summ$coefficients)[j],
        Beta = format_beta(summ_std$coefficients[j, "Standardized"], summ$coefficients[j, "Pr(>|t|)"]),
        # SE and t come from the UNstandardized fit; only beta is standardized.
        SE = sprintf("%.2f", summ$coefficients[j, "Std. Error"]),
        t  = sprintf("%.2f", summ$coefficients[j, "t value"]),
        p  = format_p_num(summ$coefficients[j, "Pr(>|t|)"]),
        stringsAsFactors = FALSE
      )
    }

    r2 <- summ$r.squared
    fstat <- summ$fstatistic
    fval <- fstat[1]; df1 <- fstat[2]; df2 <- fstat[3]
    pval <- pf(fval, df1, df2, lower.tail = FALSE)

    delta_r2 <- ""
    if (i > 1) {
      delta   <- r2 - summary(models[[i - 1]])$r.squared
      delta_p <- anova_res$`Pr(>F)`[i]
      delta_r2 <- paste0(", Delta R2 = ", sprintf("%.3f", delta), ", p = ", format_p_num(delta_p))
    }

    rows[[length(rows) + 1]] <- data.frame(
      Step = "",
      Predictor = paste0("R2 = ", sprintf("%.3f", r2),
                         ", F(", df1, ", ", df2, ") = ", sprintf("%.2f", fval),
                         ", p = ", format_p_num(pval), delta_r2),
      Beta = "", SE = "", t = "", p = "", stringsAsFactors = FALSE
    )
  }

  write_csv(bind_rows(rows), file.path(output_dir, out_name))
}

build_table(df %>% filter(is_adolescent(.)),  "Adolescents",
            "study2_table14_hierarchical_regression_adolescents.csv")
build_table(df %>% filter(is_young_adult(.)), "Young Adults",
            "study2_table13_hierarchical_regression_adults.csv")

cat("Table S4 written to outputs/.\n")

# Published values to check against:
#   Young adults  final R2 = .147, adj .10, F(7, 127) = 3.13, p = .004
#                 Step 4 delta R2 = .044, p = .09   (risk does NOT add)
#   Adolescents   final R2 = .170, adj .11, F(7,  96) = 2.82, p = .010
#                 Step 4 delta R2 = .112, p = .025  (risk DOES add)
#
# ------------------------------------------------------------------
# If you want to structure Step 4 differently
# ------------------------------------------------------------------
# All three subtypes go in together. They are correlated (r = .25 to .55), so
# each beta is the unique contribution of that subtype net of the others, and
# none of them individually reaches significance in the young adult model even
# though the block as a whole is near threshold. Two defensible alternatives:
#
#   1. Composite in Step 4, subtypes in Step 5. Tests "does risk matter" before
#      "which kind of risk", and avoids attributing shared risk variance to any
#      one subtype. Costs you the direct comparison with the published table.
#
#   2. Three separate Step 4 models, one subtype each. Cleanest read on each
#      subtype, but the betas are no longer unique contributions and you are
#      running three tests where the paper ran one.
#
# Either is fine if stated. What is not fine is picking whichever ordering makes
# risk significant and reporting only that one.
#
# COHORT 1: without YRBS, drop risk_high_neg from model_vars and from m4. Step 4
# becomes a two-predictor block and the delta R2 is not comparable to the
# published one -- report it as its own result, not as a replication.
