# Study 2 -- Setup
#
# Sourced by every other script. Loads packages, resolves paths, and defines
# the handful of helpers the pipeline uses. Nothing here touches the data.
#
# COHORT 1: the only thing you should need to edit in this file is the
# cohort split at the bottom.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(psych)
  library(Hmisc)
  library(cocor)          # Steiger's z for dependent, overlapping correlations
  library(mediation)      # bootstrapped indirect effects
  library(lm.beta)        # standardized betas for the hierarchical models
  library(ShapleyValue)   # Shapley value regression
})

# Resolve the folder this script lives in, whether it was run via Rscript,
# source(), or the RStudio Run button. Everything downstream is relative to it,
# so the pipeline works regardless of your working directory.
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

root_dir   <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)
data_dir   <- file.path(root_dir, "data")
output_dir <- file.path(root_dir, "outputs")

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)


# Helpers ---------------------------------------------------------------------

zscore <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# Outlier handling: blank the CELL, keep the ROW.
#
# A participant with an implausible foraging score still contributes valid
# impulsivity and risk data, so dropping the whole row would throw away good
# observations and shift N differently in every table. Setting the single value
# to NA lets each analysis drop only what it actually needs.
#
# Applied per variable, within the full Study 2 sample (not within cohort).
remove_outliers <- function(x, cutoff = 3) {
  z <- zscore(x)
  ifelse(abs(z) > cutoff, NA, x)
}

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

# Study 2 tables use three star levels. (Study 1 additionally marked p < .10;
# Study 2 does not. Keep it this way or Table 5 won't match the paper.)
format_r <- function(r, p) {
  stars <- if (is.na(p)) "" else if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
  sprintf("%.2f%s", r, stars)
}


# Cohort split ----------------------------------------------------------------
#
# In the published sample, PID encoded age group: 2xxx = adolescents,
# 3xxx+ = young adults. Every script splits on these two functions rather
# than hardcoding the comparison, so cohort 1 only has to change it here.
#
# COHORT 1: if PIDs no longer carry age group, rewrite these to filter on an
# explicit age or group column instead, e.g.
#   is_young_adult <- function(d) d$participant_age >= 18
is_young_adult <- function(d) d$pid > 3000
is_adolescent  <- function(d) d$pid < 3000
