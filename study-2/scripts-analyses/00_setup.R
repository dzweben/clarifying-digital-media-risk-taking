# Study 2: Setup -----------------------------------------------------------
# Purpose: Shared setup and helper functions for Study 2 analysis scripts.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
  library(psych)
  library(Hmisc)
  library(cocor)
  library(mediation)
  library(lm.beta)
  library(ShapleyValue)
})

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
root_dir <- normalizePath(file.path(script_dir, ".."), mustWork = FALSE)

data_dir <- file.path(root_dir, "data")
output_dir <- file.path(root_dir, "outputs")

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

zscore <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

remove_outliers <- function(x, cutoff = 3) {
  z <- zscore(x)
  ifelse(abs(z) > cutoff, NA, x)
}

format_p <- function(p) {
  if (is.na(p)) return(NA_character_)
  if (p < .001) return("< .001")
  sprintf("%.3f", p)
}

format_r <- function(r, p) {
  stars <- if (is.na(p)) "" else if (p < .001) "***" else if (p < .01) "**" else if (p < .05) "*" else ""
  sprintf("%.2f%s", r, stars)
}
