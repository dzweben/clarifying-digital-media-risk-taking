# Study 2 -- run the whole pipeline
#
# Run this file, or run 01 through 08 in order. Each script re-runs
# 01_data_prep.R on its own if outputs/study2_clean.rds is missing, so they also
# work standalone.
#
# Expect a few minutes; 06 (mediation, 6 x 5000 bootstraps) dominates the time.

script_dir <- getwd()
.ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (!is.null(.ofile) && nzchar(.ofile)) script_dir <- dirname(normalizePath(.ofile))
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  .ctx <- rstudioapi::getActiveDocumentContext()
  if (nzchar(.ctx$path)) script_dir <- dirname(normalizePath(.ctx$path))
}
rm(.ofile)

source(file.path(script_dir, "01_data_prep.R"))
source(file.path(script_dir, "02_descriptives_table4.R"))
source(file.path(script_dir, "03_correlations_table5.R"))
source(file.path(script_dir, "04_risk_subtype_intercorrelations.R"))
source(file.path(script_dir, "05_shapley.R"))
source(file.path(script_dir, "06_mediation_tableS3.R"))
source(file.path(script_dir, "07_hierarchical_regression_tableS4.R"))
source(file.path(script_dir, "08_steiger_table6.R"))

cat("Study 2 run_all completed.\n")
