# Study 1: Descriptives (Table 1) ------------------------------------------
# Inputs:
#   - outputs/study1_clean.rds
# Outputs:
#   - outputs/study1_table1_descriptives.csv
# Notes:
#   - MTES subscale scores (Social Media Use, Public Updating, Checking)
#     and foraging metrics are not present in the analysis dataset. This
#     script will report NA for those rows unless the variables are added.

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

# Helper for descriptive stats
summarise_var <- function(x) {
  data.frame(
    Mean = round(mean(x, na.rm = TRUE), 2),
    SD   = round(sd(x, na.rm = TRUE), 2),
    Min  = round(min(x, na.rm = TRUE), 2),
    Max  = round(max(x, na.rm = TRUE), 2)
  )
}

# Define expected variables (set to NA if missing)
vars <- list(
  mtes_social_media_use = "mtes_social_media_use",
  mtes_public_updating  = "mtes_public_updating",
  mtes_checking         = "mtes_checking",
  social_use            = "social_use",
  parasocial_use        = "parasocial_use",
  impulsivity           = "impulsivity",
  reward_sensitivity    = "reward_sensitivity",
  risk_variety          = "risk_variety",
  risk_high_neg         = "risk_high_neg",
  risk_low_neg          = "risk_low_neg",
  risk_low_pos          = "risk_low_pos",
  map_percentage        = "map_percentage",
  unique_patches        = "unique_patches",
  outer_sector_ratio    = "outer_sector_ratio"
)

rows <- lapply(names(vars), function(label) {
  var_name <- vars[[label]]
  if (var_name %in% names(df)) {
    stats <- summarise_var(df[[var_name]])
  } else {
    stats <- data.frame(Mean = NA, SD = NA, Min = NA, Max = NA)
  }
  data.frame(
    Metric = label,
    stats,
    stringsAsFactors = FALSE
  )
})

desc_table <- bind_rows(rows)
write_csv(desc_table, file.path(output_dir, "study1_table1_descriptives.csv"))

cat("Study 1 descriptives written to outputs/study1_table1_descriptives.csv\n")
