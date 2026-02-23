# Study 2: Data Preparation -----------------------------------------------
# Inputs:
#   - data/study2_analysis_data.csv
#   - data/raw/yrbs.csv
# Outputs:
#   - outputs/study2_clean.csv
#   - outputs/study2_clean.rds
#   - outputs/study2_outlier_summary.csv

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

main_path <- file.path(data_dir, "study2_analysis_data.csv")
if (!file.exists(main_path)) stop("Missing data file: ", main_path)

df_raw <- readr::read_csv(main_path, show_col_types = FALSE)

# Compute MTES z-score composite
mtes_items <- c(
  "MTES_SocialMediaUse_TimeOnApp.sum",
  "MTES_PhoneChecking.mean",
  "MTES_PublicUpdates.mean"
)

df_raw <- df_raw %>%
  mutate(across(all_of(mtes_items), ~ zscore(.x), .names = "{.col}.z")) %>%
  mutate(mtes_zscore = rowMeans(across(ends_with(".z")), na.rm = TRUE))

# Compute YRBS high-stakes negative risk variety (yrbs_var)
yrbs_path <- file.path(data_dir, "raw", "yrbs.csv")
if (!file.exists(yrbs_path)) stop("Missing YRBS file: ", yrbs_path)

yrbs <- readr::read_csv(yrbs_path, show_col_types = FALSE)

if ("pid_assignment" %in% names(yrbs)) {
  yrbs <- yrbs %>% rename(PID = pid_assignment)
}

yrbs <- yrbs %>%
  mutate(
    PID = gsub("\\..*$", "", as.character(PID)),
    PID = as.numeric(PID)
  )

# Match scoring used in previous scripts
alc <- ifelse(!is.na(yrbs$yrbs_alcfirst) & yrbs$yrbs_alcfirst != 1, 1, 0)
cig <- ifelse(yrbs$yrbs_cig_try == 1, 1, 0)
vape <- ifelse(yrbs$yrbs_vape_use == 1, 1, 0)
mj <- ifelse(!is.na(yrbs$yrbs_mjuse) & yrbs$yrbs_mjuse != 1, 1, 0)
rx <- ifelse(!is.na(yrbs$yrbs_rxuse) & yrbs$yrbs_rxuse != 1, 1, 0)
inh <- ifelse(!is.na(yrbs$yrbs_inhalants) & yrbs$yrbs_inhalants != 1, 1, 0)

# Column name can vary between exports (yrbs_cocain vs yrbs_cocaine)
if ("yrbs_cocain" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocain) & yrbs$yrbs_cocain != 1, 1, 0)
} else if ("yrbs_cocaine" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocaine) & yrbs$yrbs_cocaine != 1, 1, 0)
} else {
  stop("Missing YRBS cocaine variable in yrbs.csv")
}

yrbs <- yrbs %>%
  mutate(yrbs_var = rowMeans(data.frame(alc, cig, vape, mj, rx, inh, cocaine), na.rm = TRUE))

# Merge yrbs_var into main dataset
df_raw <- df_raw %>% left_join(yrbs %>% dplyr::select(PID, yrbs_var), by = "PID")

# Reverse foraging (higher = more exploration)
df_raw <- df_raw %>% mutate(foraging = as.numeric(foraging))
df_raw <- df_raw %>%
  mutate(
    foraging = ifelse(foraging == 0, NA, foraging),
    foraging = -1 * foraging
  )

# Standardize column names used in analysis scripts
# (Keeping original values; renaming for clarity/consistency)
df <- df_raw %>%
  rename(
    pid = PID,
    gender = sex,
    impulsivity = barrets.sum,
    reward_sensitivity = zuckerman.mean,
    exploration = foraging,
    social_use = SocialUsage,
    parasocial_use = ParasocialUsage,
    risk_low_pos = positiverisk_ever_mean,
    risk_low_neg = negativerisk_ever_mean,
    risk_high_neg = yrbs_var
  ) %>%
  mutate(
    risk_variety_raw = rowMeans(dplyr::select(., risk_low_neg, risk_low_pos, risk_high_neg), na.rm = TRUE),
    risk_variety = zscore(risk_variety_raw)
  )

analysis_vars <- c(
  "mtes_zscore",
  "impulsivity",
  "reward_sensitivity",
  "exploration",
  "risk_variety",
  "risk_high_neg",
  "risk_low_neg",
  "risk_low_pos",
  "social_use",
  "parasocial_use"
)

# Outlier removal: replace values with NA if |z| > 3 (per variable)
df_clean <- df
for (v in analysis_vars) {
  df_clean[[v]] <- remove_outliers(df_clean[[v]])
}

# Outlier removal summary
removed_cells <- sum(sapply(analysis_vars, function(v) {
  sum(!is.na(df[[v]]) & is.na(df_clean[[v]]))
}))

total_cells <- sum(sapply(analysis_vars, function(v) sum(!is.na(df[[v]]))))

outlier_summary <- data.frame(
  removed_cells = removed_cells,
  total_cells = total_cells,
  percent_removed = ifelse(total_cells == 0, NA_real_, (removed_cells / total_cells) * 100)
)

write_csv(outlier_summary, file.path(output_dir, "study2_outlier_summary.csv"))
write_csv(df_clean, file.path(output_dir, "study2_clean.csv"))
saveRDS(df_clean, file.path(output_dir, "study2_clean.rds"))

cat("Study 2 data prep complete.\n")
cat("Outliers removed:", removed_cells, "of", total_cells, "cells (",
    round(outlier_summary$percent_removed, 3), "%).\n")
