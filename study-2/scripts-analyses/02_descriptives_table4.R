# Study 2: Descriptives (Table 4) ------------------------------------------
# Inputs:
#   - data/study2_analysis_data.csv
#   - data/raw/yrbs.csv
#   - data/raw/bar.csv
# Outputs:
#   - outputs/study2_table4_descriptives.csv
# Notes:
#   - Cohorts are labeled using PID > 3000 = Young Adults and PID < 3000 = Adolescents.
#   - Impulsivity uses the mean of BAR items (1-4 scale) to match manuscript descriptives.

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

# ---- YRBS high-stakes negative risk variety ----
source_yrbs <- file.path(data_dir, "raw", "yrbs.csv")
if (!file.exists(source_yrbs)) stop("Missing YRBS file: ", source_yrbs)

yrbs <- readr::read_csv(source_yrbs, show_col_types = FALSE)
if ("pid_assignment" %in% names(yrbs)) {
  yrbs <- yrbs %>% rename(PID = pid_assignment)
}

yrbs <- yrbs %>%
  mutate(
    PID = gsub("\\..*$", "", as.character(PID)),
    PID = as.numeric(PID)
  )

alc <- ifelse(!is.na(yrbs$yrbs_alcfirst) & yrbs$yrbs_alcfirst != 1, 1, 0)
cig <- ifelse(yrbs$yrbs_cig_try == 1, 1, 0)
vape <- ifelse(yrbs$yrbs_vape_use == 1, 1, 0)
mj <- ifelse(!is.na(yrbs$yrbs_mjuse) & yrbs$yrbs_mjuse != 1, 1, 0)
rx <- ifelse(!is.na(yrbs$yrbs_rxuse) & yrbs$yrbs_rxuse != 1, 1, 0)
inh <- ifelse(!is.na(yrbs$yrbs_inhalants) & yrbs$yrbs_inhalants != 1, 1, 0)

if ("yrbs_cocain" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocain) & yrbs$yrbs_cocain != 1, 1, 0)
} else if ("yrbs_cocaine" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocaine) & yrbs$yrbs_cocaine != 1, 1, 0)
} else {
  stop("Missing YRBS cocaine variable in yrbs.csv")
}

yrbs <- yrbs %>%
  mutate(yrbs_var = rowMeans(data.frame(alc, cig, vape, mj, rx, inh, cocaine), na.rm = TRUE))

# ---- Merge derived variables ----
# ---- BAR (Impulsivity mean) ----
bar_path <- file.path(data_dir, "raw", "bar.csv")
if (!file.exists(bar_path)) stop("Missing bar file: ", bar_path)

bar <- readr::read_csv(bar_path, show_col_types = FALSE)
reverse_items <- c("bar_dothings", "bar_attention", "bar_saythings", "bar_act")
max_val <- 4
bar <- bar %>%
  mutate(across(all_of(reverse_items), ~ (max_val + 1) - ., .names = "rev_{col}"))

scored_items <- c(
  "bar_plantasks",
  "rev_bar_dothings",
  "rev_bar_attention",
  "bar_selfcontrol",
  "bar_concentrate",
  "bar_careful",
  "rev_bar_saythings",
  "rev_bar_act"
)

bar <- bar %>%
  mutate(barrets_mean = rowMeans(across(all_of(scored_items)), na.rm = TRUE))

# ---- Merge derived variables ----
merged <- df_raw %>%
  left_join(yrbs %>% dplyr::select(PID, yrbs_var), by = "PID") %>%
  left_join(bar %>% dplyr::select(PID, barrets_mean), by = "PID")

# Total risk percent (weighted by item counts)
merged <- merged %>%
  mutate(
    total_risk_pct = (
      negativerisk_ever_mean * 7 +
      positiverisk_ever_mean * 14 +
      yrbs_var * 7
    ) / (7 + 14 + 7) * 100
  )

# Helper for descriptive stats
summarise_var <- function(x) {
  data.frame(
    Mean = round(mean(x, na.rm = TRUE), 2),
    SD   = round(sd(x, na.rm = TRUE), 2),
    Min  = round(min(x, na.rm = TRUE), 2),
    Max  = round(max(x, na.rm = TRUE), 2)
  )
}

# Ensure numeric types for summary variables
numeric_vars <- c(
  "MTES_SocialMediaUse_TimeOnApp.sum",
  "MTES_PublicUpdates.mean",
  "MTES_PhoneChecking.mean",
  "SocialUsage",
  "ParasocialUsage",
  "barrets_mean",
  "zuckerman.mean",
  "total_risk_pct",
  "yrbs_var",
  "negativerisk_ever_mean",
  "positiverisk_ever_mean",
  "foraging"
)
merged <- merged %>% mutate(across(all_of(numeric_vars), as.numeric))

# Variables to report
vars <- c(
  "MTES_SocialMediaUse_TimeOnApp.sum",
  "MTES_PublicUpdates.mean",
  "MTES_PhoneChecking.mean",
  "SocialUsage",
  "ParasocialUsage",
  "barrets_mean",
  "zuckerman.mean",
  "total_risk_pct",
  "yrbs_var",
  "negativerisk_ever_mean",
  "positiverisk_ever_mean",
  "foraging"
)

# Manuscript-aligned cohorts
cohorts <- list(
  `Young Adults` = merged %>% filter(PID > 3000),
  `Adolescents` = merged %>% filter(PID < 3000)
)

rows <- list()
for (cohort_name in names(cohorts)) {
  dfc <- cohorts[[cohort_name]]
  for (v in vars) {
    if (!v %in% names(dfc)) next
    values <- dfc[[v]]

    # Convert proportion-based variables to percent
    if (v %in% c("yrbs_var", "negativerisk_ever_mean", "positiverisk_ever_mean")) {
      values <- values * 100
    }

    stats <- summarise_var(values)
    rows[[length(rows) + 1]] <- data.frame(
      Cohort = cohort_name,
      Variable = v,
      stats,
      stringsAsFactors = FALSE
    )
  }
}

desc_table <- bind_rows(rows)
write_csv(desc_table, file.path(output_dir, "study2_table4_descriptives.csv"))

cat("Study 2 descriptives written to outputs/study2_table4_descriptives.csv\n")
