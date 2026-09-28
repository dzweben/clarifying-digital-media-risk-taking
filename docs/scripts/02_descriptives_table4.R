# Study 2 -- Descriptives  ->  published Table 4
#
# In:  data/study2_analysis_data.csv, data/raw/yrbs.csv, data/raw/bar.csv
# Out: outputs/study2_table4_descriptives.csv
#
# This script deliberately does NOT read study2_clean.rds. Table 4 reports
# measures on their original scales, so it re-derives what it needs from the
# raw files and skips the outlier screen. Everything after this script uses the
# cleaned data.

script_dir <- getwd()
.ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (!is.null(.ofile) && nzchar(.ofile)) script_dir <- dirname(normalizePath(.ofile))
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  .ctx <- rstudioapi::getActiveDocumentContext()
  if (nzchar(.ctx$path)) script_dir <- dirname(normalizePath(.ctx$path))
}
rm(.ofile)
source(file.path(script_dir, "00_setup.R"))

df_raw <- readr::read_csv(file.path(data_dir, "study2_analysis_data.csv"), show_col_types = FALSE)

# --- YRBS high-stakes score (same derivation as 01_data_prep.R) --------------
yrbs <- readr::read_csv(file.path(data_dir, "raw", "yrbs.csv"), show_col_types = FALSE)
if ("pid_assignment" %in% names(yrbs)) yrbs <- yrbs %>% rename(PID = pid_assignment)
yrbs <- yrbs %>% mutate(PID = as.numeric(gsub("\\..*$", "", as.character(PID))))

alc  <- ifelse(!is.na(yrbs$yrbs_alcfirst)  & yrbs$yrbs_alcfirst  != 1, 1, 0)
cig  <- ifelse(yrbs$yrbs_cig_try  == 1, 1, 0)
vape <- ifelse(yrbs$yrbs_vape_use == 1, 1, 0)
mj   <- ifelse(!is.na(yrbs$yrbs_mjuse)     & yrbs$yrbs_mjuse     != 1, 1, 0)
rx   <- ifelse(!is.na(yrbs$yrbs_rxuse)     & yrbs$yrbs_rxuse     != 1, 1, 0)
inh  <- ifelse(!is.na(yrbs$yrbs_inhalants) & yrbs$yrbs_inhalants != 1, 1, 0)
if ("yrbs_cocain" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocain) & yrbs$yrbs_cocain != 1, 1, 0)
} else if ("yrbs_cocaine" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocaine) & yrbs$yrbs_cocaine != 1, 1, 0)
} else stop("Missing YRBS cocaine variable in yrbs.csv")

yrbs <- yrbs %>%
  mutate(yrbs_var = rowMeans(data.frame(alc, cig, vape, mj, rx, inh, cocaine), na.rm = TRUE))

# --- Impulsivity on its original 1-4 scale -----------------------------------
#
# The models use barrets.sum. Table 4 reports the MEAN (M = 2.88 / 2.87), so the
# BIS is rescored here from items. Four items are worded in the low-impulsivity
# direction and are reversed (5 - x on a 1-4 scale) before averaging.
bar <- readr::read_csv(file.path(data_dir, "raw", "bar.csv"), show_col_types = FALSE)
reverse_items <- c("bar_dothings", "bar_attention", "bar_saythings", "bar_act")
max_val <- 4
bar <- bar %>% mutate(across(all_of(reverse_items), ~ (max_val + 1) - ., .names = "rev_{col}"))

scored_items <- c("bar_plantasks", "rev_bar_dothings", "rev_bar_attention",
                  "bar_selfcontrol", "bar_concentrate", "bar_careful",
                  "rev_bar_saythings", "rev_bar_act")

bar <- bar %>% mutate(barrets_mean = rowMeans(across(all_of(scored_items)), na.rm = TRUE))

merged <- df_raw %>%
  left_join(yrbs %>% dplyr::select(PID, yrbs_var),     by = "PID") %>%
  left_join(bar  %>% dplyr::select(PID, barrets_mean), by = "PID")

# --- Total risk variety, as a percentage -------------------------------------
#
# The three subtype scores are proportions over different numbers of items, so
# the total is weighted by item count (7 negative, 14 positive, 7 YRBS) rather
# than a plain mean of the three. This is why Table 4's "Risk Taking, Total
# Variety" (53.5% / 66.56%) is not the mean of the three rows beneath it.
#
# COHORT 1: without YRBS, drop the yrbs term AND its weight from the
# denominator -- do not leave 7 in it or the percentage is understated.
merged <- merged %>%
  mutate(
    total_risk_pct = (negativerisk_ever_mean * 7 +
                      positiverisk_ever_mean * 14 +
                      yrbs_var * 7) / (7 + 14 + 7) * 100
  )

summarise_var <- function(x) {
  data.frame(
    Mean = round(mean(x, na.rm = TRUE), 2),
    SD   = round(sd(x,   na.rm = TRUE), 2),
    Min  = round(min(x,  na.rm = TRUE), 2),
    Max  = round(max(x,  na.rm = TRUE), 2)
  )
}

# Row order matches Table 4 top to bottom.
vars <- c(
  "MTES_SocialMediaUse_TimeOnApp.sum",  # Social Media Use
  "MTES_PublicUpdates.mean",            # Public Updating
  "MTES_PhoneChecking.mean",            # Checking
  "SocialUsage",                        # Social Use
  "ParasocialUsage",                    # Parasocial Use
  "barrets_mean",                       # Impulsivity
  "zuckerman.mean",                     # Reward Sensitivity
  "total_risk_pct",                     # Risk Taking, Total Variety
  "yrbs_var",                           # High-Stakes Negative Risk
  "negativerisk_ever_mean",             # Low-Stakes Negative Risk
  "positiverisk_ever_mean",             # Low-Stakes Positive Risk
  "foraging"                            # Exploration (Apples Per Tree)
)
merged <- merged %>% mutate(across(all_of(vars), as.numeric))

cohorts <- list(
  `Young Adults` = merged %>% filter(PID > 3000),
  `Adolescents`  = merged %>% filter(PID < 3000)
)

rows <- list()
for (cohort_name in names(cohorts)) {
  dfc <- cohorts[[cohort_name]]
  for (v in vars) {
    if (!v %in% names(dfc)) next
    values <- dfc[[v]]
    # The three subtype scores are proportions; Table 4 prints them as percents.
    if (v %in% c("yrbs_var", "negativerisk_ever_mean", "positiverisk_ever_mean")) {
      values <- values * 100
    }
    rows[[length(rows) + 1]] <- data.frame(
      Cohort = cohort_name, Variable = v, summarise_var(values), stringsAsFactors = FALSE
    )
  }
}

write_csv(bind_rows(rows), file.path(output_dir, "study2_table4_descriptives.csv"))
cat("Table 4 written to outputs/study2_table4_descriptives.csv\n")

# Note: foraging is reported here on its raw apples-per-tree scale (M = 43.3 /
# 45.4), before the sign flip applied in 01_data_prep.R. Table 4 describes the
# task; the models use the flipped version.
