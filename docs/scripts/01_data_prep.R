# Study 2 -- Data preparation
#
# In:  data/study2_analysis_data.csv   (one row per participant, scored measures)
#      data/raw/yrbs.csv               (item-level YRBS, scored here)
# Out: outputs/study2_clean.rds        (used by every analysis script)
#      outputs/study2_clean.csv
#      outputs/study2_outlier_summary.csv
#
# This is the only script that builds variables. Everything after it just runs
# models on what comes out of here.

script_dir <- getwd()
.ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
if (!is.null(.ofile) && nzchar(.ofile)) {
  script_dir <- dirname(normalizePath(.ofile))
}
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  .ctx <- rstudioapi::getActiveDocumentContext()
  if (nzchar(.ctx$path)) script_dir <- dirname(normalizePath(.ctx$path))
}
rm(.ofile)
source(file.path(script_dir, "00_setup.R"))

main_path <- file.path(data_dir, "study2_analysis_data.csv")
if (!file.exists(main_path)) stop("Missing data file: ", main_path)
df_raw <- readr::read_csv(main_path, show_col_types = FALSE)


# 1. SSMU composite (MTES) ----------------------------------------------------
#
# The outcome for the whole paper. Three MTES subscales on different scales:
# TimeOnApp is a SUM across platforms, the other two are item MEANS. Averaging
# them raw would let TimeOnApp dominate, so each is z-scored first and the
# composite is the mean of the three z-scores.
#
# Because it is z-scored within sample, this variable is relative to whoever is
# in the file. Cohort 1's composite is not on the same absolute scale as the
# published one -- compare patterns of association, not means.
#
# COHORT 1 -- participants without a phone: TimeOnApp is a sum, so someone who
# selects nothing sums to 0 and lands at the bottom of the distribution, which
# scores "no phone" as "lightest user." Decide explicitly whether those cases
# are a real floor or should be NA before running this, and check with Jason --
# Lina and Jason have almost certainly already set a rule for cohort 1.
mtes_items <- c(
  "MTES_SocialMediaUse_TimeOnApp.sum",
  "MTES_PhoneChecking.mean",
  "MTES_PublicUpdates.mean"
)

df_raw <- df_raw %>%
  mutate(across(all_of(mtes_items), ~ zscore(.x), .names = "{.col}.z")) %>%
  mutate(mtes_zscore = rowMeans(across(ends_with(".z")), na.rm = TRUE))


# 2. High-stakes negative risk (YRBS) -----------------------------------------
#
# Scored here from items rather than read in pre-scored. Seven substance
# behaviours, each collapsed to ever/never, then averaged -- so the score is
# "proportion of high-stakes behaviours ever engaged in", matching the variety
# logic used for the PNRT subtypes below.
#
# Response coding is not uniform across items, which is why the recodes differ:
#   - alcfirst, mjuse, rxuse, inhalants, cocaine: 1 = never, anything else = yes
#   - cig_try, vape_use: 1 = yes
#
# COHORT 1: cohort 1 does not have YRBS substance scoring. That means
# risk_high_neg does not exist, and it is a load-bearing variable -- it is a
# row in Table 5, a row in Table 6, a term in Step 4 of Table S4, and one of
# three components of risk_variety. See the walkthrough for what to do; do not
# just let it fall through as NA, because risk_variety below uses na.rm = TRUE
# and will silently become a two-component composite.
yrbs_path <- file.path(data_dir, "raw", "yrbs.csv")
if (!file.exists(yrbs_path)) stop("Missing YRBS file: ", yrbs_path)
yrbs <- readr::read_csv(yrbs_path, show_col_types = FALSE)

if ("pid_assignment" %in% names(yrbs)) yrbs <- yrbs %>% rename(PID = pid_assignment)

# PIDs come out of REDCap as "2002.1" (pid.visit); strip to the integer ID.
yrbs <- yrbs %>%
  mutate(
    PID = gsub("\\..*$", "", as.character(PID)),
    PID = as.numeric(PID)
  )

alc  <- ifelse(!is.na(yrbs$yrbs_alcfirst)  & yrbs$yrbs_alcfirst  != 1, 1, 0)
cig  <- ifelse(yrbs$yrbs_cig_try  == 1, 1, 0)
vape <- ifelse(yrbs$yrbs_vape_use == 1, 1, 0)
mj   <- ifelse(!is.na(yrbs$yrbs_mjuse)     & yrbs$yrbs_mjuse     != 1, 1, 0)
rx   <- ifelse(!is.na(yrbs$yrbs_rxuse)     & yrbs$yrbs_rxuse     != 1, 1, 0)
inh  <- ifelse(!is.na(yrbs$yrbs_inhalants) & yrbs$yrbs_inhalants != 1, 1, 0)

# Spelling of the cocaine column varies between REDCap exports.
if ("yrbs_cocain" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocain) & yrbs$yrbs_cocain != 1, 1, 0)
} else if ("yrbs_cocaine" %in% names(yrbs)) {
  cocaine <- ifelse(!is.na(yrbs$yrbs_cocaine) & yrbs$yrbs_cocaine != 1, 1, 0)
} else {
  stop("Missing YRBS cocaine variable in yrbs.csv")
}

yrbs <- yrbs %>%
  mutate(yrbs_var = rowMeans(data.frame(alc, cig, vape, mj, rx, inh, cocaine), na.rm = TRUE))

df_raw <- df_raw %>% left_join(yrbs %>% dplyr::select(PID, yrbs_var), by = "PID")


# 3. Exploration --------------------------------------------------------------
#
# Foraging is apples-per-tree: staying longer on a patch = exploiting. Higher
# raw values therefore mean LESS exploration, so it is sign-flipped to make the
# variable read in the same direction as its name everywhere downstream.
#
# Zeros are treated as missing rather than as a true floor -- a participant who
# harvested nothing did not complete the task.
df_raw <- df_raw %>%
  mutate(
    foraging = as.numeric(foraging),
    foraging = ifelse(foraging == 0, NA, foraging),
    foraging = -1 * foraging
  )


# 4. Rename to analysis names -------------------------------------------------
#
# Values are unchanged; this is only so the model scripts read in construct
# names rather than instrument names.
#
# Where each one comes from:
#   impulsivity        BIS / Barratt, sum of the 8 scored items
#   reward_sensitivity Zuckerman sensation-seeking subset (6 items), mean
#   risk_low_pos       PNRT positive, 14 items       -- EVER score, see below
#   risk_low_neg       PNRT negative, 7 items        -- EVER score, see below
#   risk_high_neg      YRBS, 7 substance items       -- scored above
#   social_use         reciprocal platforms (Instagram, Snapchat, Facebook)
#   parasocial_use     one-sided platforms (YouTube, TikTok, Pinterest, Twitter)
df <- df_raw %>%
  rename(
    pid                = PID,
    gender             = sex,
    impulsivity        = barrets.sum,
    reward_sensitivity = zuckerman.mean,
    exploration        = foraging,
    social_use         = SocialUsage,
    parasocial_use     = ParasocialUsage,
    risk_low_pos       = positiverisk_ever_mean,
    risk_low_neg       = negativerisk_ever_mean,
    risk_high_neg      = yrbs_var
  ) %>%
  mutate(
    # Total risk variety: mean of the three subtype proportions, then z-scored.
    # Keep the raw version -- the mediation script standardizes within its own
    # complete-case subsample and needs the unstandardized values.
    risk_variety_raw = rowMeans(dplyr::select(., risk_low_neg, risk_low_pos, risk_high_neg), na.rm = TRUE),
    risk_variety     = zscore(risk_variety_raw)
  )


# 5. Outlier screen -----------------------------------------------------------
#
# |z| > 3, per variable, cell-wise (see remove_outliers in 00_setup.R).
analysis_vars <- c(
  "mtes_zscore", "impulsivity", "reward_sensitivity", "exploration",
  "risk_variety", "risk_high_neg", "risk_low_neg", "risk_low_pos",
  "social_use", "parasocial_use"
)

df_clean <- df
for (v in analysis_vars) df_clean[[v]] <- remove_outliers(df_clean[[v]])

removed_cells <- sum(sapply(analysis_vars, function(v) sum(!is.na(df[[v]]) & is.na(df_clean[[v]]))))
total_cells   <- sum(sapply(analysis_vars, function(v) sum(!is.na(df[[v]]))))

outlier_summary <- data.frame(
  removed_cells   = removed_cells,
  total_cells     = total_cells,
  percent_removed = ifelse(total_cells == 0, NA_real_, (removed_cells / total_cells) * 100)
)

write_csv(outlier_summary, file.path(output_dir, "study2_outlier_summary.csv"))
write_csv(df_clean,        file.path(output_dir, "study2_clean.csv"))
saveRDS(df_clean,          file.path(output_dir, "study2_clean.rds"))

cat("Study 2 data prep complete.\n")
cat("Outliers removed:", removed_cells, "of", total_cells, "cells (",
    round(outlier_summary$percent_removed, 3), "%).\n")


# ---------------------------------------------------------------------------
# PNRT ever scores -- reference
# ---------------------------------------------------------------------------
#
# risk_low_pos and risk_low_neg arrive already scored in study2_analysis_data.csv.
# The block below is not run; it documents how they were built, because the
# item-to-valence mapping lives nowhere else and cohort 1 will need it.
#
# The PNRT asks two things per activity: whether you have EVER done it, and how
# often in the past 6 months. The frequency items are the usual scoring. We used
# the EVER items -- each score is the proportion of activities in that set a
# participant has ever engaged in, i.e. variety, not rate. That is deliberate:
# the paper's claim is about the breadth of risk behaviour, and it keeps the
# PNRT subtypes on the same footing as the YRBS score above.
#
# Note pnrt_personal is scored NEGATIVE and pnrt_secret is scored POSITIVE,
# which is not what you would guess from the variable names.
#
# pnrt_neg_items <- c("pnrt_phone", "pnrt_cheat", "pnrt_skip", "pnrt_personal",
#                     "pnrt_snuckout", "pnrt_sexy", "pnrt_mph")            # 7
#
# pnrt_pos_items <- c("pnrt_audition", "pnrt_newclub", "pnrt_truth", "pnrt_food",
#                     "pnrt_leader", "pnrt_like", "pnrt_challenge", "pnrt_newlook",
#                     "pnrt_newsocial", "pnrt_secret", "pnrt_beliefs",
#                     "pnrt_friend", "pnrt_sport", "pnrt_newpeople")       # 14
#
# pnrt <- readr::read_csv(file.path(data_dir, "raw", "pnrt.csv"), show_col_types = FALSE) %>%
#   rename(PID = pid_assignment) %>%
#   mutate(PID = as.numeric(gsub("\\..*$", "", as.character(PID)))) %>%
#   mutate(
#     negativerisk_ever_mean = rowMeans(across(all_of(pnrt_neg_items)), na.rm = TRUE),
#     positiverisk_ever_mean = rowMeans(across(all_of(pnrt_pos_items)), na.rm = TRUE)
#   )
#
# Verified against the published scored columns: this mapping reproduces both
# variables exactly on all 254 complete cases.
