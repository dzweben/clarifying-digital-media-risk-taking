# Study 2 -- Risk subtype intercorrelations  ->  reported in text (section 3.2.5)
#
# In:  outputs/study2_clean.rds
# Out: outputs/study2_risk_subtype_intercorrelations.csv
#
# Not a table in the paper. These are the three correlations quoted at the top
# of the "Specific types of risk taking" section, and they carry the argument
# that the subtypes are related but not interchangeable: high- and low-stakes
# negative risk track each other, but low-stakes POSITIVE risk does not track
# high-stakes negative risk at all.

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

pairs <- list(
  c("risk_high_neg", "risk_low_neg"),
  c("risk_high_neg", "risk_low_pos"),
  c("risk_low_neg",  "risk_low_pos")
)

run_corr <- function(d, x, y, cohort) {
  d2 <- d[, c(x, y)]
  d2 <- d2[complete.cases(d2), ]
  if (nrow(d2) < 3) return(data.frame(cohort = cohort, var1 = x, var2 = y, r = NA, p = NA, n = nrow(d2)))
  test <- cor.test(d2[[x]], d2[[y]], method = "pearson")
  data.frame(cohort = cohort, var1 = x, var2 = y,
             r = round(unname(test$estimate), 3),
             p = round(test$p.value, 4),
             n = nrow(d2))
}

rows <- list()
for (pair in pairs) rows[[length(rows) + 1]] <- run_corr(df %>% filter(is_adolescent(.)),  pair[1], pair[2], "Adolescents")
for (pair in pairs) rows[[length(rows) + 1]] <- run_corr(df %>% filter(is_young_adult(.)), pair[1], pair[2], "Young Adults")

write_csv(bind_rows(rows), file.path(output_dir, "study2_risk_subtype_intercorrelations.csv"))
cat("Risk subtype intercorrelations written to outputs/.\n")

# COHORT 1: without YRBS only the third pair survives. Keep reporting it -- the
# positive/negative distinction is still the point.
