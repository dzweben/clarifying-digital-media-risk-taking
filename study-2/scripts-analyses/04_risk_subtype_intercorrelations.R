# Study 2: Risk Subtype Intercorrelations ----------------------------------
# Inputs:
#   - outputs/study2_clean.rds
# Outputs:
#   - outputs/study2_risk_subtype_intercorrelations.csv

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

clean_path <- file.path(output_dir, "study2_clean.rds")
if (!file.exists(clean_path)) {
  source(file.path(script_dir, "01_data_prep.R"))
}

df <- readRDS(clean_path)

pairs <- list(
  c("risk_high_neg", "risk_low_neg"),
  c("risk_high_neg", "risk_low_pos"),
  c("risk_low_neg", "risk_low_pos")
)

run_corr <- function(d, x, y, cohort) {
  d2 <- d[, c(x, y)]
  d2 <- d2[complete.cases(d2), ]
  if (nrow(d2) < 3) {
    return(data.frame(cohort = cohort, var1 = x, var2 = y, r = NA, p = NA, n = nrow(d2)))
  }
  test <- cor.test(d2[[x]], d2[[y]], method = "pearson")
  data.frame(
    cohort = cohort,
    var1 = x,
    var2 = y,
    r = round(unname(test$estimate), 3),
    p = round(test$p.value, 4),
    n = nrow(d2)
  )
}

rows <- list()

# Adolescents (<3000)
teen_df <- df %>% filter(pid < 3000)
for (pair in pairs) {
  rows[[length(rows) + 1]] <- run_corr(teen_df, pair[1], pair[2], "Adolescents")
}

# Adults (>3000)
adult_df <- df %>% filter(pid > 3000)
for (pair in pairs) {
  rows[[length(rows) + 1]] <- run_corr(adult_df, pair[1], pair[2], "Young Adults")
}

out <- bind_rows(rows)
write_csv(out, file.path(output_dir, "study2_risk_subtype_intercorrelations.csv"))

cat("Study 2 risk subtype intercorrelations written to outputs/.\n")
