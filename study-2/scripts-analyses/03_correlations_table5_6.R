# Study 2: Correlations (Tables 5 and 6) -----------------------------------
# Inputs:
#   - outputs/study2_clean.rds
# Outputs:
#   - outputs/study2_table5_correlations_adults.csv
#   - outputs/study2_table6_correlations_teens.csv
#   - outputs/study2_table5_correlations_adults_numeric.csv
#   - outputs/study2_table6_correlations_teens_numeric.csv

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

main_vars <- c("mtes_zscore", "impulsivity", "reward_sensitivity", "exploration")
row_vars <- c(main_vars, "risk_variety", "risk_high_neg", "risk_low_neg", "risk_low_pos")

get_r_p <- function(d, x, y) {
  d2 <- d[, c(x, y)]
  d2 <- d2[complete.cases(d2), ]
  if (nrow(d2) < 3) return(list(r = NA_real_, p = NA_real_))
  test <- cor.test(d2[[x]], d2[[y]], method = "pearson")
  list(r = unname(test$estimate), p = test$p.value)
}

build_table <- function(d) {
  r_mat <- matrix(NA_real_, nrow = length(row_vars), ncol = length(main_vars),
                  dimnames = list(row_vars, main_vars))
  p_mat <- r_mat

  for (rv in row_vars) {
    for (cv in main_vars) {
      if (rv == cv) {
        r_mat[rv, cv] <- NA_real_
        p_mat[rv, cv] <- NA_real_
      } else {
        res <- get_r_p(d, rv, cv)
        r_mat[rv, cv] <- res$r
        p_mat[rv, cv] <- res$p
      }
    }
  }

  formatted <- matrix("", nrow = nrow(r_mat), ncol = ncol(r_mat),
                      dimnames = dimnames(r_mat))
  for (i in seq_len(nrow(r_mat))) {
    for (j in seq_len(ncol(r_mat))) {
      if (!is.na(r_mat[i, j])) {
        formatted[i, j] <- format_r(r_mat[i, j], p_mat[i, j])
      }
    }
  }

  list(
    formatted = as.data.frame(cbind(Variable = rownames(formatted), formatted)),
    numeric = as.data.frame(cbind(Variable = rownames(r_mat), r_mat))
  )
}

# Adults (>3000)
adults <- df %>% filter(pid > 3000)
res_adults <- build_table(adults)
write_csv(res_adults$formatted, file.path(output_dir, "study2_table5_correlations_adults.csv"))
write_csv(res_adults$numeric, file.path(output_dir, "study2_table5_correlations_adults_numeric.csv"))

# Adolescents (<3000)
teens <- df %>% filter(pid < 3000)
res_teens <- build_table(teens)
write_csv(res_teens$formatted, file.path(output_dir, "study2_table6_correlations_teens.csv"))
write_csv(res_teens$numeric, file.path(output_dir, "study2_table6_correlations_teens_numeric.csv"))

cat("Study 2 correlation tables written to outputs/.\n")
