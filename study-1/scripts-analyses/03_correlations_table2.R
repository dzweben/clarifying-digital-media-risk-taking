# Study 1: Correlations (Table 2) ------------------------------------------
# Inputs:
#   - outputs/study1_clean.rds
# Outputs:
#   - outputs/study1_table2_correlations.csv
#   - outputs/study1_table2_correlations_numeric.csv

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

main_vars <- c("mtes", "impulsivity", "reward_sensitivity", "exploration")
row_vars <- c(main_vars, "risk_variety", "risk_high_neg", "risk_low_neg", "risk_low_pos")

# Function to get r and p
get_r_p <- function(x, y) {
  d <- df[, c(x, y)]
  d <- d[complete.cases(d), ]
  if (nrow(d) < 3) return(list(r = NA_real_, p = NA_real_))
  test <- cor.test(d[[x]], d[[y]], method = "pearson")
  list(r = unname(test$estimate), p = test$p.value)
}

# Build numeric matrices
r_mat <- matrix(NA_real_, nrow = length(row_vars), ncol = length(main_vars),
                dimnames = list(row_vars, main_vars))

p_mat <- r_mat

for (rv in row_vars) {
  for (cv in main_vars) {
    if (rv == cv) {
      r_mat[rv, cv] <- NA_real_
      p_mat[rv, cv] <- NA_real_
    } else {
      res <- get_r_p(rv, cv)
      r_mat[rv, cv] <- res$r
      p_mat[rv, cv] <- res$p
    }
  }
}

# Create formatted table matching manuscript style
formatted <- matrix("", nrow = nrow(r_mat), ncol = ncol(r_mat),
                    dimnames = dimnames(r_mat))
for (i in seq_len(nrow(r_mat))) {
  for (j in seq_len(ncol(r_mat))) {
    if (!is.na(r_mat[i, j])) {
      formatted[i, j] <- format_r(r_mat[i, j], p_mat[i, j])
    }
  }
}

formatted_df <- as.data.frame(formatted)
formatted_df <- cbind(Variable = rownames(formatted_df), formatted_df)

numeric_df <- as.data.frame(r_mat)
numeric_df <- cbind(Variable = rownames(numeric_df), numeric_df)

# Save outputs
write_csv(formatted_df, file.path(output_dir, "study1_table2_correlations.csv"))
write_csv(numeric_df, file.path(output_dir, "study1_table2_correlations_numeric.csv"))

cat("Study 1 Table 2 correlations written to outputs/.\n")
