# Study 2

**What This Folder Contains**
- `data/` (CSV files used for Study 2)
- `scripts-analyses/` (numbered analysis scripts)

**Data Files**
- `study2_analysis_data.csv`: main analysis dataset.
- `data/raw/yrbs.csv`: YRBS items used to compute the high‑stakes negative risk composite.
- `data/raw/bar.csv`: BAR items used to compute the impulsivity mean for descriptives.
- Additional CSVs in `data/raw/` are retained for transparency and scoring checks.

**Cohorts**
- Young Adults: PID > 3000
- Adolescents: PID < 3000

**How To Run Study 2**
1. Open R or RStudio.
2. Set your working directory to this folder.
3. Run the scripts in `scripts-analyses/` in numeric order.

**Single‑Command Option**
Run everything at once:
`Rscript scripts-analyses/run_all.R`

**What Each Script Does**
- `01_data_prep.R`: computes MTES composite scores, derives YRBS high‑stakes negative risk, reverses foraging to index exploration, applies outlier removal (|z| > 3 per variable), and writes cleaned data used by later scripts.
- `02_descriptives_table4.R`: produces Study 2 descriptive statistics by cohort.
- `03_correlations_table5_6.R`: reproduces correlations for young adults (Table 5) and adolescents (Table 6).
- `04_risk_subtype_intercorrelations.R`: reproduces intercorrelations among risk subtypes reported in the text.
- `05_shapley.R`: reproduces Shapley value regressions by cohort.
- `06_mediation_table11_12.R`: reproduces mediation analyses by cohort.
- `07_hierarchical_regression_table13_14.R`: reproduces hierarchical regression tables by cohort.
- `08_steiger_table7_8.R`: reproduces Steiger tests comparing social vs. parasocial platform use.

Output tables are created when the scripts run.
