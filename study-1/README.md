# Study 1

**What This Folder Contains**
- `data/` (CSV files used for Study 1)
- `scripts-analyses/` (numbered analysis scripts)

**Data Files**
- `study1_analysis_data.csv`: analysis‑ready dataset used for Study 1 results.
- `study1_raw_survey.csv`: original survey export used to derive several composite scores.

**How To Run Study 1**
1. Open R or RStudio.
2. Set your working directory to this folder.
3. Run the scripts in `scripts-analyses/` in numeric order.

**Single‑Command Option**
Run everything at once:
`Rscript scripts-analyses/run_all.R`

**What Each Script Does**
- `01_data_prep.R`: loads the analysis dataset, applies outlier removal (|z| > 3 per variable), and writes cleaned data used by later scripts.
- `02_descriptives_table1.R`: produces descriptive statistics for Table 1.
- `03_correlations_table2.R`: reproduces Table 2 correlations (MTES, cognition, risk variety, and subtypes).
- `04_steiger_table3.R`: reproduces Steiger tests comparing social vs. parasocial platform use.
- `05_shapley.R`: reproduces the Shapley value regression reported in the manuscript.
- `06_hierarchical_regression_table10.R`: reproduces the hierarchical regression table.
- `07_mediation_table9.R`: reproduces the mediation results.

Output tables are created when the scripts run.
