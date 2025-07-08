# Configurations that will be used throughout pipeline; source this script

# Install necessary packages
packages <- c(
  "tidyverse", "DiagrammeR", "Hmisc", "RODBC", "cmprsk", "cowplot", "data.table", 
  "doParallel", "dplyr", "fastshap", "ggplot2", "ggrepel", "lubridate", "mice", 
  "performance", "pracma", "prodlim", "purrr", "riskRegression", 
  "shapviz", "shiny", "stringr", "survival", "survminer", "tictoc", "tidyr", 
  "timeROC", "fastcmprsk")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Choose a name for the analysis version.
analysis_version <- "062724"

# Define directories to be used for the analysis
## scripts and inputs
script_dir <- paste0("/path/to/scripts/GFL_", analysis_version, "/")
input_dir <- paste0(script_dir, "script_inputs/")

## outputs
output_dir <- paste0("/path/to/GFL_", analysis_version, "/")
cohort_dir <- paste0(output_dir, "cohort_data/")
preprocess_dir <- paste0(output_dir, "preprocessed_data/")
results_dir_train <- paste0(output_dir, "results_train/")
results_dir_test <- paste0(output_dir, "results_test/")
results_dir_dim <- paste0(output_dir, "results_dim/")
results_dir_export_details <- paste0(results_dir_dim, "feature_details/")

## Directory for summary-level results that will be egressed
egress_dir <- paste0(output_dir, "egress_tmp/")

## Ensure the directories exist, create them if not
dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cohort_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(preprocess_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir_train, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir_export_details, recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(results_dir_test, "imgs/"), recursive = TRUE, showWarnings = FALSE)
dir.create(egress_dir, showWarnings = FALSE)

# Set default working directory
setwd(script_dir)

# Directory of raw RDS files, and the approximate creation date
C2023dir <- "/path/to/rawCommons/"
C2023_PULL_DATE <- decimal_date(as.Date("2023-03-06")) - 2000

# Directory of dim files (e.g. MVP Lab Dim)
dim_dir <- "/path/to/dim/files/"

# Project database
dbProj <- "VA_MVP066"

# Connect to db
dbDriver <- "18"
db_dbProj <- odbcDriverConnect(
  paste0('Driver={ODBC Driver ', dbDriver, 
         ' for SQL Server};Server=vawnornl.va.ornlkdi.org;Database=', 
         dbProj, ';Trusted_Connection=yes;TrustServerCertificate=yes'))


