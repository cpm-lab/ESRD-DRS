#Configurations that will be used throughout pipeline; source this script

analysis_version <- "062724"

script_dir <- paste0("/path/to/scripts/GFL_", analysis_version, "/")
input_dir <- paste0(script_dir, "script_inputs/")
output_dir <- paste0("/path/to/GFL_", analysis_version, "/")
cohort_dir <- paste0(output_dir, "cohort_data/")
preprocess_dir <- paste0(output_dir, "preprocessed_data/")
results_dir_train <- paste0(output_dir, "results_train/")
results_dir_test <- paste0(output_dir, "results_test/")
results_dir_dim <- paste0(output_dir, "results_dim/")
results_dir_export_details <- paste0(results_dir_dim, "feature_details/")
egress_dir <- paste0(output_dir, "egress_tmp/")

dir.create(input_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cohort_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(preprocess_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir_train, recursive = TRUE, showWarnings = FALSE)
dir.create(results_dir_export_details, recursive = TRUE, showWarnings = FALSE)
dir.create(paste0(results_dir_test, "imgs/"), recursive = TRUE, showWarnings = FALSE)
dir.create(egress_dir, showWarnings = FALSE)

fastcmprsk_lib <- "/path/to/R/x86_64-redhat-linux-gnu-library_alt/4.4/"

setwd(script_dir)

dbCohort <- "VA_MVP066.[KDI\\jensena.mvp066v].Ci2023"
dbProj <- "VA_MVP066"
dim_dir <- "/mnt/mvpfs/VA_MVP066/mcmahon/C24/"

# Directory of raw RDS files, and the date they were pulled
C2023dir <- "/mnt/mvp_datacommons/rawCommons/C2023/"
C2023_PULL_DATE <- decimal_date(as.Date("2023-03-06")) - 2000

# Driver 13 --> 18 (17 for walsh)
# db <- odbcDriverConnect('Driver={ODBC Driver 18 for SQL Server};Server=vawnornl.va.ornlkdi.org;Trusted_Connection=yes;TrustServerCertificate=yes')
# tableinfo_all <- sqlQuery(db, "SELECT * FROM Information_schema.Tables")
# db_OMOP <- odbcDriverConnect('Driver={ODBC Driver 18 for SQL Server};Server=vawnornl.va.ornlkdi.org;Database=OMOP_V5_v2018;Trusted_Connection=yes;TrustServerCertificate=yes')
# db_CDWWork <- odbcDriverConnect('Driver={ODBC Driver 18 for SQL Server};Server=vawnornl.va.ornlkdi.org;Database=CDWWork;Trusted_Connection=yes;TrustServerCertificate=yes')
# db_dbProj <- odbcDriverConnect(paste0('Driver={ODBC Driver 18 for SQL Server};Server=vawnornl.va.ornlkdi.org;Database=', dbProj, ';Trusted_Connection=yes;TrustServerCertificate=yes'))
# tableinfo_CDWWork <- sqlQuery(db_CDWWork, "SELECT * FROM Information_schema.Tables")
# columninfo <- sqlQuery(db_CDWWork, "SELECT * FROM Information_schema.Columns")
# tableinfo_proj <- sqlQuery(db_dbProj, "SELECT * FROM Information_schema.Tables")
