## Impute at y1, carry forward imputed values

# Libraries and setup ####

# Run from parent directory
source("config_mnt.R")
source("functions.R")

library(tictoc)
library(mice)
library(tidyverse)

args <- commandArgs(trailingOnly = T)
irep <- as.numeric(args[1]) # multiple imputation replicates
outcome <- args[2]
minCorr <- as.numeric(args[3])
traintest <- args[4]
seed <- irep # Set seed to replicate number

set.seed(seed)

## Read in data, define functions, process data ####
d1 <- readRDS(paste0(
  preprocess_dir, "cohAll/", outcome, "/observed/landmark_y1.",
  traintest, ".", outcome, ".observed.RDS"
))

if (class(d1$SmokingFactor) == "character") {
  d1$SmokingFactor <- factor(d1$SmokingFactor,
    levels = c("NEVERSMOKER", "CURRENTSMOKER", "FORMERSMOKER", "UNKNOWN")
  )
}

if (class(d1$EVERSMOKER) == "character") {
  d1$EVERSMOKER <- factor(d1$EVERSMOKER,
    levels = c("NO", "YES", "UNKNOWN")
  )
}

feature_key_loc <- paste0(preprocess_dir, "feature_key.", outcome, ".RData")
load(feature_key_loc)

BBM_cont_cat_key <- readRDS(paste0(results_dir_export_details, 
                                   "BBM_cont_cat_key.RDS")) %>%
  filter(!grepl("_Ref", feature_name))

# Figure out which biomarkers to impute, based on a maxiumum missingness rate
imputation_missingness_threshold <- .4
propNA <- apply(
  d1 %>% select(all_of(BBM_obj)),
  2, function(x) round(mean(is.na(x)) * 100, 4)
)
to_impute_continuous <- propNA[propNA <= imputation_missingness_threshold * 100]
not_impute_continuous <- propNA[propNA >= imputation_missingness_threshold * 100]
to_impute_continuous
not_impute_continuous

length(to_impute_continuous)
length(not_impute_continuous)

imputed_vs_categorized_key <-
  data.frame(
    propNA = to_impute_continuous,
    form = "imputed"
  ) %>%
  tibble::rownames_to_column("marker_name") %>%
  rbind(
    data.frame(
      propNA = not_impute_continuous,
      form = "categorized"
    ) %>%
      tibble::rownames_to_column("marker_name")
  ) %>%
  mutate(dataset = traintest) %>%
  dplyr::rename(percentNA = propNA)

fwrite(imputed_vs_categorized_key, 
       paste0(egress_dir, "imputed_vs_categorized_key_", traintest, ".csv"))

to_drop_categorical <- 
  BBM_cont_cat_key$feature_name[BBM_cont_cat_key$marker_name %in% 
                                  names(to_impute_continuous)]

# Some vars not categorized except '_NA' so they aren't in the key, drop 
# these if the var will be imputed
addl_na_cat <- 
  paste0(names(to_impute_continuous)[!(names(to_impute_continuous) %in% 
                                         BBM_cont_cat_key$marker_name)], "_NA")
addl_na_cat
to_drop_categorical <- c(to_drop_categorical, addl_na_cat)
length(to_drop_categorical)

# Vars to separate prior to imputation, then add back later
vars_preserve <- c(
  "event01", "event_tte", "T1D", "DeceasedFlag", "BirthDate", "DeathDate",
  "event_CR", "CR_tte", "censtime", "Age_2000", "calyear_td",
  "Age_DmDx", # rather will use td_age for imputation (they are collinear)
  "death_censor_date", "DmDxCode_first", "LastDx", "FirstDx", "DmDx_first",
  "censor_date", "Deceased_tte", "cohort", "first_RenalFailure", "RenalFailure",
  "RenalFailure_tte", "tstart", "tstop", "endpt", "death", "DeceasedFlag",
  # only use the main versions of these scores in imputation - highly collinear
  "DCSI_excl_cvd", "DCSI_excl_neph", "Elix_excl_RF", "Elix_RawScore"
)

d1_s <- d1 %>%
  select(-any_of(
    c(names(not_impute_continuous), to_drop_categorical, vars_preserve)))

d1_preserve <- d1 %>% select(all_of(c("PatientICN", vars_preserve)))
rm(d1)
gc()

## Perform multiple imputation at year 1 #####
tic("Getting quickpred for mice")
pred1 <- quickpred(d1_s,
  mincor = minCorr,
  minpuc = 0.01,
  exclude = c("PatientICN", "EVERSMOKER")
) 

toc()

tic("Generating mice object")
obj_mice <- mice(d1_s, m = 1, seed = seed, pred = pred1)
toc()

obj_mice$loggedEvents

out_dir <- paste0(preprocess_dir, "cohAll/", outcome, "/imputed/")
dir.create(out_dir, recursive = T)
outfile_y1_mobj <- paste0(
  out_dir, "mice.landmark_y1.", traintest, ".", outcome,
  ".r", irep, ".imputed.mincorr", minCorr, ".RDS"
)
saveRDS(obj_mice, outfile_y1_mobj)

# Completing t1 data")
imputed_t1 <- complete(obj_mice) %>%
  left_join(d1_preserve)
outfile_y1_data_prexform <- gsub("mice.", "data.prexform.", outfile_y1_mobj)
saveRDS(imputed_t1, outfile_y1_data_prexform)

check_classes <- sapply(imputed_t1, class)
table(check_classes)
check_classes[check_classes == "character"]
names(check_classes[check_classes == "factor"])
names(check_classes[check_classes == "numeric"])

rm(obj_mice)
gc()

# Convert binary variables to factor
imputed_t1_f <- imputed_t1 %>%
  mutate(across(everything(), factorize.i, ignore.failed = T))

numeric_vars <- imputed_t1_f %>%
  select_if(is.numeric) %>%
  names(.)
numeric_vars

# Variables to standardize after imputation
logtx_vars <- c("Trig", "WBC", "Platelet")
never_transform_vars <- c(
  "PatientICN", "event01", "event_tte", "CR_tte", "event_CR",
  "BirthDate", "DeathDate", "censtime", "calyear_td", "death_censor_date", 
  "DmDxCode_first", "LastDx", "FirstDx", "DmDx_first", "censor_date", 
  "Deceased_tte", "cohort", "first_RenalFailure",
  "RenalFailure_tte", "tstart", "tstop"
)
std_vars <- numeric_vars[!(numeric_vars %in% never_transform_vars)]
std_vars

tic("Applying transformations at year 1")
imputed_t1_f <- imputed_t1_f %>%
  mutate(across(logtx_vars, log)) %>%
  mutate(across(std_vars, standardize))
toc()

outfile_y1_data_postxform <- gsub("mice.", "data.postxform.", outfile_y1_mobj)
saveRDS(imputed_t1_f, outfile_y1_data_postxform)
rm(imputed_t1_f)
gc()

# Fill in data for future time points, write out both full sets & subsets #####
tic("Filling in years 5 and 10")
for (landmark_year in c(5, 10)) {
  print(paste0("Filling in data for landmark year ", landmark_year))

  ydat <- readRDS(paste0(
    preprocess_dir, "cohAll/", outcome, "/observed/landmark_y", landmark_year,
    ".", traintest, ".", outcome, ".observed.RDS"
  )) %>%
    inner_join(imputed_t1 %>% select(PatientICN))

  tic("Filling in values")
  ydat <- ydat %>%
    mutate(t1row = FALSE) %>%
    # Join with the imputed year 1 data, ids in the current set 
    full_join_quiet(imputed_t1 %>% mutate(t1row = TRUE) %>%
      right_join(ydat %>% distinct(PatientICN))) %>%
    arrange(PatientICN, desc(t1row)) %>%
    group_by(PatientICN) %>%
    fill(any_of(names(to_impute_continuous)), .direction = "down") %>%
    filter(!t1row) %>%
    ungroup() %>%
    select(-t1row) %>%
    select(-any_of(c(names(not_impute_continuous), to_drop_categorical)))
  toc()

  if (mean(is.na(ydat$BMI)) > 0) {
    # report missing rates for variables
    propNA <- apply(ydat, 2, function(x) mean(is.na(x)))
    propNA[propNA > 0]
    stop("There is still missing data...")
  }

  # Write out pre-transforming
  outfile_data_prexform <- paste0(
    out_dir, "data.prexform.landmark_y", landmark_year,
    ".", traintest, ".", outcome, ".r", irep,
    ".imputed.mincorr", minCorr, ".RDS"
  )
  saveRDS(ydat, outfile_data_prexform)

  # Then apply transformations
  ydat <- ydat %>%
    select(-any_of(c(names(not_impute_continuous), to_drop_categorical))) %>%
    mutate(across(everything(), factorize.i, ignore.failed = T)) %>%
    mutate(across(logtx_vars, log)) %>%
    mutate(across(std_vars, standardize))
  toc()

  # Write out after transforming
  outfile_data_postxform <- gsub("prexform", "postxform", outfile_data_prexform)
  saveRDS(ydat, outfile_data_postxform)
}

toc()

print("End of script")
