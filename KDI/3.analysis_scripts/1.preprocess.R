# Coerce all the time-dependent datasets into a single time-dependent dataset, 
# by cohort. Collapsing everything into 0.1 yr increments
# Export snapshots of covariate data at each landmark (1, 5, 10 years post DM)

# Run from parent directory
source("config_mnt.R")
source("functions.R")

library(dplyr)
library(tictoc)
library(survival)
# library(GGally)
library(stringr)
library(performance)
library(tidyr)
library(data.table)

args <- commandArgs(trailingOnly = T)
cohort <- args[1]
outcome <- args[2]
print(paste0("cohort is ", cohort))

## COHORT LEVEL PROCESSING #####
tic("Cohort level processing ")

# Load the data generated for this cohort
load(paste0(cohort_dir, "coh", cohort, "/data.RData"))
if (nrow(data_wide) != length(unique(data_wide$PatientICN))) {
  stop("Numer of rows must match number of unique patients")
}

# Ages the patients in the cohort would be in 2023, if alive
2023 - 2000 - unique(floor(data_wide$BirthDate))

# Standard variables
data_wide <- data_wide %>%
  select(-any_of(c("age", "yrs", "dt"))) %>%
  select(-c(
    first_Insulin, first_glucagon, first_OralGlucoseLoweringExclMetformin,
    first_GLP1, first_Metformin
  ))

# Baseline variables we defined at DmDx T0
baseline_data <- data_wide %>%
  select(PatientICN, ends_with("0")) %>%
  select(-Age_2000)

# Standard variables except baseline variables
data_std <- data_wide %>%
  select(-all_of(names(baseline_data %>% select(-PatientICN))))
if (outcome == "RenalFailure") {
  data_std <- data_std %>% select(-contains("CKD"))
}
names(data_std)

# Names of time-varying features
tv_features <- gsub("0$", "", 
                    gsub("_ever0$", "", 
                         names(baseline_data %>% select(-PatientICN))))
tv_features <- tv_features[!(tv_features %in% 
                               c("KidneyTransplant_Dx", "Dialysis_Proc", 
                                 "Dialysis_Dx", "KidneyTransplant_Proc"))]
tv_features

rm(baseline_data, data_wide)

# Number of rows and patients
npr(data_std)

# Rename the outcome to something generic so that code can be applied
# to other outcomes
coh_outcome <- data_std %>%
  dplyr::rename(
    event01 = outcome,
    event_tte = paste0(outcome, "_tte")
  ) %>%
  dplyr::select(PatientICN, event01, event_tte)

# There shouldn't be missing data in these fields, warn if so
if (nrow(coh_outcome) != nrow(coh_outcome %>% filter(complete.cases(.)))) {
  warning("Some patients were missing the event status or time to event")
}
coh_outcome <- coh_outcome %>%
  filter(complete.cases(.)) %>%
  filter(event_tte > 0) %>%
  arrange(PatientICN)
npr(coh_outcome)

# Filter some suspicious cases of events recorded much later than death
coh <- coh_outcome %>%
  left_join(data_std) %>%
  mutate(event_tte = ifelse(DeceasedFlag == 1 & 
                              event_tte > Deceased_tte & 
                              event_tte - Deceased_tte <= 0.2,
                            Deceased_tte, event_tte)
         ) %>% 
  filter(!(event_tte > Deceased_tte)) %>% 
  # Not interested in those having the event prior to DM
  filter(!(round(event_tte, 1) <= 0)) 
npr(coh)
coh_ids <- coh %>% distinct(PatientICN)

# Overall case proportions:
coh %>%
  count(event01) %>%
  mutate(prop = round(n / sum(n), 3))

toc()

## FEATURE CATEGORIES TO FACILITATE PROCESSING ####

tic("Feature categories ")
biomarker_features <- unique(c(
  labs_export_1$marker_name, # continuous
  labs_export_2$feature_name # categorized
)) 
biomarker_features <- biomarker_features[!grepl("_Ref", biomarker_features)] 
# Reference values are not selectable features
biomarker_features <- biomarker_features[biomarker_features != ""]
biomarker_features

# These won't be predictors in the model
non_features <- c(
  "PatientICN", "T1D", "DeceasedFlag", "BirthDate", "DeathDate", 
  "death_censor_date", "DmDxCode_first",
  "LastDx", "FirstDx", "DmDx_first", "censor_date", "Deceased_tte", "cohort"
)
# These features don't vary with time
invariant_features <- c("Age_DmDx", "Age_2000", "Race", "Ethnicity", 
                        "Gender", "MaritalStatus")
# Although MaritalStatus is not invariant, we don't have the time-varying status

# All possible conditions/procedures
cond_proc_features_all <- extraction_details_export %>%
  filter(domain %in% c("Conditions", "Procedures")) %>%
  .$feature_name %>%
  unique()
cond_proc_features_all
rm(cond_proc_features)


# Remove certain features that won't be time-dependent variables
cond_proc_features_all <- 
  cond_proc_features_all[!(
    cond_proc_features_all %in%
      c("DM", "CKD_Dx", "CKD3a_Dx", "CKD3b_Dx", "CKD4_Dx", "CKD5_Dx",
        "KidneyTransplant_Dx", "Dialysis_Proc", "Dialysis_Dx", 
        "KidneyTransplant_Proc", "OldMI")
    )]
cond_proc_features_all

# Existing med features in this cohort:
med_features

# All posssible med features 
med_features_all <- extraction_details_export %>%
  filter(domain == "Medications") %>%
  .$feature_name %>%
  unique()
rm(med_features)

other_features <- setdiff(tv_features, c(
  med_features_all, cond_proc_features_all,
  biomarker_features, non_features, invariant_features
))
other_features # comorbidity and smoking

rm(tv_features)

toc()
## MEDS PROCESSING ####
tic("Meds processing")
for (obj in med_features_all) {
  print(obj)
  assign(obj, get(obj) %>% med.gap())
}
toc()

### ROUNDING AND SUMMARIZING TO 0.1-YR INTERVALS (EXCEPT MEDS) ####
tic("Rounding and summarizing")
# apply rounding to all objects with time-dependent data, as well as the 
# time to event/death.
# Round all times to 0.1 year.
names(coh)
coh_rounded <- coh %>%
  mutate(across(c("event_tte", "Deceased_tte"), function(x) round(x, 1)))
rm(coh)

# Identify objects with "yrs" variable
objects_to_round <- ls()[sapply(
  ls(),
  function(x) {
    obj <- get(x)
    is.data.frame(obj) && "yrs" %in% colnames(obj)
  }
)]

# BBM_obj are the object names and the name of the continuous biomarkers
BBM_obj <- objects_to_round[objects_to_round %in% biomarker_features]
BBM_obj
BBM_categorical <- setdiff(biomarker_features, BBM_obj)
BBM_categorical

# Save these categories for future use
feature_key_loc <- paste0(preprocess_dir, "feature_key.", outcome, ".RData")
save(invariant_features, other_features, non_features,
  BBM_obj, BBM_categorical, biomarker_features, med_features_all, 
  cond_proc_features_all, extraction_details_export,
  objects_to_round,
  file = feature_key_loc
)

tic("Round/summarize biomarkers")
# For biomarker objects, round and filter to no more than 2 yrs prior to DM
for (obj in BBM_obj) {
  assign(obj, get(obj) %>% filter(yrs >= -2) %>% yr_round())
}
toc()

tic("Round/summarize other objects")
# For other objects, can be more than 2 yrs old. Don't do meds as these were 
# already processed and rounded.
for (obj in setdiff(objects_to_round, c(BBM_obj, med_features_all))) {
  assign(obj, get(obj) %>% yr_round())
}
toc()

toc()

## DEFINE TINDPT AND TDEPT, SLIP IN ALL VARS ####
tic("Slipping in variables")
# initialize time-independent object
tindpt <- tmerge(coh_rounded, coh_rounded,
  id = PatientICN,
  endpt = event(event_tte, event01),
  death = event(Deceased_tte, DeceasedFlag)
)
# Merge DCSI to initialize time-dependent object
tdept <- tmerge(tindpt, DCSI, id = PatientICN, DCSI = tdc(yrs, DCSI))

# Merge other time-dependent comorbidity scores 
# (Elixhauser 2 versions and DCSI 2 versions)
tdept <- tmerge(tdept, DCSI_excl_cvd, id = PatientICN, 
                DCSI_excl_cvd = tdc(yrs, DCSI_excl_cvd))
tdept <- tmerge(tdept, DCSI_excl_neph, id = PatientICN, 
                DCSI_excl_neph = tdc(yrs, DCSI_excl_neph))
tdept <- tmerge(tdept, elix_excl_RF, id = PatientICN, 
                Elix_excl_RF = tdc(yrs, Elix_excl_RF))
tdept <- tmerge(tdept, elix.score.nodm, id = PatientICN, 
                Elix_RawScore = tdc(yrs, Elix_RawScore))

# Merge smoking if it exists
if (nrow(smoking %>% inner_join(coh_ids)) > 0) {
  tdept <- tmerge(tdept, smoking,
    id = PatientICN, SmokingFactor = tdc(yrs, SmokingFactor),
    EVERSMOKER = tdc(yrs, EVERSMOKER)
  )
} else {
  tdept[, "SmokingFactor"] <- NA
  tdept[, "EVERSMOKER"] <- NA
}

# tmerge all conditions and procedures - dataset name and variable name are the
# same, value is yes/no
for (COV in cond_proc_features_all) {
  print(paste("Merging ", COV))
  data2 <- get(COV) %>%
    inner_join_quiet(coh_ids) 
  if (nrow(data2) > 0) {
    tdept <- tmerge(tdept, data2,
      id = PatientICN,
      newtdc = tdc(yrs)
    ) # automatically 0 -> 1 at time of event and onward
    names(tdept)[ncol(tdept)] <- COV
  } else {
    tdept[, COV] <- NA
  }
  rm(COV, data2)
}

# tmerge all biomarkers
for (COV in BBM_obj) {
  print(paste("Merging ", COV))
  data2 <- get(COV) %>% inner_join_quiet(coh_ids)
  vars_data2 <- names(data2)[names(data2) %in% biomarker_features]
  for (v in vars_data2) {
    if (nrow(data2) > 0) {
      tdept <- tmerge(tdept, data2,
        id = PatientICN,
        newtdc = tdc(yrs, get(v))
      )
      names(tdept)[ncol(tdept)] <- v
    } else {
      tdept[, v] <- NA
    }
  }
  rm(data2, vars_data2, COV, v)
}

# tmerge medications
for (COV in med_features_all) {
  print(paste("Merging ", COV))
  data2 <- get(COV) %>% inner_join_quiet(coh_ids)
  if (nrow(data2) > 0) {
    tdept <- tmerge(tdept, data2,
      id = PatientICN,
      newtdc = tdc(yrs, med)
    )
    names(tdept)[ncol(tdept)] <- COV
  } else {
    tdept[, COV] <- NA
  }
  rm(data2, COV)
}

toc()

## Post-tmerge varibles processing ####
tic("Post-tmerge processing")
tdept <- tdept %>%
  # If never taken med set to 0.  If unknown biomarker value, cat is 0 
  # (NA cat will be set to 1)
  mutate(across(
    any_of(c(med_features_all, BBM_categorical, cond_proc_features_all)),
      coalesce0))

pNA <- apply(tdept, 2, function(x) {
  mean(is.na(x))
})
pNA[pNA > 0 & !(names(pNA) %in% BBM_obj)]

tdept <- tdept %>%
  mutate(
    age_td = Age_DmDx + tstart,
    calyear_td = floor(DmDx_first + 2000 + tstart)
  ) %>%
  mutate_at(c("Age_DmDx", "Age_2000", "age_td", BBM_obj), 
            function(x) round(x, 3)) %>%
  mutate(EGFR = round(EGFR, 0)) %>%
  mutate(
    SmokingFactor = coalesce(SmokingFactor, "UNKNOWN"),
    EVERSMOKER = ifelse(is.na(EVERSMOKER), "UNKNOWN",
      ifelse(EVERSMOKER == 1, "YES", "NO")
    ),
    MaritalStatus = coalesce(MaritalStatus, "UNKNOWN")
  ) %>%
  mutate(censtime = censor_date - DmDx_first)

# Add missingness indicators
for (na.var in BBM_obj) {
  tdept <- tdept %>% mutate(newvar = as.numeric(is.na(!!sym(na.var))))
  names(tdept)[ncol(tdept)] <- paste0(na.var, "_NA")
}


# Make sure both datasets have the same individuals in the same order
tindpt <- tindpt %>% filter(PatientICN %in% tdept$PatientICN)
tdept <- left_join(tindpt %>% distinct(PatientICN), tdept, multiple = "all")

toc()

## SAVE RESULTS TO FILES ####

# Save only the relevant objects for the next steps.

save_dir <- paste0(preprocess_dir, "coh", cohort, "/", outcome, "/observed/")
dir.create(save_dir, recursive = T)

save_loc <- paste0(save_dir, "preprocess.coh", cohort, ".", outcome, ".RData")

save(data_std, coh_rounded, tindpt, tdept, invariant_features, other_features, 
     non_features, BBM_obj, BBM_categorical, biomarker_features, 
     med_features_all, cond_proc_features_all, extraction_details_export, 
     objects_to_round, extraction_results_export, labs_export_1, labs_export_2, 
     flowchart_data, cohort, outcome, cohort_n, pNA,
  file = save_loc
)

## LANDMARKING FOR SUBCOHORT ####
tic("Landmarking")
# Landmark & export
for (t in c(1, 5, 10)) {
  print(paste0("Landmarking for year ", t))
  ydat <- landmark(tdept, t) %>%
    mutate(event_CR = ifelse(event01 == 1 & event_tte <= Deceased_tte, 1,
      # only consider deaths if within 6mo of last visit. Otherwise event 
      # couldn't be observed after LTF.
      ifelse(DeceasedFlag == 1 & (Deceased_tte - censtime <= 0.5), 2, 0)
    )) %>%
    mutate(
      CR_tte = ifelse(event_CR %in% c(1, 0), event_tte,
        ifelse(event_CR == 2, Deceased_tte, NA)
      ) - t, # TIME FROM CURRENT
      censtime = censtime - t,
      event_tte = event_tte - t,
      Deceased_tte = Deceased_tte - t
    ) %>%
    filter(CR_tte > 0 & censtime > 0) #FILTER TO PATIENTS STILL AT RISK
  saveRDS(ydat, paste0(save_dir, "landmark_y", t, ".coh", cohort, ".", 
                       outcome, ".RDS"))
}
toc()
print("End of script")
