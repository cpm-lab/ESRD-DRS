# This step to combine the preprocessed data landmark snapshots from each 
# subcohort, then divide them into test and training sets consistent across 
# landmark times and write these out, then to generate a table of cohort 
# characteristics at L1. Also combine the summaries of feature extraction 
# by subcohort

# Run from parent directory
source("config_mnt.R")
source("functions.R")

library(tidyverse)
library(tictoc)
library(data.table)

args <- commandArgs(trailingOnly = T)
outcome <- args[1]
print(paste0("outcome is ", outcome))

feature_key_loc <- paste0(preprocess_dir, "feature_key.", outcome, ".RData")
load(feature_key_loc)
fwrite(extraction_details_export, 
       paste0(egress_dir, 
              "extraction_def_conditions_procedures_medications_demog.csv"))

# Read data for landmark year - observed ####
fls <- list.files(preprocess_dir, 
                  recursive = T, pattern = outcome, full.names = T)
fls <- fls[grepl(outcome, fls) & 
             grepl("observed", fls) & 
             !grepl("cohAll|train|test", fls)]

fls1 <- fls[grepl(paste0("landmark_y1.coh"), fls)]
fls5 <- fls[grepl(paste0("landmark_y5.coh"), fls)]
fls10 <- fls[grepl(paste0("landmark_y10.coh"), fls)]
rm(fls)

combined_dir <- paste0(preprocess_dir, "cohAll/", outcome, "/observed/")
dir.create(combined_dir, recursive = T)

# Combine cohorts for each landmark
d10 <- fls10 %>%
  map_df(~ readRDS(.)) %>%
  mutate(across(all_of(c(cond_proc_features_all, med_features_all)), 
                coalesce0))
d5 <- fls5 %>%
  map_df(~ readRDS(.)) %>%
  mutate(across(all_of(c(cond_proc_features_all, med_features_all)), 
                coalesce0))
d1 <- fls1 %>%
  map_df(~ readRDS(.)) %>%
  mutate(across(all_of(c(cond_proc_features_all, med_features_all)), 
                coalesce0))
gc()

# define PatientIDs for train/test split
set.seed(12345)
d1_train <- d1 %>% sample_frac(0.7)
d1_test <- d1 %>% anti_join_quiet(d1_train)

trainIDs <- d1_train %>% distinct(PatientICN)
testIDs <- d1_test %>% distinct(PatientICN)

d5_train <- d5 %>% inner_join(trainIDs)
d5_test <- d5 %>% inner_join(testIDs)

d10_train <- d10 %>% inner_join(trainIDs)
d10_test <- d10 %>% inner_join(testIDs)

rm(d5, d10)
gc()

# Write out data snapshots separately for train/test sets
saveRDS(d10_train, 
        paste0(combined_dir, "landmark_y10.train.", outcome, ".observed.RDS"))
saveRDS(d10_test, 
        paste0(combined_dir, "landmark_y10.test.", outcome, ".observed.RDS"))
saveRDS(d5_train, 
        paste0(combined_dir, "landmark_y5.train.", outcome, ".observed.RDS"))
saveRDS(d5_test, 
        paste0(combined_dir, "landmark_y5.test.", outcome, ".observed.RDS"))
saveRDS(d1_train, 
        paste0(combined_dir, "landmark_y1.train.", outcome, ".observed.RDS"))
saveRDS(d1_test, 
        paste0(combined_dir, "landmark_y1.test.", outcome, ".observed.RDS"))
rm(d5_train, d5_test, d10_train, d10_test, d1_train, d1_test)
gc()


# Usling L1, generate a table 1 of cohort charateristics.
table1_start <- d1 %>%
  mutate(SmokingFactor = factor(SmokingFactor, 
                                levels = c("NEVERSMOKER", "FORMERSMOKER", 
                                           "CURRENTSMOKER", "UNKNOWN"))) %>%
  select(
    person_id = PatientICN, DmDx_first, Age_DmDx, Age_2000, SmokingFactor, 
    EVERSMOKER, Ethnicity, Race, Gender, MaritalStatus,
    `Years to first Event (or Censoring)` = CR_tte,
    `Years to Censoring` = censtime,
    event_CR,
    DeceasedFlag,
    `Years to Death (or Censoring)` = Deceased_tte,
    all_of(c(BBM_obj, cond_proc_features_all, med_features_all))
  ) %>%
  mutate(across(everything(), 
                function(x) factorize.i(x, ignore.failed = TRUE))) %>%
  group_by(event_CR)

# Process continuous variables ####

# Read in objects and get N measures
marker_files <- list.files(paste0(cohort_dir, "cohAll/biomarkers_combined/"), 
                           full.names = TRUE)
marker_names <- gsub(".*/|_combined.txt", "", marker_files)
marker_names

# Read in and process the combined biomarker data
for (i in 1:length(marker_names)) {
  print(paste0("Processing marker ", i, " of ", length(marker_names), ": ", 
               marker_names[i]))
  process_marker(marker_names[i], marker_files[i])
  gc()
}

# Summarize all the longitudinal biomarkers to a person-level table
tictoc::tic("Summarizing number of repeated measures per marker/person")
init <- data.frame(person_id = 0) %>% filter(is.na(person_id))
for (marker in marker_names) {
  print(paste0("Adding person-level summary for ", marker))
  tmp <- get_n_measures(marker)
  init <- full_join_quiet(init, tmp)
  rm(tmp, marker)
  gc()
}
tictoc::toc() 

rm(list = marker_names)
gc()

# Medians and IQRs for continuous variables
table1_numeric_iqr <- table1_start %>%
  select_if(is.numeric)
table1_numeric_iqr <- table1_numeric_iqr %>%
  mutate(event_CR = 99) %>% ## 99 as code for "Overall"
  rbind(table1_numeric_iqr) %>%
  summarize_all(iqrsumm) %>%
  t() %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(variable = rowname) %>%
  mutate(stat_type = "1.median_iqr") %>%
  filter(!(variable %in% c("event_CR", "DmDx_first", "person_id")))
names(table1_numeric_iqr)[2:5] <- c("No Events", "Renal Failure", 
                                    "Death without Renal Failure", "Overall")
table1_numeric_iqr

table1_numeric_iqr_sex <- table1_start %>%
  select_if(is.numeric) %>%
  left_join(table1_start %>% select(person_id, Gender))
table1_numeric_iqr_sex <- table1_numeric_iqr_sex %>%
  mutate(Gender = "Overall") %>%
  rbind(table1_numeric_iqr_sex) %>%
  group_by(Gender) %>%
  summarize_all(iqrsumm) %>%
  t() %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(variable = rowname) %>%
  mutate(stat_type = "1.median_iqr") %>%
  filter(!(variable %in% c("event_CR", "DmDx_first", "person_id", "Gender")))
names(table1_numeric_iqr_sex)[2:4] <- c("Female", "Male", "Overall")
table1_numeric_iqr_sex

# Percent missing for continuous variables
table1_numeric_NA <- table1_start %>%
  select_if(is.numeric)
table1_numeric_NA <- table1_numeric_NA %>%
  mutate(event_CR = 99) %>%
  rbind(table1_numeric_NA) %>%
  summarize_all(summcat_mask20_NAs) %>%
  t() %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(variable = rowname) %>%
  mutate(stat_type = "2.report_NAs") %>%
  filter(!(variable %in% c("event_CR", "DmDx_first", "person_id")))
names(table1_numeric_NA)[2:5] <- c("No Events", "Renal Failure", 
                                   "Death without Renal Failure", "Overall")
table1_numeric_NA


# # Summarize number of repeated measures to a group-level table
table1_numeric_nmeasures <- init %>%
  left_join(table1_start %>% select(person_id, event_CR))
table1_numeric_nmeasures <- table1_numeric_nmeasures %>%
  mutate(event_CR = 99) %>%
  rbind(table1_numeric_nmeasures) %>%
  group_by(event_CR) %>%
  summarize_all(iqrsumm) %>%
  t() %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(variable = rowname) %>%
  mutate(stat_type = "3.report_n_measures_y1p") %>%
  filter(!(variable %in% c("event_CR", "DmDx_first", "person_id")))
names(table1_numeric_nmeasures)[2:5] <- c("No Events", "Renal Failure", 
                                          "Death without Renal Failure", 
                                          "Overall")
table1_numeric_nmeasures


# # Summarize to a group-level table for sex-specific summaries
table1_numeric_nmeasures_sex <- init %>%
  left_join(table1_start %>% select(person_id, Gender))
table1_numeric_nmeasures_sex <- table1_numeric_nmeasures_sex %>%
  mutate(Gender = "Overall") %>%
  rbind(table1_numeric_nmeasures_sex) %>%
  group_by(Gender) %>%
  summarize_all(iqrsumm) %>%
  t() %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(variable = rowname) %>%
  mutate(stat_type = "3.report_n_measures_y1p") %>%
  filter(!(variable %in% c("event_CR", "DmDx_first", "person_id", "Gender")))
names(table1_numeric_nmeasures_sex)[2:4] <- c("Female", "Male", "Overall")
table1_numeric_nmeasures_sex


table1_categorical <- table1_start %>%
  select(where(is.factor) | where(is.character)) %>%
  mutate_if(is.character, as.factor)
table1_categorical <- table1_categorical %>%
  mutate(event_CR = 99) %>%
  rbind(table1_categorical) %>%
  pivot_longer(!event_CR, names_to = "variable") %>%
  mutate(value = as.character(value)) %>%
  group_by(event_CR, variable) %>%
  mutate(n = n()) %>%
  group_by(event_CR, variable, value) %>%
  summarise(smry = paste0(n(), " (", signif((n() / n) * 100, 3), "%)"), 
            n.val = n(), n.tot = unique(n)) %>%
  mutate(smry = ifelse(n.val < 20, 
                       paste0("<= 20 (<=", signif((20 / n.tot) * 100, 3), "%)"), 
                       smry)) %>%
  select(-n.val, -n.tot) %>%
  distinct() %>%
  filter(value != "0") %>% 
  mutate(stat_type = "1.report_categories") %>%
  pivot_wider(names_from = "event_CR", values_from = "smry")
names(table1_categorical)[4:7] <- c("No Events", "Renal Failure", 
                                    "Death without Renal Failure", "Overall")
table1_categorical

# Colnames addendum
table1_addendum <- table1_start %>%
  select(person_id, event_CR)
table1_addendum <- table1_addendum %>%
  mutate(event_CR = 99) %>%
  rbind(table1_addendum) %>%
  ungroup() %>%
  mutate(n.total = length(unique(person_id))) %>%
  group_by(event_CR) %>%
  summarise(`0.colname_addendum` = 
              paste0(n(), 
                     " (", signif(100 * n() / unique(n.total), 3), "%)")) %>%
  t() %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(variable = rowname) %>%
  filter(variable != "event_CR") %>% 
  mutate(stat_type = "0.table_colstats")
names(table1_addendum)[2:5] <- c("No Events", "Renal Failure", 
                                 "Death without Renal Failure", "Overall")
table1_addendum

# Colnames addendum for gender
table1_sex_addendum <- table1_start %>%
  select(person_id, Gender)
table1_sex_addendum <- table1_sex_addendum %>%
  mutate(Gender = "Overall") %>%
  rbind(table1_sex_addendum) %>%
  ungroup() %>%
  mutate(n.total = length(unique(person_id))) %>%
  group_by(Gender) %>%
  summarise(`0.colname_addendum` = 
              paste0(n(), 
                     " (", signif(100 * n() / unique(n.total), 3), "%)")) %>%
  t() %>%
  data.frame() %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(variable = rowname) %>%
  filter(variable != "Gender") %>% 
  mutate(stat_type = "0.table_colstats")
names(table1_sex_addendum)[2:4] <- c("Female", "Male", "Overall")
table1_sex_addendum

# Combine the components of the table1
table1_everything <- full_join(table1_numeric_iqr, table1_numeric_NA) %>%
  full_join(table1_categorical) %>%
  full_join(table1_numeric_nmeasures) %>%
  full_join(table1_addendum) %>%
  arrange(variable, stat_type, value) %>%
  select(variable, stat_type, value, everything())
# table1_everything %>% View(.)
fwrite(table1_everything, paste0(egress_dir, "table1_L1_KDI.csv"), 
       row.names = F)

# Combine the components of the sex-specific biomarker summaries
biomarkers_by_sex <- table1_numeric_iqr_sex %>%
  full_join(table1_numeric_nmeasures_sex) %>%
  full_join(table1_sex_addendum) %>%
  arrange(variable, stat_type) %>%
  select(variable, stat_type, everything())
fwrite(biomarkers_by_sex, paste0(egress_dir, "biomarkers_by_sex_L1_KDI.csv"), 
       row.names = F)


rm(
  init, biomarkers_by_sex, table1_addendum, table1_sex_addendum, 
  table1_everything, table1_numeric_iqr, table1_numeric_iqr_sex, 
  table1_numeric_NA, table1_numeric_nmeasures, table1_numeric_nmeasures_sex, 
  table1_categorical
)
gc()

## FEATURE extraction summaries

# Summarized results for conditions, procedures, medications
cond_proc_med_summary <- list.files(
  paste0(results_dir_export_details, "subcohorts/"),
  pattern = "cond_proc_med", full.names = T
) %>%
  map_df(~ readRDS(.)) %>%
  distinct() %>%
  filter(cohort >= 33)

cohort_ns <- cond_proc_med_summary %>%
  distinct(cohort, cohort_n) %>%
  filter(!is.na(cohort_n))
total_cohort_n <- sum(cohort_ns$cohort_n)

cond_proc_med_summary <- cond_proc_med_summary %>%
  group_by(domain, feature_name, code, code_vocabulary, Description, 
           drug_name, VAClassification) %>%
  summarise(n_records = sum(n_records), n_patients = sum(n_patients)) %>%
  mutate(n_cohort = total_cohort_n) %>%
  mutate(prop_patients = n_patients / n_cohort) %>%
  arrange(domain, feature_name, code_vocabulary, desc(n_records)) %>%
  filter(!is.na(n_records))

# Summarized results for continuous lab extractions
labs_cont_summary <- list.files(
  paste0(results_dir_export_details, "subcohorts/"),
  pattern = "labs_cont", full.names = T
) %>%
  map_df(~ readRDS(.)) %>%
  distinct() %>%
  group_by(domain, marker_name, LabChemTestName, lower, upper, source) %>%
  summarise(n_records = sum(n_records), n_patients = sum(n_patients)) %>%
  mutate(n_cohort = total_cohort_n) %>%
  mutate(prop_patients = n_patients / n_cohort) %>%
  arrange(domain, marker_name, desc(prop_patients))

# Summarized results for labs categories
labs_cat_summary <- list.files(
  paste0(results_dir_export_details, "subcohorts/"),
  recursive = T, pattern = "labs_cat", full.names = T
) %>%
  map_df(~ readRDS(.)) %>%
  distinct() %>%
  group_by(domain, marker_name, feature_name,
    lower = coalesce(lower_chr, as.character(lower)),
    upper = coalesce(upper_chr, as.character(upper))
  ) %>%
  summarise(n_records = sum(n_records), 
            n_patients = sum(n_patients), 
            n_cohort = sum(cohort_n)) %>%
  mutate(prop_patients = n_patients / n_cohort) %>%
  arrange(marker_name, feature_name, lower, upper, desc(n_records))

BBM_cont_cat_key <- labs_cat_summary %>%
  ungroup() %>%
  distinct(marker_name, feature_name)
saveRDS(BBM_cont_cat_key, 
        paste0(results_dir_export_details, "BBM_cont_cat_key.RDS"))

cond_proc_med_summary
labs_cont_summary
labs_cat_summary


data <- full_join(
  labs_cont_summary %>% mutate(
    lower = as.character(lower),
    upper = as.character(upper),
    feature_name = marker_name
  ),
  labs_cat_summary %>% mutate(
    feature_subcategory = feature_name,
    feature_name = marker_name
  )
) %>%
  full_join(cond_proc_med_summary) %>%
  distinct() %>%
  data.frame() %>%
  mutate(Description = tolower(Description)) %>%
  mutate(Description = gsub("\r", "", Description)) %>%
  mutate(VAClassification = ifelse(VAClassification == "", 
                                   NA, VAClassification)) %>%
  mutate(percent_of_patients = prop_patients * 100)

fwrite(data, 
       paste0(results_dir_export_details, "feature_extraction_summaries.txt"))
fwrite(data, 
       paste0(egress_dir, "feature_extraction_summaries.txt"))

# Ignore codes / meds with fewer than 100 patients to facilitate vizualization
ShinyInput <- data %>%
  data.frame() %>%
  filter(n_patients > 100 | n_patients == 0 | marker_name == "EGFR")

# Further clean certain drug names to facilitate vizualization
newg <- paste0("HUMULIN|HUMULN|HUMU|HUM |HUM$|NOVOLIN|INNOLET|PENFILL| N |", 
               " N$|LILY|NOVOLN|SOLOSTR|GLARGINE|ULTRALENTE|ULTRA LENTE|LENTE", 
               "| PEN | PEN$| ASPART | ASPART$| LISPRO | LISPRO$| L | L$| U |", 
               " U$| NPH | REG | REGULAR | R | R$| EC | EC$| VD | VD$| UD |", 
               " UD$| LE | LE$| CA | CA$| IJ$| IJ ")
ShinyInput <- ShinyInput %>%
  mutate(drug_name = gsub(newg, " ", drug_name)) %>% # Need to do more than once
  mutate(drug_name = gsub(newg, " ", drug_name)) %>%
  mutate(drug_name = gsub(newg, " ", drug_name)) %>%
  mutate(drug_name = gsub("[ ][ ]*", " ", drug_name)) %>% # internal spaces
  # spaces at beginning or end
  mutate(drug_name = gsub("^[ ][ ]*|[ ][ ]*$", "", drug_name)) 

DrugClass <- readRDS("/mnt/mvp_datacommons/rawCommons/C2023/Dim.Drugs.RDS")

VAClass_key <- DrugClass %>%
  dplyr::rename(VAClassification = DrugClassCode, 
                VAClassification_Description = DrugClassification) %>%
  distinct(VAClassification, VAClassification_Description) %>%
  filter(!grepl("INACTIVE|^ HEPARIN", VAClassification_Description))

ShinyInput <- left_join(ShinyInput, VAClass_key) %>%
  mutate(VAClass = paste0(VAClassification, "-", VAClassification_Description))

fwrite(ShinyInput, paste0(results_dir_export_details, "ShinyInput.txt"))
fwrite(ShinyInput, paste0(egress_dir, "ShinyInput.txt"))

print("End of Script")
