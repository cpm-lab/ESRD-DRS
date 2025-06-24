# This script generates the the diabetes cohort as detailed in the proposal
# using the modified Klompas algorithm, and extracts the relevant data for
# them (DRUGS, DIAGNOSES, BIOMARKERS, PROCEDURES, DERIVED VARIABLES AND DATASETS)


## SETUP ########################################################################
library(data.table)
library(tidyverse)
library(lubridate)
library(purrr)
library(tictoc)
library(RODBC)
library(dplyr)

# The sub-cohort number (0-99) is passed in as a command arg.
args <- commandArgs(trailingOnly = TRUE)
cohort <- as.numeric(args[1])

#Run from parent directory
source("config_mnt.R")
source("functions.R")

print(C2023_PULL_DATE)

# Descriptions of codes - for summary-level results of data extraction.
ICD10Dim <- fread(paste0(results_dir_dim, "ICD10Descriptions.csv")) %>%
  mutate(code_vocabulary = "ICD10") %>%
  dplyr::rename(ICDCode = ICD10Code, Description = ICD10Description)
ICD9Dim <- fread(paste0(results_dir_dim, "ICD9Descriptions.csv")) %>%
  mutate(code_vocabulary = "ICD9") %>%
  mutate(ICD9Code = tolower(ICD9Code)) %>%
  dplyr::rename(ICDCode = ICD9Code, Description = ICD9Description)
ICDdict <- rbind(ICD10Dim, ICD9Dim)
rm(ICD10Dim, ICD9Dim)
CPTDim <- fread(paste0(results_dir_dim, "CPTDescriptions.csv")) %>%
  mutate(code_vocabulary = "CPT") %>%
  filter(CPTDescription != "") %>%
  group_by(CPTCode) %>%
  slice(1) %>%
  ungroup()
ICD9ProcDim <- fread(paste0(results_dir_dim, "ICD9ProcedureDescriptions.csv"), 
                     keepLeadingZeros = TRUE) %>%
  mutate(code_vocabulary = "ICD9Procedure")
ICD10ProcDim <- fread(paste0(results_dir_dim, "ICD10ProcedureDescriptions.csv"), 
                      keepLeadingZeros = TRUE) %>%
  mutate(code_vocabulary = "ICD10Procedure")

LabDim <- readRDS(paste0(dim_dir, "DimLab8.RDS")) %>%
  mutate(Consensus = tolower(Consensus))

# Basic Demographic Info
demo <- readRDS(paste0(C2023dir, "DemoU.", cohort, ".RDS"))

outlier <- function(x) {
  rng <- fivenum(x)[c(2, 4)]
  iqr <- abs(rng[2] - rng[1])
  outlier <- (x > rng[2] + 3 * iqr) | (x < rng[1] - 3 * iqr)
  return(outlier)
}

# Flowchart data to track cohort size attrition
flowchart_data <- data.frame(step = "Initial", n = npat(demo))

# Process demographic info
demo <- demo %>%
  select(PatientICN:Ethnicity) %>% # Why isn't MaritalStatus time-dependent?
  mutate(
    BirthDate = decimal_date(as.Date(BirthDateTime)) - 2000,
    DeathDate = just_date(DeathDateTime)
  ) %>%
  select(-BirthDateTime, -DeathDateTime) %>%
  mutate(
    Ethnicity = factor(
      case_match(Ethnicity,
        c("DECLINED TO ANSWER", "UNKNOWN BY PATIENT", NA) ~ "UNKNOWN",
        "HISPANIC OR LATINO" ~ "HIS",
        "NOT HISPANIC OR LATINO" ~ "NHIS",
        .default = Ethnicity
      ),
      levels = c("NHIS", "HIS", "UNKNOWN")
    ),
    Race = factor(
      case_match(Race,
        "WHITE NOT OF HISP ORIG" ~ "WHITE",
        c("DECLINED TO ANSWER", "UNKNOWN BY PATIENT", NA) ~ "UNKNOWN",
        "BLACK OR AFRICAN AMERICAN" ~ "BLACK",
        "AMERICAN INDIAN OR ALASKA NATIVE" ~ "AIAN",
        "NATIVE HAWAIIAN OR OTHER PACIFIC ISLANDER" ~ "NHPI",
        .default = Race
      ),
      levels = c("WHITE", "BLACK", "ASIAN", "AIAN", "NHPI", "UNKNOWN")
    ),
    MaritalStatus = factor(
      case_match(MaritalStatus,
        "WIDOW/WIDOWER" ~ "WIDOWED",
        c("*Missing*", "UNKNOWN", "QUESTIONABLE", NA) ~ "UNKNOWN",
        c("DIVORCED", "SEPARATED") ~ "DIVORCED_SEPARATED",
        c("SINGLE", "NEVER MARRIED", "SINGLE *DO NOT USE*") ~ "NEVER_MARRIED",
        "COMMON-LAW" ~ "MARRIED",
        .default = MaritalStatus
      ),
      levels = c("MARRIED", "DIVORCED_SEPARATED", 
                 "NEVER_MARRIED", "WIDOWED", "UNKNOWN")
    ),
    Gender = factor(Gender, levels = c("M", "F", NA))
  )

npat(demo)

# Summarize this cohort's age
coh_years_demo <- demo %>%
  filter(!(outlier(BirthDate))) %>%
  summarise(
    min_DOB = min(BirthDate) + 2000,
    median_DOB = median(BirthDate) + 2000,
    max_DOB = max(BirthDate) + 2000
  ) %>%
  mutate(
    min_age_pulldate = C2023_PULL_DATE - max_DOB + 2000,
    median_age_pulldate = C2023_PULL_DATE - median_DOB + 2000,
    max_age_pulldate = C2023_PULL_DATE - min_DOB + 2000, cohort
  )

coh_years_demo

results_dir_export_details <- paste0(results_dir_export_details, "subcohorts/")
dir.create(results_dir_export_details)
saveRDS(coh_years_demo, 
        paste0(results_dir_export_details, "cohort.ages.coh", cohort, ".RDS"))

# Don't allow missingness for some fields
demo <- demo %>%
  filter(!is.na(BirthDate) & !is.na(Gender))

# Update flowchart
flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Demographic filters: No missing age or gender",
    n = npat(demo)
  )
)

# ADD MIN YOB
MIN_YOB <- 1938
demo <- demo %>%
  filter(BirthDate >= (MIN_YOB - 2000)) %>%
  mutate(death_censor_date = C2023_PULL_DATE) 
  # censor date for death is different than for conditions, 
  # since visits are not required.

# Update flowchart
flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Born in 1938 or later",
    n = npat(demo)
  )
)

demo_ids <- demo %>% distinct(PatientICN)
nrow(demo_ids)

if (nrow(demo_ids) == 0) {
  saveRDS(
    flowchart_data %>% mutate(cohort),
    paste0(results_dir_export_details, "flowchart_data.", cohort, ".RDS")
  )
  stop("No patients left after filtering")
}

# For each filter, we will export the details of how it was extracted, what 
# patterns were used, etc. Initialize:
extraction_details_export <- data.frame(feature_name = "") %>% 
  filter(feature_name != "")

# We will also export the 'results', i.e. what codes were matched by the 
# patterns and how many in each subcohort. Initialize:
extraction_results_export <- data.frame(cohort)

## READ IN ICD CODES  ##########################################################

# Read in the various sources of diagnosis data: ICD9 and ICD10 from outpatient, 
# inpatient, inpatient discharge, fee-based tables 
# Note that some tables do not exist for some cohorts
read_raw_Dx <- function(file_prefix, cohort, columns) {
  flnm <- paste0(C2023dir, file_prefix, cohort, ".RDS")
  if (file.exists(flnm)) {
    d <- readRDS(flnm)
    d <- d[, ..columns]
  } else {
    warning(paste0(flnm, " does not exist, creating an empty data frame"))
    d <- data.frame(matrix(ncol = 3, nrow = 0))
  }
  names(d) <- c("PatientICN", "dt", "ICDCode")
  d <- d %>%
    mutate(PatientICN = as.numeric(PatientICN), dt = as.numeric(dt)) %>%
    inner_join_quiet(demo_ids) %>%
    anti_join_quiet(data.frame(
      ICDCode = c("*Unknown at this time*", "*Missing*"))) %>%
    mutate(dt = just_date(dt)) %>% # date correction and drop time
    # Visit dates could not have been after the date the data was pulled
    filter(dt <= C2023_PULL_DATE) 
  return(d)
}


Fx9 <- read_raw_Dx("fee/FeeService.Dx9.", cohort, 
                   c("PatientICN", "InitialTreatmentDateTime", "ICDCode"))
Fx10 <- read_raw_Dx("fee/FeeService.Dx10.", cohort, 
                    c("PatientICN", "InitialTreatmentDateTime", "ICD10Code"))
Dx9a <- read_raw_Dx("outpat/oDx9.", cohort, 
                    c("PatientICN", "VisitDateTime", "ICDCode"))
Dx10a <- read_raw_Dx("outpat/oDx10.", cohort, 
                     c("PatientICN", "VisitDateTime", "ICD10Code"))
Dx9b <- read_raw_Dx("inpat/iDx9.", cohort, 
                    c("PatientICN", "DischargeDateTime", "ICDCode"))
Dx10b <- read_raw_Dx("inpat/iDx10.", cohort, 
                     c("PatientICN", "DischargeDateTime", "ICD10Code"))
Dx9c <- read_raw_Dx("inpat/idDx9.", cohort, 
                    c("PatientICN", "AdmitDateTime", "ICDCode"))
Dx10c <- read_raw_Dx("inpat/idDx10.", cohort, 
                     c("PatientICN", "AdmitDateTime", "ICD10Code"))

# Change IC9 letters (V, E codes) to lower (external sources of injury, etc)
# This is so that we can query ICD9 and ICD10 codes together without the 
# chance of mixing them up
Dx9a <- Dx9a %>% mutate(ICDCode = tolower(ICDCode))
Dx9b <- Dx9b %>% mutate(ICDCode = tolower(ICDCode))
Dx9c <- Dx9c %>% mutate(ICDCode = tolower(ICDCode))
Fx9 <- Fx9 %>% mutate(ICDCode = tolower(ICDCode))

# Add sources
# Combine Outpatient Dx only:
Dx.o <- base::rbind(Fx9, Fx10, Dx9a, Dx10a) %>%
  mutate(Source = "OutpatDx")

# Combine inpatient Dx only:
Dx.i <- base::rbind(Dx9b, Dx9c, Dx10b, Dx10c) %>%
  mutate(Source = "InpatDx")

# Combine diagnosis codes from different sources
Dx <- base::rbind(Dx.o, Dx.i)

# Find patients with Dx before birth or after death (by more than 60 days)
# These people either have incorrect death records or incorrect PatientICN 
# mapping, or very late/incorrect EHR entry
bad_ICNs <- demo %>%
  select(PatientICN, BirthDate, DeathDate) %>%
  full_join_quiet(Dx) %>%
  filter(dt < BirthDate | days_between(DeathDate, dt) > 60) %>%
  distinct(PatientICN)
npat(bad_ICNs)

# Take out the bad patientIDs
Dx <- Dx %>% anti_join_quiet(bad_ICNs)
Dx.i <- Dx.i %>% anti_join_quiet(bad_ICNs)
Dx.o <- Dx.o %>% anti_join_quiet(bad_ICNs)
demo <- demo %>% anti_join_quiet(bad_ICNs)
demo_ids <- demo %>% distinct(PatientICN)


# Clear memory
rm(Fx9, Fx10, Dx9a, Dx9b, Dx9c, Dx10a, Dx10b, Dx10c, bad_ICNs)
gc()

## PRELIMINARY: EXTRACT DM RECORDS, BEGIN KLOMPAS, 1st AND LAST DX #############

# Extract DM records *including inpatient*, using inner join to get
# DMtypes (Type 1, 2, Unspecified) and determine the first date of any DM code
# in/outpatient, T1d/T2d/unspecified type (Klompas considers outpatient codes only)

# Do this within a function so that we can easily re-run it after the final 
# cohort definition (otherwise percentages of patients will be >100%)

DM_function <- function() {
  DM_pattern <<- "^250|^362.0[1-7]|^366.41|^E1[013]|^E08|^O24[0138]"
  DM <<- extract.detail.icd(Dx, "DM", DM_pattern)

  # Define type 1 vs type 2 vs unspecified type codes
  DM_type_key <<- DM %>%
    distinct(ICDCode) %>%
    left_join_quiet(ICDdict) %>%
    mutate(DM_type = case_when(
      grepl("type II|type 2", Description, ignore.case = TRUE) ~ "T2D",
      grepl("type I|type 1|juvenile", Description, ignore.case = TRUE) ~ "T1D",
      TRUE ~ "Unspecified"
    ))
  # Add the DM type key to the extraction summary
  extraction_results_export <<- left_join_quiet(
    extraction_results_export,
    DM_type_key %>% select(code = ICDCode, subclass = DM_type)
  )
}
# Run it
DM_function()

# Get date of first DM Dx code
DM.first <- DM %>%
  group_by(PatientICN) %>%
  summarise(DmDxCode_first = min(dt, na.rm = TRUE))
npat(DM.first)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Patients with 1+ Diabetes Code",
    n = npat(DM.first)
  )
)

if (npat(DM.first) == 0) {
  saveRDS(
    flowchart_data %>% mutate(cohort),
    paste0(results_dir_export_details, "flowchart_data.", cohort, ".RDS")
  )
  stop("No patients left after filtering")
}

# Get the first and last date of ANY diagnosis code
LastDx <- Dx %>%
  filter(!is.na(dt)) %>%
  group_by(PatientICN) %>%
  summarise(
    LastDx = max(dt),
    FirstDx = min(dt)
  )

# Extract DM OUTPATIENT records only and add DM type key
DM.ro <- Dx.o[grep(DM_pattern, ICDCode), ] %>%
  left_join_quiet(DM_type_key) %>%
  distinct()
npat(DM.ro)

# Start to implement Klompas algorithm - portion relating to ICD codes
# T1D: patients with over 2 outpatient T1D codes, with T1D codes being >=50% 
# of their outpatient DM records
t1d_icd_qualify <- DM.ro %>%
  group_by(PatientICN) %>%
  filter(any(DM_type == "T1D")) %>%
  summarise(
    nT1Dcode = sum(DM_type == "T1D"),
    propT1Dcode = mean(DM_type == "T1D"),
    T1D_first = min(dt[DM_type == "T1D"])
  ) %>%
  filter(nT1Dcode >= 2 & propT1Dcode >= .5)
npat(t1d_icd_qualify)

# patients with 2 or more outpatient T2D codes 30+ days apart.
t2d_icd_qualify <- DM.ro %>%
  filter(DM_type %in% c("T2D", "Unspecified")) %>%
  group_by(PatientICN) %>%
  summarise(
    ncode = n(),
    days_dif = days_between(min(dt), max(dt)),
    T2D_first = min(dt)
  ) %>%
  filter(ncode >= 2 & days_dif >= 30)
npat(t2d_icd_qualify)

# Preliminary set of IDS to be further subset later with medications data.
prelim_ids <- base::rbind(
  t2d_icd_qualify[, "PatientICN"],
  t1d_icd_qualify[, "PatientICN"]
) %>%
  distinct()

# Shrink the tables because memory becomes an issue for the larger sub-cohorts.
Dx <- Dx %>% inner_join_quiet(prelim_ids)
Dx.o <- Dx.o %>% inner_join_quiet(prelim_ids)
Dx.i <- Dx.i %>% inner_join_quiet(prelim_ids)
demo <- demo %>%
  inner_join_quiet(prelim_ids) %>%
  left_join_quiet(DM.first) %>%
  left_join_quiet(LastDx)
demo_ids <- demo %>% distinct(PatientICN)
rm(DM.ro, DM, DM.first, LastDx, DM_type_key)
gc()

## READ IN MEDICATIONS, REFINE KLOMPAS ########################################

# Medications were separated by class; join them all together
CN <- readRDS(paste0(C2023dir, "meds/Rx.CN.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids)
CV <- readRDS(paste0(C2023dir, "meds/Rx.CV.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids)
A <- readRDS(paste0(C2023dir, "meds/Rx.A.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids)
HS <- readRDS(paste0(C2023dir, "meds/Rx.HS.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids)
oth <- readRDS(paste0(C2023dir, "meds/Rx.Oth.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids)
FeeRx <- readRDS(paste0(C2023dir, "fee/Fee.Rx.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  dplyr::rename(
    FillDateTime = PrescriptionFilledDate,
    QtyNumeric = QuantityNumeric
  ) %>%
  mutate(DrugNameWithoutDose = toupper(DrugName)) %>%
  mutate(
    LocalDrugNameWithDose = DrugName,
    DrugNameWithDose = DrugName
  ) %>% # Fill in fields so that we can still pattern match with available name
  select(-c(Sta3n, DrugName, Strength, Quantity))
BCMAfile <- paste0(C2023dir, "meds/BCMA.", cohort, ".RDS")
IVfile <- paste0(C2023dir, "meds/IV.", cohort, ".RDS")

# Combine main
Rx <- base::rbind(CN, CV, A, HS, oth) %>% mutate(Source = "Outpat")
npr(Rx)

Rx <- base::rbind(Rx, FeeRx %>% mutate(Source = "Fee"), fill = TRUE)
npr(Rx)

# Add IV and BMA (Bar coded medication administration for hospital patients) 
if (file.exists(BCMAfile)) {
  BCMA <- readRDS(BCMAfile) %>%
    inner_join_quiet(prelim_ids) %>%
    dplyr::rename(FillDateTime = ActionDateTime, QtyNumeric = DosesGiven) %>%
    mutate(DrugNameWithDose = LocalDrugNameWithDose) %>%
    mutate(Source = "BCMA") %>%
    select(-DosesOrdered, -UnitOfAdministration)
  Rx <- base::rbind(Rx, BCMA, fill = TRUE)
} else {
  print(paste0("No BCMA file for cohort ", cohort))
}

npr(Rx)

if (file.exists(IVfile)) {
  IV <- readRDS(IVfile) %>%
    inner_join_quiet(prelim_ids) %>%
    dplyr::rename(
      DrugNameWithDose = IVAdditiveIngredientPrintName,
      DrugNameWithoutDose = PrimaryDrug,
      FillDateTime = StartDateTime
    ) %>%
    mutate(LocalDrugNameWithDose = DrugNameWithDose) %>%
    mutate(Source = "IV") %>%
    select(-Strength, -ChemotherapyType, -DosageOrdered)
  Rx <- base::rbind(Rx, IV, fill = TRUE)
} else {
  print(paste0("No IV file for cohort ", cohort))
}

npr(Rx)

# Percentages by source:
signif(prop.table(table(Rx$Source)) * 100, 3)

# Define patterns for study drugs that may be placebo
placebo_patterns <- 
  "PLACEBO|OR PBO|/PBO|STUDY|TRIAL|COOP #|CSP #|INV-|INV[ :#]|ACCORD|IRB#"

tic("Processing the Rx Data")
Rx <- Rx %>%
  # Don't count if returned to stock
  filter(is.na(ReturnedToStockDateTime)) %>%
  # Need to have a date
  filter(!is.na(FillDateTime) | !is.na(ReleaseDateTime)) %>%
  # Use ReleaseDateTime if FillDateTime is missing
  mutate(dt = coalesce(FillDateTime, ReleaseDateTime)) %>%
  select(-c(ReturnedToStockDateTime, FillDateTime, 
            ReleaseDateTime, LocalDrugSID)) %>%
  # Filter out possible placebo / study drugs / investigational drugs 
  # chapter for investigational drugs
  filter(!grepl("^IN", VAClassification) | is.na(VAClassification)) %>% 
  filter(!grepl(placebo_patterns, LocalDrugNameWithDose) &
    !grepl(placebo_patterns, DrugNameWithDose) &
    !grepl(placebo_patterns, DrugNameWithoutDose)) %>%
  # Also filter out chapters Diagnostic agents, pharmaceutical aids/reagents, 
  # prosthetics/supplies/devices, and anti-septics
  filter(!grepl("^DX|^PH|^XA|^AS", VAClassification) | 
           is.na(VAClassification)) %>%
  filter(!is.na(LocalDrugNameWithDose) & LocalDrugNameWithDose != "*Missing*") %>%
  # Use LocalDrugNameWithDose for pattern matching if DrugNameWithDose missing
  mutate(DrugNameWithDose = ifelse(DrugNameWithDose == "*Missing*" | 
                                     is.na(DrugNameWithDose),
    LocalDrugNameWithDose, DrugNameWithDose
  )) %>%
  mutate(DrugNameWithDose = gsub("  ", " ", DrugNameWithDose)) 
toc()
npr(Rx)


# For those that are missing the DrugNameWithoutDose, try to strip away the 
# dosage information manually
Rx_WithoutDoseMissing <- Rx %>% 
  filter(is.na(DrugNameWithoutDose) | DrugNameWithoutDose == "*Missing*")
npr(Rx_WithoutDoseMissing)

#For summary purposes
tic("Trying to fill in missing DrugNameWithoutDose") 
patterns_to_sub <- 
  paste("SIMPLE|SUSPENSION|SUSP|ORAL| HCL|SOLUBLE|SOLUABLE|SOLN|ELIXER|ELIX", 
        "PATCH|LOZENGE|CARTRIDGE|HUMAN|POWDER|SPRAY|INHALER|INHL|INH", 
        " FREE | FREE$|INJECTOR|SUNSCREEN|SUBLINGUAL|NASAL|ACTUATION", 
        "PROTECTIVE|SHAMPOO|OINTMENT|OINT|CREAM| TOP | TOP$|LOTION|CLEANSING", 
        "FOAM|PACK | PACK$| PK | PK$| PCK | PCK$| PKT | PKT$| KIT ",
        " KIT$|CHEWABLE|POWDER|PWDR|PWD| CAN$| CAN |TITRATION|WEEK", 
        "STARTER| START | START$|PROGRAM|PATIENT| BAR | BAR$|TABLETS|TABLET", 
        "TABS|TABL| TAB | TAB$|TAB/CAP|HUMULIN|HUMULN|HUMU|HUM |HUM$|NOVOLIN", 
        "INNOLET|PENFILL|LILY|NOVOLN|SOLOSTR|GLARGINE|ULTRALENTE|ULTRA LENTE", 
        "LENTE| ASPART | ASPART$| LISPRO | LISPRO$| NPH | NPH$| REG | REGULAR ", 
        " R | R$|CAPSULES|CAPSULE| CAP | CAP$|CAPS|BRAND NAME|BRAND| CA | CA$", 
        " SA | SA$| SL | SR | SO | N | N$| BR | NA | NA$| D | D$|GM$| GM | PO ", 
        " PO$| INJ | INJ$| IJ$| IJ | VIAL | VIAL$|SYRINGE|MLPEN|ML PEN| PEN ", 
        " PEN$| HRS|OPHTHALMIC|OPHTH| OPH| AQ$| AQ | XR | XR$| OTIC | OTIC$", 
        " GEL | GEL$|PERINEAL|CLEANSER|SYRUP| ML | ML$| LIQ | LIQ$|CHEW", 
        "POCKETHALER| A | B | SYR | SYR$| UNIT | UNIT$| UNT | UNT$|PREPKET", 
        "LIQUID|AEROSOL| NON |AQUEOUS| DOSE$| DOSE | ADJ ",
        " L | L$| U | U$| EC | EC$| VD | VD$| UD | UD$| LE | LE$",
  sep = "|"
)

Rx_addedWithoutDose <- Rx_WithoutDoseMissing %>%
  mutate(DrugNameWithoutDose = 
           gsub("\\*|\\(.*\\)|#|,|\\.|%|\\+|<|-|&|/", " ", DrugNameWithDose)) %>%
  mutate(DrugNameWithoutDose = 
           gsub("^AAA|XXX|XX|xxx|xx|^ZZ[Z]*[ ]*|^z[z]*[ ]*|^Zz|^Z |^z ", " ", 
                DrugNameWithoutDose)) %>%
  mutate(DrugNameWithoutDose = 
           gsub("[ ]*[0-9][0-9]*[ ]*", " ", DrugNameWithoutDose)) %>%
  mutate(DrugNameWithoutDose = toupper(DrugNameWithoutDose)) %>%
  mutate(DrugNameWithoutDose = 
           gsub(" MG[ /]*| MG/ML[ /]*| ML[ /]*| MCG[ /]*|MGTAB|MGCAP", " ", 
                DrugNameWithoutDose)) %>%
  mutate(DrugNameWithoutDose = 
           gsub(patterns_to_sub, " ", DrugNameWithoutDose)) %>%
  # May have to do  twice due to shared padding spaces /ends of lines
  mutate(DrugNameWithoutDose = 
           gsub(patterns_to_sub, " ", DrugNameWithoutDose)) %>%
  mutate(DrugNameWithoutDose = 
           gsub("APAP", "ACETAMINOPHEN", DrugNameWithoutDose)) %>%
  mutate(DrugNameWithoutDose = 
           gsub("[ ][ ]*", " ", DrugNameWithoutDose)) %>% # internal spaces
  mutate(DrugNameWithoutDose = # spaces at beginning or end
           gsub("^[ ][ ]*|[ ][ ]*$", "", DrugNameWithoutDose)) 
  
toc()

Rx <- anti_join_quiet(Rx, Rx_WithoutDoseMissing)

Rx <- Rx %>%
  full_join_quiet(Rx_addedWithoutDose %>% mutate(ProcessedDoseFlag = "Y")) %>%
  mutate(dt = just_date(dt)) %>% # date correction and drop time
  filter(dt <= C2023_PULL_DATE)
rm(CN, CV, oth, HS, A, FeeRx, BCMA, IV, Rx_addedWithoutDose, 
   Rx_WithoutDoseMissing, BCMAfile, IVfile, patterns_to_sub, placebo_patterns)
gc()

# Identify any additional questionable records/ patient IDs.
bad_ICNs <- demo %>%
  select(PatientICN, BirthDate, DeathDate) %>%
  full_join_quiet(Rx) %>%
  filter(dt < BirthDate | days_between(DeathDate, dt) > 60) %>%
  filter(!is.na(dt)) %>%
  distinct(PatientICN)
npat(bad_ICNs)

# Remove patients with potentially incorrect birth/death dates
Dx <- Dx %>% anti_join_quiet(bad_ICNs)
Dx.o <- Dx.o %>% anti_join_quiet(bad_ICNs)
Dx.i <- Dx.i %>% anti_join_quiet(bad_ICNs)
Rx <- Rx %>% anti_join_quiet(bad_ICNs)
demo <- demo %>% anti_join_quiet(bad_ICNs)
demo_ids <- demo %>% distinct(PatientICN)
t1d_icd_qualify <- t1d_icd_qualify %>% anti_join_quiet(bad_ICNs)
t2d_icd_qualify <- t2d_icd_qualify %>% anti_join_quiet(bad_ICNs)
rm(prelim_ids)

# Extract Medications needed for finishing the Klompas algorithm
# Put them in a function to rerun numbers after cohort is final
DM_meds_function <- function() {
  Insulin <<- extract.detail.Rx(Rx, "Insulin", "HS501", "VAClassification")
  Glucagon <<- extract.detail.Rx(Rx, "Glucagon", "GLUCAGON", "DrugNameWithoutDose")
  Metformin <<- extract.detail.Rx(Rx, 
                                  "Metformin", "METFORMIN", "DrugNameWithoutDose", 
                                  exact_match = TRUE)
  GLP1 <<- extract.detail.Rx(
    Rx, "GLP1", "GLUTIDE|EXENATIDE|LIXISENATIDE|SEMAGLUTIDE|TIRZEPATIDE", 
    "DrugNameWithDose")
  DPP4 <<- extract.detail.Rx(Rx, "DPP4", "GLIPTIN", "DrugNameWithDose")
  SGLT2 <<- extract.detail.Rx(Rx, "SGLT2", "FLOZIN", "DrugNameWithDose")
  Sulfonylurea <<- extract.detail.Rx(
    Rx, "Sulfonylurea",
    "PIRIDE|PIZIDE|GLYBURIDE|TOLAZAMIDE|TOLBUTAMIDE|CHLORPROPAMIDE", 
    "DrugNameWithDose"
  )
  Thiazolidinedione <<- extract.detail.Rx(
    Rx, "Thiazolidinedione", "GLITAZONE", "DrugNameWithDose")
  Other_GlucoseLowering <<- extract.detail.Rx(
    Rx, "Other_GlucoseLowering", "MIGLITOL|REPAGLINIDE|ACARBOSE|NATEGLINIDE", 
    "DrugNameWithDose"
  )
  OralGlucoseLoweringExclMetformin <<- rbind(
    DPP4, SGLT2, Sulfonylurea, Thiazolidinedione, Other_GlucoseLowering)
}
# Run it
DM_meds_function()

first_Insulin <- Insulin %>%
  group_by(PatientICN) %>%
  summarise(first_Insulin = min(dt))
first_Glucagon <- Glucagon %>%
  group_by(PatientICN) %>%
  summarise(first_glucagon = min(dt))
first_Metformin <- Metformin %>%
  group_by(PatientICN) %>%
  summarise(first_Metformin = min(dt))
first_GLP1 <- GLP1 %>%
  group_by(PatientICN) %>%
  summarise(first_GLP1 = min(dt))
first_OralGlucoseLoweringExclMetformin <- OralGlucoseLoweringExclMetformin %>%
  group_by(PatientICN) %>%
  summarise(first_OralGlucoseLoweringExclMetformin = min(dt))

# Qualify for T1D definition (see modified Klompas algrothm in protocol)
# 1. Must be on insulin.
t1d_insulin <- inner_join_quiet(t1d_icd_qualify, first_Insulin %>% 
                                  distinct(PatientICN))
npat(t1d_insulin)

# 2. Either uses glucagon or does NOT use any oral DM meds
t1d_qualify <- left_join_quiet(t1d_insulin, first_Glucagon) %>%
  left_join_quiet(first_OralGlucoseLoweringExclMetformin) %>%
  left_join_quiet(first_GLP1) %>%
  filter(!is.na(first_glucagon) | 
           (is.na(first_OralGlucoseLoweringExclMetformin) & is.na(first_GLP1)))
npat(t1d_qualify)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Patients meeting criteria for T1D",
    n = npat(t1d_qualify)
  )
)

# T2D - exclude those that qualify for T1D
t2d_qualify <- t2d_icd_qualify %>%
  anti_join_quiet(t1d_qualify)
npat(t2d_qualify)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Patients meeting criteria for T2D",
    n = npat(t2d_qualify)
  )
)

# Preliminary cohort: includes T2 and T1.  
# Move up the first date of DM based on DM drugs(insulin, 
# oraldm except metformin, glucagon, GLP1)
prelim_cohort <- full_join_quiet(
  t1d_qualify %>% mutate(T1D = 1),
  t2d_qualify %>% mutate(T1D = 0)
) %>%
  dplyr::select(-c(ncode, days_dif, nT1Dcode, propT1Dcode, 
                   first_glucagon, first_OralGlucoseLoweringExclMetformin, 
                   first_GLP1)) %>%
  left_join_quiet(first_Insulin) %>%
  left_join_quiet(first_Glucagon) %>%
  left_join_quiet(first_OralGlucoseLoweringExclMetformin) %>%
  left_join_quiet(first_GLP1) %>%
  left_join_quiet(first_Metformin) %>%
  left_join_quiet(demo) %>%
  mutate(DmDx_first = pmin(DmDxCode_first, first_Insulin, 
                           first_OralGlucoseLoweringExclMetformin, first_GLP1,
    first_glucagon,
    na.rm = TRUE
  )) %>%
  mutate(Age_DmDx = DmDx_first - BirthDate)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Patients meeting criteria for T1D or T2D",
    n = npat(prelim_cohort)
  )
)

if (npat(prelim_cohort) == 0) {
  saveRDS(
    flowchart_data %>% mutate(cohort),
    paste0(results_dir_export_details, "flowchart_data.", cohort, ".RDS")
  )
  stop("No patients left after filtering")
}


prelim_cohort <- prelim_cohort %>%
  filter(Age_DmDx >= 21)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Patients diagnosed with DM after age 21",
    n = npat(prelim_cohort)
  )
)

prelim_cohort <- prelim_cohort %>%
  filter(LastDx - FirstDx >= 1) %>%
  select(-T1D_first, -T2D_first)
npat(prelim_cohort)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Patients with 1+ year of VHA use",
    n = npat(prelim_cohort)
  )
)

rm(
  demo, demo_ids, first_Insulin, first_Glucagon, 
  first_OralGlucoseLoweringExclMetformin, first_GLP1, first_Metformin,
  t1d_icd_qualify, t2d_icd_qualify, t1d_qualify, t2d_qualify, t1d_insulin, 
  bad_ICNs
)

# Prior 2yr outpatient usage - filter to those with newly diagnosed diabetes
Dm_p2 <- Dx.o %>%
  inner_join_quiet(prelim_cohort %>% dplyr::select(PatientICN, DmDx_first)) %>%
  mutate(years_before_DM = round(dt - DmDx_first, 0)) %>%
  filter(years_before_DM >= -2 & years_before_DM < 0) %>%
  distinct(PatientICN, years_before_DM) %>%
  group_by(PatientICN) %>%
  filter(sum(years_before_DM) == -3) %>%
  distinct(PatientICN)

prelim_cohort <- inner_join_quiet(prelim_cohort, Dm_p2)
prelim_ids <- prelim_cohort[, "PatientICN"]
npat(prelim_cohort)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Patients with 2 years of outpatient data prior to first DM code/med",
    n = npat(prelim_cohort)
  )
)
rm(Dm_p2, Dx.o, Dx.i)
gc()

# Trim down large objects to free up memory; the rest will be trimmed after 
# cohort definition is finalized
Dx <- Dx %>% inner_join_quiet(prelim_ids)
Rx <- Rx %>% inner_join_quiet(prelim_ids)
gc()

## READ IN LABS, DEFINE DM COHORT  ############################################
# Require 1+ LDL, A1c, and serum/blood creatinine after the first DM.
# LIPIDS AND SUGARS
LabL <- readRDS(paste0(C2023dir, "labs/Lipid.Sugars.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# PATHOGENS AND CANCER
LabP <- readRDS(paste0(C2023dir, 
                       "labs/Pathogens.Cancer.GIAx.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# KIDNEY AND PROTEIN
LabK <- readRDS(paste0(C2023dir, 
                       "labs/M.kidney.protein.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# ELECTROLYTES
LabMe <- readRDS(paste0(C2023dir, 
                        "labs/M.electrolytes.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# ENZYMES AND HORMONES
LabE <- readRDS(paste0(C2023dir, 
                       "labs/Enzymes.hormones.Drugs.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# RBC / IRON / HB
LabH <- readRDS(paste0(C2023dir, 
                       "labs/H.RBC.iron.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# WBC
LabW <- readRDS(paste0(C2023dir, 
                       "labs/H.WBC.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# OTHER CBC
LabC <- readRDS(paste0(C2023dir, 
                       "labs/H.CBC.coag.o.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# protein and urine
LabU <- readRDS(paste0(C2023dir, 
                       "labs/M.protein.urine.vital.o.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()
# OTHER
LabOth <- readRDS(paste0(C2023dir, 
                         "labs/Other.", cohort, ".RDS")) %>%
  inner_join_quiet(prelim_ids) %>%
  select(-c(RefLow, RefHigh, Abnormal, LabTestType))
gc()

# Combine labs
Labs <- rbind(
  LabL, LabP, LabK, LabMe, LabE, LabH, LabW, LabC, LabU, LabOth) %>%
  mutate(Consensus = tolower(Consensus)) %>%
  dplyr::rename(dt = LabChemSpecimenDateTime, 
                value = LabChemResultNumericValue) %>%
  filter(!is.na(dt))
rm(LabL, LabP, LabK, LabMe, LabE, LabH, LabW, LabC, LabU, LabOth)
gc()

labs_export_1 <- data.frame(marker_name = "")
labs_export_2 <- data.frame(feature_name = "")

# Sources
Boston <- "Boston:phenomics.va.ornl.gov/web/api/phenotype/attachments/87/download"
Nashville <- "Nashville"
LabRS <- "LabRS:pubmed.ncbi.nlm.nih.gov/28505339"
West_Haven <- "West_Haven"

## Extract Labs Needed for Cohort Definition
# each patient must have 1+ measure each of LDL, A1C, and Creatinine after DMDx
# put these as a function so that we can rerun it after final cohort definition 
# (so that no % > 100)

CohLabs_function <- function() {
  LDLC <<- Labs %>%
    filter((ShortName == "LDLC" & Consensus == "yes") |
      (Consensus == "no" & LOINC %in% c("2089-1", "13457-7", "18262-6"))) %>%
    summlab(., 10, 500, "LDLC", source = Boston) %>%
    mutate(
      m_100_le_LDL_lt_130 = as.numeric(LDLC > 100 & LDLC < 130),
      m_130_le_LDL_lt_160 = as.numeric(LDLC >= 130 & LDLC < 160),
      m_160_le_LDL_lt_190 = as.numeric(LDLC >= 160 & LDLC < 190),
      LDL_ge_190 = as.numeric(LDLC >= 190)
    ) %>%
    summlab2(.)
  table(LDLC$Consensus, useNA = "always")

  # HbA1c
  A1C <<- Labs %>%
    filter(ShortName == "A1C" & Consensus == "yes") %>%
    summlab(., 3, 20, "A1C", source = Boston) %>%
    mutate(
      m_6_le_HbA1c_lt_7 = as.numeric(A1C >= 6 & A1C < 7),
      m_7_le_HbA1c_lt_8 = as.numeric(A1C >= 7 & A1C < 8),
      m_8_le_HbA1c_lt_9 = as.numeric(A1C >= 8 & A1C < 9),
      HbA1c_ge_9 = as.numeric(A1C >= 9)
    ) %>%
    summlab2(.)

  # Serum Creatinine
  Creat_BSP <<- Labs %>%
    filter(ShortName == "Creat - BSP" & Consensus == "yes") %>%
    summlab(., 0.2, 10, "Creat_BSP", source = Boston)
}

CohLabs_function()

## Apply the filter
DMcohort <- prelim_cohort %>%
  left_join_quiet(Creat_BSP, multiple = "all") %>%
  filter(dt >= DmDx_first) %>%
  dplyr::select(PatientICN:DmDx_first) %>%
  distinct() %>%
  left_join_quiet(A1C, multiple = "all") %>%
  filter(dt >= DmDx_first) %>%
  dplyr::select(PatientICN:DmDx_first) %>%
  distinct() %>%
  left_join_quiet(LDLC, multiple = "all") %>%
  filter(dt >= DmDx_first) %>%
  dplyr::select(PatientICN:DmDx_first) %>%
  distinct() %>%
  mutate(
    Age_DmDx = DmDx_first - BirthDate,
    Age_2000 = 0 - BirthDate,
    censor_date = pmin(LastDx, DeathDate, na.rm = TRUE),
    # Define time-to-event for death, which has its own censoring date and 
    # doesn't differ by outcome
    Deceased_tte = coalesce(
      DeathDate - DmDx_first,
      death_censor_date - DmDx_first
    ),
    DeceasedFlag = ifelse(DeceasedFlag == "Y", 1, 0)
  )

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = paste0("Has at least 1 measurement each of LDL, HbA1c,", 
                  " and serum Creatinine at/after DmDx"),
    n = npat(DMcohort)
  )
)

DMcohort <- DMcohort %>%
  filter(Deceased_tte >= 0) %>%
  mutate(cohort)

flowchart_data <- rbind(
  flowchart_data,
  data.frame(
    step = "Date of death, if present, does not precede date of first DM",
    n = npat(DMcohort)
  )
)

cohort_n <- npat(DMcohort)
DMcohortICNs <- DMcohort[, "PatientICN"]

rm(prelim_cohort, prelim_ids)

# Save this object as a checkpoint, and for the case where we want to read in 
# IDs without reading in all data
dir.create(paste0(cohort_dir, "coh", cohort, "/RDS/"), 
           recursive = TRUE, showWarnings = F)
saveRDS(DMcohort, 
        paste0(cohort_dir, "coh", cohort, "/RDS/cohort.", cohort, ".RDS"))

# Restrict all objects that we wish to keep to the final cohort IDs, 
# and remove objects that will be re-pulled
rm(
  A1C, Creat_BSP, LDLC, GLP1, Glucagon, Insulin, Thiazolidinedione, SGLT2, DPP4, 
  Sulfonylurea, Metformin, OralGlucoseLoweringExclMetformin, 
  Other_GlucoseLowering
)
for (obj in c("Labs", "Dx", "Rx")) {
  assign(obj, get(obj) %>% 
           inner_join(DMcohortICNs, by = "PatientICN"))
}
gc()
rm(obj)

# Now that the cohort is final, re-summarize LDLC, A1C, and Serum Creatinine, 
# DM codes, and DM meds.
DM_function()
DM_meds_function()
CohLabs_function()

rm(DM, DM_type_key, DM_pattern)

# Any further filtering steps will be outcome-specific. Write out the flowchart.
saveRDS(
  flowchart_data %>% mutate(cohort),
  paste0(results_dir_export_details, "flowchart_data.", cohort, ".RDS")
)

# Add info about cohort ages/years in DM cohort only (may differ from overall)
coh_years_demo
coh_years_demo <- coh_years_demo %>%
  mutate(population = "all") %>%
  rbind(DMcohort %>%
    filter(!(outlier(BirthDate))) %>%
    summarise(
      min_DOB = min(BirthDate) + 2000,
      median_DOB = median(BirthDate) + 2000,
      max_DOB = max(BirthDate) + 2000
    ) %>%
    mutate(
      min_age_pulldate = C2023_PULL_DATE - max_DOB + 2000,
      median_age_pulldate = C2023_PULL_DATE - median_DOB + 2000,
      max_age_pulldate = C2023_PULL_DATE - min_DOB + 2000,
      cohort, population = "DMcohort"
    ))

coh_years_demo

saveRDS(coh_years_demo, 
        paste0(results_dir_export_details, "cohort.ages.coh", cohort, ".RDS"))

# Now that the cohort IDs are defined, continue defining the rest of variables

## CONTINUE DEFINING MEDICATIONS VARIABLES #####################################

# Lipids drugs
Statin_High <- extract.detail.Rx(
  Rx, "Statin_High",
  paste0("SIMVASTATIN (TAB |)80[ ]*MG", 
         "|ATORVASTATIN (CALCIUM |CA |TAB |)[48]0[ ]*MG", 
         "|ROSUVASTATIN (CALCIUM |CA |TAB |)[24]0[ ]*MG"
  ), 
  "DrugNameWithDose"
)
Statin_Medium <- extract.detail.Rx(
  Rx, "Statin_Medium",
  paste0(
    "SIMVASTATIN (TAB |)[24]0[ ]*MG", 
    "|ATORVASTATIN (CALCIUM |CA |TAB |)[12]0[ ]*MG", 
    "|ROSUVASTATIN (CALCIUM |CA |TABLET |)(5|10)[ ]*MG",
    "|PRAVASTATIN (NA |)[48]0[ ]*MG|LOVASTATIN 40[ ]*MG", 
    "|FLUVASTATIN (NA |)80[ ]*MG|PITAVASTATIN.* [1-4][ ]*MG"
  ),
  "DrugNameWithDose"
)
Statin_Low <- extract.detail.Rx(
  Rx, "Statin_Low",
  paste0("SIMVASTATIN (TAB |)(5|10)[ ]*MG|", 
         "ATORVASTATIN (CALCIUM |CA |TAB |)5[ ]*MG", 
         "|ROSUVASTATIN (CALCIUM |CA |TAB |)2\\.5[ ]*MG", 
         "|LOVASTATIN [12]0[ ]*MG|PRAVASTATIN (NA |)(5|10|20)[ ]*MG|", 
         "FLUVASTATIN (NA |)[24]0[ ]*MG"
         ),
  "DrugNameWithDose"
)
Statin_Any <- extract.detail.Rx(
  Rx, "Statin_Any", "VASTATIN", "DrugNameWithDose")
Ezetimibe <- extract.detail.Rx(
  Rx, "Ezetimibe", "EZETIMIBE", "DrugNameWithoutDose")
Fibrates <- extract.detail.Rx(
  Rx, "Fibrates", "GEMFIBROZIL|FIBRATE|FIBRIC", "DrugNameWithDose")
BAS <- extract.detail.Rx(
  Rx, "BAS", "COLESTIPOL|CHOLESTYRAMINE", "DrugNameWithoutDose")
Niacin <- extract.detail.Rx(
  Rx, "Niacin", "NIACIN |NIACIN$", "DrugNameWithDose") # space is not a typo

# BP drugs
ACEInhibitors <- extract.detail.Rx(Rx, "ACEInhibitors", "CV800", 
                                   "VAClassification", exact_match = TRUE)
AngiotensinIIRB <- extract.detail.Rx(Rx, "AngiotensinIIRB", "CV805", 
                                     "VAClassification", exact_match = TRUE)
AntiAnginals <- extract.detail.Rx(Rx, "AntiAnginals", "CV250", 
                                  "VAClassification", exact_match = TRUE)
BetaBlockers <- extract.detail.Rx(Rx, "BetaBlockers", "CV100", 
                                  "VAClassification", exact_match = TRUE)
BetaB2nd <- extract.detail.Rx(Rx, "BetaB2nd", "METOPROLOL|ATENOLOL|BISOPROLOL", 
                              "DrugNameWithoutDose")
BetaB3rd <- extract.detail.Rx(Rx, "BetaB3rd", "CARVEDILOL|NEBIVOLOL", 
                              "DrugNameWithoutDose")
LoopDiuretics <- extract.detail.Rx(Rx, "LoopDiuretics", "CV702", 
                                   "VAClassification", exact_match = TRUE)
ThiazideDiuretics <- extract.detail.Rx(Rx, "ThiazideDiuretics", "CV701", 
                                       "VAClassification", exact_match = TRUE)
CChannelBlockers <- extract.detail.Rx(Rx, "CChannelBlockers", "CV200", 
                                      "VAClassification", exact_match = TRUE)
AlphaBlockers <- extract.detail.Rx(Rx, "AlphaBlockers", "CV150", 
                                   "VAClassification", exact_match = TRUE)
AntiHypertensiveComb <- extract.detail.Rx(Rx, "AntiHypertensiveComb", "CV400", 
                                          "VAClassification", exact_match = TRUE)
PotassiumSparingDiuretics <- extract.detail.Rx(
  Rx, "PotassiumSparingDiuretics", "CV704", "VAClassification", 
  exact_match = TRUE)
PeriphVasodilators <- extract.detail.Rx(Rx, "PeriphVasodilators", "CV500", 
                                        "VAClassification", exact_match = TRUE)
Other_Antihypertensives <- extract.detail.Rx(
  Rx, "Other_Antihypertensives", "CV490", "VAClassification", exact_match = TRUE)
DigitalisGlycosides <- extract.detail.Rx(Rx, "DigitalisGlycosides", "CV050", 
                                         "VAClassification", exact_match = TRUE)
AntiArrhythmics <- extract.detail.Rx(Rx, "AntiArrhythmics", "CV300", 
                                     "VAClassification", exact_match = TRUE)

# Other Drugs
Anticoagulants <- extract.detail.Rx(Rx, "Anticoagulants", "BL110", 
                                    "VAClassification", exact_match = TRUE)
NSAID <- extract.detail.Rx(Rx, "NSAID", "CN104|MS102", "VAClassification")
PlateletAggInhib <- extract.detail.Rx(Rx, "PlateletAggInhib", "BL117", 
                                      "VAClassification", exact_match = TRUE)
ProtonPumpInhib <- extract.detail.Rx(
  Rx, "ProtonPumpInhib", c("PRAZOLE", "ARIPIPRAZOLE", "BREXPIPRAZOLE"), 
  "DrugNameWithDose", exclude = c(FALSE, TRUE, TRUE)) 
Antivirals <- extract.detail.Rx(Rx, "Antivirals", "AM800", 
                                "VAClassification", exact_match = TRUE)
ProteaseInhib <- extract.detail.Rx(
  Rx, "ProteaseInhib", "NAVIR", "DrugNameWithDose") 
Antibiotics <- extract.detail.Rx(Rx, "Antibiotics", "AM[1-6]", 
                                 "VAClassification")
AntiNeoplastics <- extract.detail.Rx(Rx, "AntiNeoplastics", "AN", 
                                     "VAClassification")
Corticosteroids <- extract.detail.Rx(Rx, "Corticosteroids", "HS05[012]", 
                                     "VAClassification")
Antidepressant_Rx <- extract.detail.Rx(
  Rx, "Antidepressant_Rx",
  paste0(
    "CHLORPROMAZINE|FLUPHENAZINE|PERPHENAZINE|THIORIDAZINE|THIOTHIXENE", 
    "|TRIFLUOPERAZINE|ARIPIPRAZOLE|ASENAPINE",
    "BREXPIPRAZOLE|CARIPRAZINE|CLOZAPINE|HALOPERIDOL|ILOPERIDONE|LOXAPINE", 
    "|LURASIDONE|OLANZAPINE|PALIPERIDONE|PIMAVANSERIN",
    "QUETIAPINE|RISPERIDONE|ZIPRASIDONE|LITHIUM|ACETOPHENAZINE|CHLORPROTHIXENE",
    "DROPERIDOL|FLUPHENAZINE|HALOPERIDOL|ILOPERIDONE|LURASIDONE|MESORIDAZINE", 
    "|MOLINDONE|PIMOZIDE|PROCHLORPERAZINE|PROMETHAZINE",
    collapse = "|"
  ), "DrugNameWithoutDose"
)
Stimulant_Rx <- extract.detail.Rx(
  Rx, "Stimulant_Rx", "AMPHETAMINE|METHYLPHENIDATE|MODAFINIL", 
  "DrugNameWithoutDose")
OpioidForPain_Rx <- extract.detail.Rx(Rx, "OpioidForPain_Rx",
  c("CN101", "TRAMADOL", "DOVERIN|NALOX|IPECAC|BUPRE"),
  c("VAClassification", "DrugNameWithoutDose", "DrugNameWithDose"),
  exclude = c(FALSE, FALSE, TRUE), exact_match = FALSE
)
EEstradiol <- extract.detail.Rx(
  Rx, "EEstradiol", "ETHINYL ESTRADIOL", "DrugNameWithoutDose")
Levothyroxine <- extract.detail.Rx(
  Rx, "Levothyroxine", "LEVOTHYROXINE", "DrugNameWithoutDose")
Aspirin <- extract.detail.Rx(Rx, "Aspirin", "ASPIRIN", "DrugNameWithoutDose")
AnyDrug <- extract.detail.Rx(Rx, "AnyDrug", "", "DrugNameWithoutDose")

# Clean up
rm(Rx)
gc()

## CONTINUE DEFINING CONDITIONS VARIABLES (ICD) ###############################

MI <- extract.detail.icd(Dx, "MI", "^410|^412|^I2[12]|^I25.2")
OldMI <- extract.detail.icd(Dx, "OldMI", "^412|^I25.2")
# Stroke_any <- extract.detail.icd(
#  Dx, "Stroke_any", "^433.[012389]1|^433.3|^434.91|^43[0-2]|^I6[0-4]")
Stroke_infarct <- extract.detail.icd(
  Dx, "Stroke_infarct", "^433.[012389]1|^433.3|^434.91|^I63")
# Stroke_hem <- extract.detail.icd(Dx, "Stroke_hem", "^43[0-2]|^I6[0-2]")
alcdx_poss <- extract.detail.icd(Dx, "alcdx_poss", "^291|^303|^305.0|^F10.")
Nicdx_poss <- extract.detail.icd(Dx, "Nicdx_poss", "^305.1|^F17.2|^Z72.0")
AmphetamineUseDisorder <- extract.detail.icd(
  Dx, "AmphetamineUseDisorder", "^304.4|^305.7|^F15.")
OpioidOverdose <- extract.detail.icd(
  Dx, "OpioidOverdose", "^965.0|^e850.[012]|^e980.0|^e935.[012]|^T40.[01234]")
OUD <- extract.detail.icd(Dx, "OUD", "^304.[07]|^305.5|^F11")
SedativeUseDisorder <- extract.detail.icd(
  Dx, "SedativeUseDisorder", "^304.1|^305.4|^F13")
# poisoning by barbiturates, other sedatives and hypnotics, 
# CNS muscle-tone depressants, anti-depressants,
# (accidental poisoning or adverse effectrs by same)
# Adverse Effects of Antiepileptic, Sedatives-Hypnotics, and Antiparkinsonism drugs
SAE_sed <- extract.detail.icd(
  Dx, "SAE_sed",
  paste0("^967.[08]|^968.0|^969.[12345]|^e851.|^e852.[01234589]|^e853.[01289]", 
         "|^e937.[08]|^e938.0|^e939.[1245]|^e980.[123]|^T42"
         )
)
# Substance aboust disorder, Not all-inclusive, includes depend in icd 9 vs 10
SUD_CatchAll <- extract.detail.icd(
  Dx, "SUD_CatchAll", "^304.[5689|^305.[389]|^F1[1235689]")
# Adverse effects of Non-opioid Analgesics, Antipyretics and Antirheumatics
SAE_Acet <- extract.detail.icd(
  Dx, "SAE_Acet",
  paste0("^967.[08]|^968.[012345]|^e850.4|^e851.|^e852.[01234589]|", 
         "^e853.[01289]|^e935.4|^e937.[089]|^e939.[1245]|^e980.[13]|^T39"
         )
)
# Adverse effects of other drugs
SAE_OtherDrug <- extract.detail.icd(
  Dx, "SAE_OtherDrug",
  paste0("^969.|^970.1|^e940.1|^e850.3|^e855.[012345689]|^e854.[013]|", 
         "^e939.[067]|^965.[16]|^e935.[36]|^e980.[45]|^T40.[5789]|^T4[13]"
         )
)
# Certain Mental health disorders (not any/all), does include depressive episodes
MH_CatchAll <- extract.detail.icd(
  Dx, "MH_CatchAll",
  paste0(
    "^290.[89]|^293.[01234]|^295.00|^295.5|^296.82|^296.9|^300.1[1234569]", 
    "|^300.[3456789]|^301.[13]|^301.21|^301.5[19]",
    "|^301.8[49]|^307.1|^307.5[12349]|^308|^309.[012349]|^309.8[239]|^31[124]", 
    "|^780.1|^F3[234]|^F4[45]|^F6[38]|^F9[01]|^R45.7"
  )
)
ODEPRdx_poss <- extract.detail.icd(
  Dx, "ODEPRdx_poss", 
  paste0("^301.12|^300.4|^293.83|^298.0|^301.1|^311|^296.9|^309.0|", 
         "^309.1|^296.82|^F32|^F33")
  )
TBI_Dx <- extract.detail.icd(Dx, "TBI_Dx", "^85[123456789]|^S06.[23456]")
Concuss <- extract.detail.icd(Dx, "Concuss", "^850|^S06.[019]")
Headache <- extract.detail.icd(
  Dx, "Headache", "^784.0|^307.81|^339|^346|^G4[34]|^M54.81|^R51")
SAE_Falls <- extract.detail.icd(
  Dx, "SAE_Falls",
  paste0("^e880.[01]|^e880.[19]|^e881.[01]|^e88[2578].|^e883.[0129]", 
         "|^e884.[01234569]|^e886.[09]|^e929.3|^e987.[0129]|^W[01]")
)
Amputation <- extract.detail.icd(
  Dx, "Amputation", "^8[89][567]|^v49.[67]|^S68|^S88")
SpinalCordInj <- extract.detail.icd(
  Dx, "SpinalCordInj", "^806.[0123456789]|^952.[0123489]|^S14|^S24|^S34|^G82")
Backpain <- extract.detail.icd(
  Dx, "Backpain",
  paste0("^72[01234]|^G54.[134]|^M43.[013589]|^M45.[0456789]|^M46.[01489]", 
         "|^M4[789]|^M5[134]|^Q76|^S23|^S33")
)
EH_PARALYSIS <- extract.detail.icd(
  Dx, "EH_PARALYSIS", "^342.[019]|^34[34]|^G04.1|^G11.4|^G80.[12]|^G8[123]")
DEMENTIA <- extract.detail.icd(
  Dx, "DEMENTIA",
  paste0(
    "^046.[13]|^046.79|^290.[01234]|^291.2|^292.82|^294.[128]|^331.[01279]|", 
    "^331.8[23]|^333.[04]|^A81.[0289]|^F0[123]|^F10.29]|^F13.[29]", 
    "|^F1[89].[129]|^G23.1|^G30.[0189]|^G31.09|^G31.83|^G90.3"
  )
)
Parkinsons <- extract.detail.icd(Dx, "Parkinsons", "^332|^G20")
DeliriumTremens <- extract.detail.icd(
  Dx, "DeliriumTremens", "^291|^F10.2[3567]")
HIV <- extract.detail.icd(Dx, "HIV", "^04[234]|^v08|^B20|^Z21")
Visual <- extract.detail.icd(Dx, "Visual", "^369|^H5[34]")
Neuropathy <- extract.detail.icd(
  Dx, "Neuropathy",
  paste0(
    "^256.60|^355.[09]|^357.[29]|^356|^B02.2[23]|^E08.42|^E10.4[0123]|", 
    "^E1[13].4|^G50.[089]|^G52.1|^G5[4567]|^G58.[789]|^G60.[2389]|", 
    "^G61.9|^G62.[089]|^G90.09|^G99.0|^H46.[2389]|^H47.0|^M79.2"
  )
)
Homeless <- extract.detail.icd(Dx, "Homeless", "^v60.0|^Z59.0")
CAD <- extract.detail.icd(Dx, "CAD", "^411|^413|^414|^I25")
ChestPain_Dx <- extract.detail.icd(Dx, "ChestPain_Dx", "^786.50|^R07.[189]")
EH_ARRHYTH <- extract.detail.icd(
  Dx, "EH_ARRHYTH",
  paste0("^426.1[013]|^426.[234678]|^426.5[0123]|^427.[02]|^427.31|", 
         "^427.60|^427.9|^785.0|^v45.0|^v53.3|^I4[89]")
)
EH_BLANEMIA <- extract.detail.icd(Dx, "EH_BLANEMIA", "^280.0|^D5[89]")
EH_CHRNPULM <- extract.detail.icd(
  Dx, "EH_CHRNPULM", "^49|^506.4|^50[012345]|^J4[01234]|^J6")
EH_COAG <- extract.detail.icd(Dx, "EH_COAG", "^286|^287.[1345]|^D6[56789]")
EH_DefANEMIA <- extract.detail.icd(
  Dx, "EH_DefANEMIA", 
  "^280.[189]|^281|^D50.[89]|^D51.[012389]|^D52.[0189]|^D653.[01289]")
EH_ELECTRLYTE <- extract.detail.icd(
  Dx, "EH_ELECTRLYTE", "^276|^E22.2|^E86.[019]|^E87.[012345678]")
EH_HEARTFAILURE <- extract.detail.icd(
  Dx, "EH_HEARTFAILURE", "^398.91|^402.11|^402.91|^404.1[13]|^404.9[13]|^428|^I50")
EH_HYPERTENS <- extract.detail.icd(
  Dx, "EH_HYPERTENS", "^401.[19]|^402.[19]0|^404.[19]0|^405.[19]|^I1[01]")
EH_LIVER <- extract.detail.icd(
  Dx, "EH_LIVER", 
  paste0("^070.3[23]|^070.54|^456.[012]|^571.[0235689]|^571.4[09]|", 
         "^572.[38]|^v42.7|^K7[0123456]|^B18.|^I85")
  )
EH_OBESITY <- extract.detail.icd(
  Dx, "EH_OBESITY", "^278.0|^E66")
# cerebral degeneration, parkinsons, huntingtons, chorea, MS,  ataxia, 
# muscular atrophy, demyelinating diseases, epilepsy
EH_OTHNEURO <- extract.detail.icd(
  Dx, "EH_OTHNEURO",
  paste0("^331.9|^332.0|^333.[45]|^340|^33[45]|^341.[1289]|^345.[014589]", 
         "|^G1[0123]|^G2[015]|^G31.[289]|^G32.[089]|^G35|^G36.[189]", 
         "|^G37.[01234589]|^G40.01234589AB]|^G.[14]|^R47|^R56.[019]")
)
# aortic aneurysm, stricture of artery, vascular insufficiency, 
# vascular implants, pacemaker, atherosclerosis, other peripheral vascular disease
EH_PERIVASC <- extract.detail.icd(
  Dx, "EH_PERIVASC", 
  paste0("^441.[2479]|^447.1|^557.[19]|^v43.4|^440|^443.[123456789]", 
         "|^I70.[12345678]|^I71.|^I72.|^I73.9")
  )
# chronic pulmonary heart disease, disease of pulmonary vessels
EH_PULMCIRC <- extract.detail.icd(Dx, "EH_PULMCIRC", "^416|^417.9|^I27")
# circumscribed scleroderma, diffuse diseases of connective tissue, rheumatoid 
# arthritis, ankylosing spondylitis, polymyalgia rheumatica. lupus
EH_RHEUMART <- extract.detail.icd(
  Dx, "EH_RHEUMART", 
  "^701.0|^71[04]|^72[05]|^M0[23456]|^M1[23]|^M45|^M32")
# Heart Valve disease
EH_VALVDIS <- extract.detail.icd(
  Dx, "EH_VALVDIS", 
  paste0("^v42.4|^v43.3|^093.2|^39[456]|^397.[01]|^424.[0-8]|^424.9[01]", 
         "|^746.[3456]|^A52.0|^I0[56789]|^I3[456789]|^Q23.[0123]|^Z95.[234]")
  )
EH_WEIGHTLS <- extract.detail.icd(
  Dx, "EH_WEIGHTLS", "^26[0123]|^783.21|^E4[0123456]|^R63.4|^R64")

# Add Diabetic retinopathy
DiabeticRetinopathy <- extract.detail.icd(
  Dx, "DiabeticRetinopathy", 
  "^249.5[01]|^250.5[0-3]|^362.0[1-7]|^E0[89].3[1-5]|^E1[013].3[1-5]")
Urolithiasis <- extract.detail.icd(Dx, "Urolithiasis", "^59[24]|^N2[0123]") 
Pyelonephritis <- extract.detail.icd(
  Dx, "Pyelonephritis", "^590.[0189]|^590.\\.|^N1[012]") # kidney infection
FSGS <- extract.detail.icd(Dx, "FSGS", "^N04.1")
ADPKD <- extract.detail.icd(Dx, "ADPKD", "^753.1[23]|^Q61.[23]")
Cancer <- extract.detail.icd(
  Dx, "Cancer", 
  "^1[4-9]|^20[0-8]|^209.[01237]|^23[0-8]|^C|^D0[0-9]|^D22|^D4[5-7]|^Q85.0[01239]")
Lupus <- extract.detail.icd(Dx, "Lupus", "^695.4|^710.089|^M32")
SleepDisorder <- extract.detail.icd(
  Dx, "SleepDisorder", "^307.4|^327|^347|^780.5|^G47")
NephroticSyndrome <- extract.detail.icd(
  Dx, "NephroticSyndrome", "^581.9|^N04.[0-9]")
Proteinuria_Dx <- extract.detail.icd(
  Dx, "Proteinuria_Dx", "^R80.[012389]|^N06|^593.6|^791.0")
Dialysis_Dx <- extract.detail.icd(
  Dx, "Dialysis_Dx",
  paste0("^39.95[23]$|^54.98[23]$|^585.6|^792.5$|^99.78$|^v45.1$|^v45.1[12]$|", 
         "^v56.[0128]$|^v56.3[12]$|^N18.6|^R88.0$|^Z49.[03][12]$|^Z99.2$")
)
KidneyTransplant_Dx <- extract.detail.icd(
  Dx, "KidneyTransplant_Dx", "^996.81|^v42.0|^T86.1[01239]|^Z94.0")
CKD5_Dx <- extract.detail.icd(
  Dx, "CKD5_Dx", 
  "^403.[019]1|^404.[019][23]|^585.5|^I12.0|^I13.11|^I13.2|^N18.5")
ESRD_Dx <- rbind(
  Dialysis_Dx %>% mutate(code_type = "Dialysis_ESRD"),
  KidneyTransplant_Dx %>% mutate(code_type = "KidneyTransplant"),
  CKD5_Dx %>% mutate(code_type = "CKD5")
)
CKD4_Dx <- extract.detail.icd(Dx, "CKD4_Dx", "^N18.4|^N04.[89]|^585.4") %>%
  rbind(ESRD_Dx, fill = TRUE) %>%
  distinct() # also include more severe codes
CKD3b_Dx <- extract.detail.icd(Dx, "CKD3b_Dx", "^N18.32") %>%
  rbind(CKD4_Dx, fill = TRUE) %>%
  distinct()
CKD3a_Dx <- extract.detail.icd(Dx, "CKD3a_Dx", "^N18.3|^585.3") %>%
  rbind(CKD3b_Dx, fill = TRUE) %>%
  distinct()
# CKD to include un-staged codes
CKD_Dx <- extract.detail.icd(
  Dx, "CKD_Dx", "^E1..2|^N08.3|^I1[23]|^N18|^N04.[89]|^250.4|^40[34]|^585") %>%
  rbind(CKD3a_Dx, fill = TRUE) %>%
  distinct()
ACUTERF_Dx <- extract.detail.icd(Dx, "ACUTERF_Dx", "^N17|^584")

## DEFINE COMORBIDITIES (ELIXHAUSER, DCSI)  ###################################

# DCSI
dcsi.ret1 <- paste0("^250.5|^249.5|^362.[01]|^362.53|^362.8[123]|^E0[89].3", 
                    "|^E1[013].3|H35.[069]|H35.35|H35.8[0-9]")
dcsi.ret1.excl <- "^362.02|^E0[89].3[45]|^E1[013].3[45]"
dcsi.ret2 <- "^36[19].|^362.02|^379.23|^E0[89].3[45]|^E1[013].3[45]"
dcsi.neph1 <- paste0("^58[0123]|^585.[1239]|^250.4|^249.4|^E0[89].2[129]", 
                     "|^E1[013].2[129]|N00|N0[345]|N18.[1239]")
dcsi.neph2 <- "^585.[456]|^586|^593.9|N18.[4-6]|N19"
dcsi.neur1 <- paste0(
  "^249.6|^250.6|^337.[01]|^35[45]|^356.9|^357.2|^358.1|^458.0|^536.3|^564.5", 
  "|^596.54|^713.5|^951.[013]|^E0[8-9].4|^E1[013].4|G5[67]|G60.9|G73.3", 
  "|G90.0[19]|G90.[8-9]|G99.0|H49|I95.1|K31.84|K59.1|N31.9|M14.6|S04"
)
dcsi.cbr1 <- "^435|G45"
dcsi.cbr2 <- "^43[1346]|I6[1356]|I67.81"
dcsi.cvd1 <- "^41[134]|^429.2|^440|I2[045]|I70"
dcsi.cvd1.excl <- "^440.2[34]|I25.2|I70.2[56]"
dcsi.cvd2 <- paste0("^41[02]|^427.[1345]|^428|^440.2[34]|^441|I2[1-3]|", 
                    "I25.2|I4[6-9]|I50|I70.2[56]|I71.")
dcsi.pvd1 <- paste0("^250.7|^249.7|^440.21|^442.3|^443.81|^443.9|^892.1", 
                    "|^E0[89].5[19]|^E0[89].621|^E1[013].5[19]|^E1[013].621", 
                    "|I72.4|I70.21|I73.89|I73.9|S91.3")
dcsi.pvd2 <- paste0("^040.0|^444.22|^707.1|^785.4|A48.0|I74.3|L97|^E0[89].52", 
                    "|^E1[013].52|I96")
dcsi.met1 <- "E0[89].[01]0|^E1[013].[01]0|^E0[89].649|^E1[013].649"
dcsi.met2 <- paste0("^249.[123]|^250.[123]|^E0[89].[01]1|^E1[013].[01]1", 
                    "|^E0[89].641|^E1[013].641")
dcsi_key <- Dx %>%
  distinct(ICDCode) %>%
  mutate(
    ret1 = ifelse(grepl(dcsi.ret1, ICDCode) &
      !grepl(dcsi.ret1.excl, ICDCode), 1, 0),
    ret2 = ifelse(grepl(dcsi.ret2, ICDCode), 1, 0),
    neph1 = ifelse(grepl(dcsi.neph1, ICDCode), 1, 0),
    neph2 = ifelse(grepl(dcsi.neph2, ICDCode), 1, 0),
    neur1 = ifelse(grepl(dcsi.neur1, ICDCode), 1, 0),
    cbr1 = ifelse(grepl(dcsi.cbr1, ICDCode), 1, 0),
    cbr2 = ifelse(grepl(dcsi.cbr2, ICDCode), 1, 0),
    cvd1 = ifelse(grepl(dcsi.cvd1, ICDCode) &
      !grepl(dcsi.cvd1.excl, ICDCode), 1, 0),
    cvd2 = ifelse(grepl(dcsi.cvd2, ICDCode), 1, 0),
    pvd1 = ifelse(grepl(dcsi.pvd1, ICDCode), 1, 0),
    pvd2 = ifelse(grepl(dcsi.pvd2, ICDCode), 1, 0),
    met1 = ifelse(grepl(dcsi.met1, ICDCode), 1, 0),
    met2 = ifelse(grepl(dcsi.met2, ICDCode), 1, 0),
  ) %>%
  filter(ret1 | ret2 | neph1 | neph2 | neur1 | cbr1 | cbr2 | cvd1 | 
           cvd2 | pvd1 | pvd2 | met1 | met2)

dcsi_long <- inner_join(Dx, dcsi_key) %>%
  # multiple diagnosis/comorbidities on same date go on the same row
  pivot_longer(ret1:met2, 
               names_to = "dcsi.element", values_to = "element.value") %>%
  filter(element.value == 1) %>%
  select(-ICDCode) %>%
  distinct() %>%
  group_by(PatientICN, dcsi.element) %>%
  summarize(
    dcsi_element_first = min(dt),
    element.value = unique(ifelse(grepl("2", dcsi.element), 2, 1))
  ) %>%
  arrange(PatientICN, dcsi.element)
dcsi_wide <- dcsi_long %>%
  pivot_wider(names_from = "dcsi.element", values_from = "element.value") %>%
  filter(!is.na(dcsi_element_first)) %>%
  arrange(PatientICN, dcsi_element_first) %>%
  replace(is.na(.), 0)
dcsi_elem <- dcsi_wide %>%
  group_by(PatientICN) %>%
  mutate(across(!dcsi_element_first, cummax)) %>%
  # for each patient start their score at 0 at birth
  full_join(DMcohort %>% 
              select(PatientICN, dcsi_element_first = BirthDate)) %>% 
  arrange(PatientICN, dcsi_element_first) %>%
  replace(is.na(.), 0)
if (is.null(dcsi_elem$met1)) {
  dcsi_elem$met1 <- 0
}
if (is.null(dcsi_elem$met2)) {
  dcsi_elem$met2 <- 0
}
if (is.null(dcsi_elem$cbr1)) {
  dcsi_elem$cbr1 <- 0
}
if (is.null(dcsi_elem$cbr2)) {
  dcsi_elem$cbr2 <- 0
}
if (is.null(dcsi_elem$ret1)) {
  dcsi_elem$ret1 <- 0
}
if (is.null(dcsi_elem$ret2)) {
  dcsi_elem$ret2 <- 0
}
if (is.null(dcsi_elem$neph1)) {
  dcsi_elem$neph1 <- 0
}
if (is.null(dcsi_elem$neph2)) {
  dcsi_elem$neph2 <- 0
}
if (is.null(dcsi_elem$neur1)) {
  dcsi_elem$neur1 <- 0
}
if (is.null(dcsi_elem$cvd1)) {
  dcsi_elem$cvd1 <- 0
}
if (is.null(dcsi_elem$pvd1)) {
  dcsi_elem$pvd1 <- 0
}
if (is.null(dcsi_elem$cvd2)) {
  dcsi_elem$cvd2 <- 0
}
if (is.null(dcsi_elem$pvd2)) {
  dcsi_elem$pvd2 <- 0
}


DCSI <- dcsi_elem %>%
  mutate(
    cbr = pmax(cbr1, cbr2),
    cvd = pmax(cvd1, cvd2),
    neph = pmax(neph1, neph2),
    pvd = pmax(pvd1, pvd2),
    ret = pmax(ret1, ret2),
    met = pmax(met1, met2)
  ) %>%
  mutate(DCSIscore = cbr + cvd + neur1 + pvd + ret + met)

DCSI_excl_neph <- DCSI %>%
  mutate(DCSI_excl_neph = DCSIscore - neph) %>%
  select(PatientICN, dt = dcsi_element_first, DCSI_excl_neph) %>%
  td_context()
DCSI_excl_cvd <- DCSI %>%
  mutate(DCSI_excl_cvd = DCSIscore - cvd) %>%
  select(PatientICN, dt = dcsi_element_first, DCSI_excl_cvd) %>%
  td_context()

DCSI <- DCSI %>%
  select(PatientICN, dt = dcsi_element_first, DCSI = DCSIscore) %>%
  td_context()

# comorbidities
comor <- fread(paste0(results_dir_dim, "ICD_COMORBID_DIM.txt"))

# Suitable to query Ben's diagnosis data
comor_ICD <- base::rbind(
  comor %>% dplyr::select(-c(Sta3n, ICD10Code, contains("SID"))) %>%
    dplyr::rename(ICDCode = ICD9Code) %>% filter(!is.na(ICDCode)),
  comor %>% dplyr::select(-c(Sta3n, ICD9Code, contains("SID"))) %>%
    dplyr::rename(ICDCode = ICD10Code) %>% filter(!is.na(ICDCode))
) %>%
  distinct()

# Extract and save to RDS record-level info for each comorbidity 
# separately to save memory.
dir.create(paste0(cohort_dir, "coh", cohort, "/RDS/comorbidities/"), 
           recursive = T)
for (cond in names(comor_ICD)[-1]) {
  # Reduce comorbidites to the current condition
  d_cond <- comor_ICD %>% dplyr::select(all_of(c("ICDCode", cond)))
  names(d_cond)[2] <- "condition"
  d_cond <- d_cond %>%
    filter(!is.na(condition)) %>%
    mutate(condition = cond) %>%
    distinct()
  # Extract patient-level data
  patdat <- inner_join_quiet(Dx, d_cond, multiple = "all") %>% distinct()
  saveRDS(patdat, paste0(cohort_dir, "coh", cohort, "/RDS/comorbidities/", 
                         cond, ".RDS"))
  rm(d_cond, cond, patdat)
}
## rm(d_cond, cond, patdat, Dx); gc()

# Function to read in the comorbidity
read_comor <- function(filename) {
  cond <- gsub("^.*/|.RDS", "", filename)
  d <- readRDS(filename) %>%
    group_by(PatientICN, condition) %>%
    summarize(cci_elem_first = min(dt), .groups = "drop") %>%
    mutate(value = 1) %>%
    ungroup()
  gc()
  if (nrow(d) == 0) {
    d <- DMcohortICNs %>% mutate(condition = cond, value = 0)
  }
  return(d)
  rm(cond)
}

comor_long <- list.files(paste0(cohort_dir, "coh", cohort, "/RDS/comorbidities"), 
                         full.names = TRUE) %>%
  map_df(~ read_comor(.))
comor_wide <- comor_long %>%
  pivot_wider(names_from = "condition", values_from = "value") %>%
  filter(!is.na(cci_elem_first)) %>%
  arrange(PatientICN, cci_elem_first) %>%
  replace(is.na(.), 0)
comorbidities <- comor_wide %>%
  group_by(PatientICN) %>%
  mutate(across(AIDS:WtLoss, function(x) {
    x <- as.numeric(cumsum(x) > 0)
  })) %>%
  # for each patient start their score at 0 at birth
  full_join_quiet(DMcohort %>% select(PatientICN, cci_elem_first = BirthDate)) %>% 
  arrange(PatientICN, cci_elem_first) %>%
  replace(is.na(.), 0)
elix.score.nodm <- comorbidities %>%
  rowwise() %>%
  mutate(
    Elix_RawScore =
      sum(CHF + VD + PCD + PVD + HT + Paralysis + Neuro + CPD + Thyroid + RF +
        LD + PUD + AIDS + Lymph + MC + Tumor +
        RA + Coag + Obese + WtLoss + FED + BLAnemia +
        DAnemia + Alcohol + Drug + Psychoses + Depression)
  ) %>%
  dplyr::select(PatientICN, dt = cci_elem_first, Elix_RawScore) %>%
  ungroup() %>%
  td_context()
elix_excl_RF <- comorbidities %>%
  rowwise() %>%
  mutate(
    Elix_excl_RF =
      sum(CHF + VD + PCD + PVD + HT + Paralysis + Neuro + CPD + Thyroid +
        LD + PUD + AIDS + Lymph + MC + Tumor +
        RA + Coag + Obese + WtLoss + FED + BLAnemia + DAnemia +
        Alcohol + Drug + Psychoses + Depression)
  ) %>%
  dplyr::select(PatientICN, dt = cci_elem_first, Elix_excl_RF) %>%
  ungroup() %>%
  td_context()
rm(
  comor, comor_ICD, read_comor, dcsi_key, dcsi_elem,
  dcsi_long, dcsi_wide, comor_long, comor_wide, comorbidities,
  dcsi.cbr1, dcsi.cbr2, dcsi.cvd1, dcsi.cvd1.excl, dcsi.cvd2, 
  dcsi.met1, dcsi.met2, dcsi.neph1, dcsi.neph2, dcsi.neur1, 
  dcsi.pvd1, dcsi.pvd2, dcsi.ret1, dcsi.ret2, dcsi.ret1.excl
)

## CONTINUE DEFINING LABS AND BIOMARKERS ######################################
# Continue extracting from LabK
# UACR
UACR <- Labs %>%
  filter(ShortName %in% 
           c("Microalbumin/creatinine ratio", "Microalbumin/Creatinine") & 
           Consensus == "yes") %>%
  summlab(., 0.1, 20000, "UACR", source = Nashville) %>%
  mutate( 
    Microalbuminuria_30_le_UACR_lt_300 = as.numeric(UACR >= 30 & UACR < 300),
    Macroalbuminuria_UACR_ge_300 = as.numeric(UACR >= 300)
  ) %>%
  summlab2(.)

Troponin <- Labs %>%
  filter(ShortName == "Troponin I" & Consensus == "yes") %>%
  summlab(., 0, 0.7, "Troponin", source = Boston) %>%
  mutate(Troponin_ge_0.4 = as.numeric(Troponin >= 0.4)) %>%
  summlab2(.)

BUN <- Labs %>%
  filter(ShortName == "BUN - BSP" & Consensus == "yes") %>%
  summlab(., 0.2, 400, "BUN", source = Boston) %>%
  mutate(
    m_20_lt_BUN_lt_200 = as.numeric(BUN >= 20 & BUN < 200),
    BUN_ge_200 = as.numeric(BUN >= 200)
  ) %>%
  summlab2(.) 

Albumin <- Labs %>%
  filter(ShortName == "Albumin" & Consensus == "yes") %>%
  summlab(., 0.8, 10, "Albumin", source = Boston) %>%
  mutate(Albumin_lt_3.4 = as.numeric(Albumin < 3.4)) %>%
  summlab2(.)

TotChol <- Labs %>%
  filter(ShortName == "TotChol" & Consensus == "yes") %>%
  summlab(., 50, 600, "TotChol", source = Boston) %>%
  mutate(
    m_200_le_TotChol_lt_220 = as.numeric(TotChol >= 200 & TotChol < 220),
    TotChol_ge_220 = as.numeric(TotChol >= 220)
  ) %>%
  summlab2(.)

HDLC <- Labs %>%
  filter((ShortName == "HDLC" & Consensus == "yes") |
    (Consensus == "no" & LOINC == "2085-9" & 
       !grepl("LDL|NON-HDL|NON HDL", LabChemTestName))) %>%
  summlab(., 8.5, 120, "HDLC", source = Boston) %>%
  mutate(
    HDL_lt_40 = as.numeric(HDLC < 40),
    m_40_le_HDL_lt_60 = as.numeric(HDLC >= 40 & HDLC < 60),
    HDL_ge_90 = as.numeric(HDLC >= 90)
  ) %>%
  summlab2(.)

Trig <- Labs %>%
  filter(ShortName == "Trig" & Consensus == "yes") %>%
  summlab(., 15, 2000, "Trig", source = Boston) %>%
  mutate(
    m_150_le_Trig_lt_200 = as.numeric(Trig >= 150 & Trig < 200),
    Trig_ge_200 = as.numeric(Trig >= 200)
  ) %>%
  summlab2(.)

Glucose <- Labs %>%
  filter(ShortName == "Glucose" & Consensus == "yes") %>%
  summlab(., 30, 1500, "Glucose", source = Boston) %>%
  mutate(
    Glucose_lt_70 = as.numeric(Glucose < 70),
    m_100_le_Glucose_lt_125 = as.numeric(Glucose >= 100 & Glucose < 125),
    m_125_le_Glucose_lt_150 = as.numeric(Glucose >= 125 & Glucose < 150),
    Glucose_ge_150 = as.numeric(Glucose >= 150)
  ) %>%
  summlab2(.)

# Finger stick glucose
GlucoseFS <- Labs %>%
  filter(ShortName == "Glucose - FS" & Consensus == "yes") %>%
  summlab(., 30, 400, "GlucoseFS", source = Boston) %>%
  mutate(
    m_100_le_GlucoseFS_lt_125 = as.numeric(GlucoseFS >= 100 & GlucoseFS < 125),
    GlucoseFS_ge_125 = as.numeric(GlucoseFS >= 125)
  ) %>%
  summlab2(.)

Bicarbonate <- Labs %>%
  filter(ShortName == "Bicarbonate" & Consensus == "yes") %>%
  summlab(., 5, 55, "Bicarbonate", source = Boston) %>%
  mutate(
    Bicarbonate_lt_22 = as.numeric(Bicarbonate < 22),
    Bicarbonate_ge_28 = as.numeric(Bicarbonate >= 28)
  ) %>%
  summlab2(.)

Na <- Labs %>%
  filter(ShortName == "Sodium - BSP" & Consensus == "yes") %>%
  summlab(., 95, 225, "Na", source = Boston) %>%
  mutate(
    Na_lt_134 = as.numeric(Na < 134),
    Na_ge_146 = as.numeric(Na >= 146)
  ) %>%
  summlab2(.)

K <- Labs %>%
  filter(ShortName == "Potas - BSP" & Consensus == "yes") %>%
  filter(!grepl("PO4|PROTEIN", LabChemTestName)) %>%
  summlab(., 2, 8.5, "K", source = Boston) %>%
  mutate(
    K_lt_3.5 = as.numeric(K < 3.5),
    K_ge_5.1 = as.numeric(K >= 5.1)
  ) %>%
  summlab2(.)

Cl <- Labs %>%
  filter(ShortName == "Chloride - BSP" & Consensus == "yes") %>%
  summlab(., 60, 150, "Cl", source = Boston) %>%
  mutate(
    Cl_lt_92 = as.numeric(Cl < 92),
    Cl_ge_109 = as.numeric(Cl >= 109)
  ) %>%
  summlab2(.)

Ca <- Labs %>%
  filter(ShortName == "Calcium - BSP" & Consensus == "yes") %>%
  mutate(Units = stringr::str_trim(tolower(Units))) %>%
  filter(Units == "mg/dl") %>%
  summlab(., 6.5, 18, "Ca", source = Boston) %>%
  mutate(
    Ca_lt_8 = as.numeric(Ca < 8),
    Ca_ge_10.4 = as.numeric(Ca >= 10.4)
  ) %>%
  summlab2(.)

Mg <- Labs %>%
  filter(ShortName == "Mg - BSP" & Consensus == "yes") %>%
  summlab(., 0.5, 8, "Mg", source = Boston) %>% # these are recommended by MVP
  mutate(
    Mg_lt_1.6 = as.numeric(Mg < 1.6),
    Mg_ge_2.8 = as.numeric(Mg >= 2.8)
  ) %>%
  summlab2(.)

CRP <- Labs %>%
  filter(ShortName == "CRP" & Consensus == "yes") %>%
  summlab(., 0.1, 1000, "CRP", source = Boston) %>%
  mutate(CRP_ge_10 = as.numeric(CRP >= 10)) %>%
  summlab2(.)

TotalCK <- Labs %>%
  filter(ShortName == "TotalCK" & Consensus == "yes") %>%
  summlab(., 5, 2000, "TotalCK", source = Boston) %>%
  mutate(TotalCK_ge_200 = as.numeric(TotalCK >= 200)) %>%
  summlab2(.)

ALT <- Labs %>%
  filter(ShortName == "ALT" & Consensus == "yes") %>%
  summlab(., 0, 1000000, "ALT", source = Boston) %>%
  mutate(
    ALT_lt_7 = as.numeric(ALT < 7),
    ALT_ge_55 = as.numeric(ALT >= 55)
  ) %>%
  summlab2(.)

AST <- Labs %>%
  filter(ShortName == "AST" & Consensus == "yes") %>%
  summlab(., 0, 8000, "AST", source = Boston) %>%
  mutate(
    AST_lt_10 = as.numeric(AST < 10),
    AST_ge_40 = as.numeric(AST >= 40)
  ) %>%
  summlab2(.)

AlkalinePhosphatase <- Labs %>%
  filter(ShortName == "Phosphatase Alkaline" & Consensus == "yes") %>%
  summlab(5, 5000, "AlkalinePhosphatase", source = Nashville) %>%
  mutate(
    AlkalinePhosphatase_lt_20 = as.numeric(AlkalinePhosphatase < 20),
    AlkalinePhosphatase_ge_140 = as.numeric(AlkalinePhosphatase >= 140)
  ) %>%
  summlab2(.)

Hemoglobin <- Labs %>%
  filter(ShortName == "Hemoglobin" & Consensus == "yes") %>%
  summlab(., 5, 25, "Hemoglobin", source = Boston) %>%
  left_join(DMcohort %>% select(PatientICN, Gender)) %>%
  mutate(
    VVLow_Hemoglobin = ifelse((Gender == "M" & Hemoglobin < 10) |
      (Gender == "F" & Hemoglobin < 9), 1, 0),
    VLow_Hemoglobin = 
      ifelse((Gender == "M" & Hemoglobin < 11 & Hemoglobin >= 10) |
      (Gender == "F" & Hemoglobin < 10 & Hemoglobin >= 9), 1, 0),
    Low_Hemoglobin = 
      ifelse((Gender == "M" & Hemoglobin < 12 & Hemoglobin >= 11) |
      (Gender == "F" & Hemoglobin < 11 & Hemoglobin >= 10), 1, 0)
  ) %>%
  summlab2(.)

labs_export_2$upper_chr[labs_export_2$feature_name == "VVLow_Hemoglobin"] <- 
  "10 (Male) or 9 (Female)"
labs_export_2$upper_chr[labs_export_2$feature_name == "VLow_Hemoglobin"] <- 
  "11 (Male) or 10 (Female)"
labs_export_2$lower_chr[labs_export_2$feature_name == "VLow_Hemoglobin"] <- 
  "10 (Male) or 9 (Female)"
labs_export_2$upper_chr[labs_export_2$feature_name == "Low_Hemoglobin"] <- 
  "12 (Male) or 11 (Female)"
labs_export_2$lower_chr[labs_export_2$feature_name == "Low_Hemoglobin"] <- 
  "11 (Male) or 10 (Female)"

RBC <- Labs %>%
  filter(ShortName == "RBC" & Consensus == "yes") %>%
  summlab(., 2, 8, "RBC", source = Boston) %>%
  left_join(DMcohort %>% select(PatientICN, Gender)) %>%
  mutate(
    Low_RBC = ifelse((Gender == "M" & RBC < 4.5) | 
                       (Gender == "F" & RBC < 3.8), 1, 0),
    High_RBC = ifelse((Gender == "M" & RBC >= 5.5) | 
                        (Gender == "F" & RBC >= 4.8), 1, 0)
  ) %>%
  summlab2(.)
labs_export_2$upper_chr[labs_export_2$feature_name == "Low_RBC"] <- 
  "4.5 (Male) or 3.8 (Female)"
labs_export_2$lower_chr[labs_export_2$feature_name == "High_RBC"] <- 
  "5.5 (Male) or 4.8 (Female)"

RDW <- Labs %>%
  filter(ShortName == "RDW" & Consensus == "yes") %>%
  summlab(., 4, 40, "RDW", source = Boston) %>%
  mutate(
    RDW_lt_12 = as.numeric(RDW < 12),
    RDW_ge_15 = as.numeric(RDW >= 15)
  ) %>%
  summlab2(.)

MCV <- Labs %>%
  filter(ShortName == "MCV" & Consensus == "yes") %>%
  summlab(., 40, 200, "MCV", source = Boston) %>%
  mutate(
    MCV_lt_81 = as.numeric(MCV < 81),
    MCV_ge_97 = as.numeric(MCV >= 97)
  ) %>%
  summlab2(.)

MCH <- Labs %>%
  filter(ShortName == "MCH" & Consensus == "yes") %>%
  summlab(., 10, 50, "MCH", source = Boston) %>%
  mutate(
    MCH_lt_28 = as.numeric(MCH < 28),
    MCH_ge_32 = as.numeric(MCH >= 32)
  ) %>%
  summlab2(.)

MCHC <- Labs %>%
  filter(ShortName == "MCHC" & Consensus == "yes") %>%
  summlab(., 25, 45, "MCHC", source = Boston) %>%
  mutate(
    MCHC_lt_32 = as.numeric(MCHC < 32),
    MCHC_ge_36 = as.numeric(MCHC >= 36)
  ) %>%
  summlab2(.)

# Hematocrit -
Hematocrit <- Labs %>%
  filter(ShortName == "HCT" & Consensus == "yes") %>%
  summlab(., 15, 75, "Hematocrit", source = Boston) %>%
  left_join(DMcohort %>% select(PatientICN, Gender)) %>%
  mutate(
    Low_Hematocrit = ifelse((Gender == "M" & Hematocrit < 40) | 
                              (Gender == "F" & Hematocrit < 36), 1, 0),
    High_Hematocrit = ifelse((Gender == "M" & Hematocrit >= 54) | 
                               (Gender == "F" & Hematocrit >= 46), 1, 0)
  ) %>%
  summlab2(.)
labs_export_2$upper_chr[labs_export_2$feature_name == "Low_Hematocrit"] <- 
  "40 (Male) or 36 (Female)"
labs_export_2$lower_chr[labs_export_2$feature_name == "High_Hematocrit"] <- 
  "54 (Male) or 46 (Female)"

WBC <- Labs %>%
  filter(ShortName == "WBC" & Consensus == "yes") %>%
  summlab(., 1, 100, "WBC", source = Boston) %>%
  mutate(
    WBC_lt_4 = as.numeric(WBC < 4),
    WBC_ge_12 = as.numeric(WBC >= 12)
  ) %>%
  summlab2(.)

Eos <- Labs %>%
  filter(ShortName == "Eos - Abs" & Consensus == "yes") %>%
  summlab(., 0, 30, "Eos", source = Boston)

EosFra <- Labs %>%
  filter(ShortName == "Eos - Fra" & Consensus == "yes") %>%
  summlab(., 0, 10, "EosFra", source = Boston) %>%
  mutate(
    EosFra_lt_1 = as.numeric(EosFra < 1),
    EosFra_ge_4 = as.numeric(EosFra >= 4)
  ) %>%
  summlab2(.)

Baso <- Labs %>%
  filter(ShortName == "Baso - Abs" & Consensus == "yes") %>%
  summlab(., 0, 0.5, "Baso", source = Boston)

BasoFra <- Labs %>%
  filter(ShortName == "Baso - Fra" & Consensus == "yes") %>%
  summlab(., 0.2, 2.1, "BasoFra", source = Boston) %>%
  mutate(BasoFra_lt_0.5 = as.numeric(BasoFra < 0.5)) %>%
  mutate(BasoFra_ge_1 = as.numeric(BasoFra >= 1)) %>%
  summlab2(.)

Mono <- Labs %>%
  filter(ShortName == "Mono - Abs" & Consensus == "yes") %>%
  summlab(., 0.1, 5, "Mono", source = Boston)

MonoFra <- Labs %>%
  filter(ShortName == "Mono - Fra" & Consensus == "yes") %>%
  summlab(., 0, 20, "MonoFra", source = Boston) %>%
  mutate(
    MonoFra_lt_2 = as.numeric(MonoFra < 2),
    MonoFra_ge_8 = as.numeric(MonoFra >= 8)
  ) %>%
  summlab2(.)

Neut <- Labs %>%
  filter(ShortName == "Neut - Abs" & Consensus == "yes") %>%
  summlab(., 0.1, 50, "Neut", source = Boston)

NeutFra <- Labs %>%
  filter(ShortName == "Neut - Fra" & Consensus == "yes") %>%
  summlab(., 0, 85, "NeutFra", source = Boston) %>%
  mutate(
    NeutFra_lt_55 = as.numeric(NeutFra < 55),
    NeutFra_ge_70 = as.numeric(NeutFra >= 70)
  ) %>%
  summlab2(.)

Lymph <- Labs %>%
  filter(ShortName == "Lymph - Abs" & Consensus == "yes") %>%
  summlab(., 0.2, 15, "Lymph", source = Boston)

LymphFra <- Labs %>%
  filter(ShortName == "Lymph - Fra" & Consensus == "yes") %>%
  summlab(., 5, 75, "LymphFra", source = Boston) %>%
  mutate(
    LymphFra_lt_20 = as.numeric(LymphFra < 20),
    LymphFra_ge_40 = as.numeric(LymphFra >= 40)
  ) %>%
  summlab2(.)

Platelet <- Labs %>%
  filter(ShortName == "Platelet" & Consensus == "yes") %>%
  summlab(., 10, 4000, "Platelet", source = Boston) %>%
  mutate(
    Platelet_lt_150 = as.numeric(Platelet < 150),
    Platelet_ge_440 = as.numeric(Platelet >= 440)
  ) %>%
  summlab2(.)

MPV <- Labs %>%
  filter(ShortName == "MPV" & Consensus == "yes") %>%
  summlab(., 5, 15, "MPV", source = Boston) %>%
  mutate(
    MPV_lt_8 = as.numeric(MPV < 8),
    MPV_ge_12 = as.numeric(MPV >= 12)
  ) %>%
  summlab2(.)

# Two peaks may be due to whether the person is taking anti-coag therapy
INR <- Labs %>%
  filter(ShortName == "INR" & Consensus == "yes") %>%
  summlab(., 0.5, 5.5, "INR", source = Boston) %>%
  mutate(
    m_1_le_INR_lt_2 = as.numeric(INR >= 1 & INR < 2),
    m_2_le_INR_lt_3 = as.numeric(INR >= 2 & INR < 3),
    INR_ge_3 = as.numeric(INR >= 3)
  ) %>%
  summlab2(.)

PT <- Labs %>%
  filter(ShortName == "PT" & Consensus == "yes") %>%
  summlab(., 5, 85, "PT", source = Boston) %>%
  mutate(
    PT_lt_10 = as.numeric(PT < 10),
    PT_ge_13 = as.numeric(PT >= 13)
  ) %>%
  summlab2(.)

ESR <- Labs %>%
  filter(ShortName == "ESR" & Consensus == "yes") %>%
  summlab(.1, 150, "ESR", source = Boston) %>%
  td_context() %>%
  left_join(DMcohort %>% select(PatientICN, Gender)) %>%
  mutate(High_ESR = ifelse((Gender == "M" & age >= 50 & ESR >= 20) |
    (Gender == "M" & age < 50 & ESR >= 15) |
    (Gender == "F" & age >= 50 & ESR >= 30) |
    (Gender == "F" & age < 50 & ESR >= 20),
  1, 0
  )) %>%
  summlab2(.)
labs_export_2$lower_chr[labs_export_2$feature_name == "High_ESR"] <-
  paste0("20 (Male >= 50yo) or 15 (Male < 50yo) or ", 
         "30 (Female >= 50yo) or 20 (Female < 50yo)")

# Uric Acid
Uric_Acid_BSP <- Labs %>%
  filter(ShortName == "Uric Acid") %>% # NOT REQUIRE CONSENSUS??
  filter(grepl("SER|PLA|BLOOD", Topography, ignore.case = T)) %>%
  summlab(., 0.5, 20, "Uric_Acid_BSP", source = Boston) %>%
  left_join(DMcohort %>% select(PatientICN, Gender)) %>%
  mutate(High_Uric_Acid = 
           ifelse((Gender == "M" & Uric_Acid_BSP >= 7.2) | 
                    (Gender == "F" & Uric_Acid_BSP >= 6.1), 1, 0)) %>%
  summlab2(.)
labs_export_2$lower_chr[labs_export_2$feature_name == "High_Uric_Acid"] <- 
  "7.2 (Male) or 6.1 (Female)"

# CCP antibody test (test of rheumatoid arthritis)
CCP <- Labs %>%
  filter(ShortName == "CCP") %>% # NOT REQUIRE CONSENSUS??
  filter(grepl("SER|PLA|BLOOD", Topography, ignore.case = T)) %>%
  filter(Consensus == "yes" | 
           grepl("CYCLIC CITRULL|CCP", LabChemTestName, ignore.case = T)) %>%
  summlab(., 5, 300, "CCP", source = Boston) %>%
  mutate(CCP_ge_40 = as.numeric(CCP >= 40)) %>%
  summlab2(.)

Bilirubin_BSP_total <- Labs %>%
  filter(ShortName == "Bilirubin Total") %>% # Consensus?
  filter(grepl("SER|PLAS|BLOOD", Topography, ignore.case = T)) %>%
  summlab(., 0.1, 30, "Bilirubin_BSP_total", source = Nashville) %>%
  mutate(Bilirubin_BSP_total_ge_1.2 = as.numeric(Bilirubin_BSP_total >= 1.2)) %>%
  summlab2(.)

Bilirubin_BSP_conjugated <- Labs %>%
  filter(ShortName == "Bilirubin Direct") %>% # consensus?
  filter(grepl("SER|PLAS|BLOOD", Topography, ignore.case = T)) %>%
  mutate(value = ifelse(LabChemResultValue == "<0.1", 0, value)) %>%
  summlab(., 0, 20, "Bilirubin_BSP_conjugated", source = Nashville) %>%
  mutate(Bilirubin_BSP_conjugated_ge_0.3 = 
           as.numeric(Bilirubin_BSP_conjugated >= 0.3)) %>%
  summlab2(.)

BilirubinStick <- Labs %>%
  filter(ShortName == "Bilirubin Stick") %>% # consensus?
  filter(grepl("Urine", Topography, ignore.case = T) & 
           !grepl("Ser|Plas|Blood", Topography, ignore.case = T)) %>%
  mutate(value = case_when(
    !is.na(value) ~ value,
    grepl("Mod|2+", LabChemResultValue, ignore.case = T) ~ 2,
    grepl("Large|LG|3+", LabChemResultValue, ignore.case = T) ~ 3,
    grepl("Neg|^N$", LabChemResultValue, ignore.case = T) ~ 0,
    grepl("Sm|1+|POS|TR", LabChemResultValue, ignore.case = T) ~ 1,
    TRUE ~ NA
  )) %>%
  summlab(., 0, 3, "BilirubinStick", source = Nashville) %>%
  mutate(BilirubinStick_ge_1 = as.numeric(BilirubinStick >= 1)) %>%
  summlab2(.)
npat(Amylase)

GGTP <- Labs %>%
  filter(LOINC == "2324-2") %>%
  summlab(., 0, 1500, "GGTP", source = LabRS) %>%
  mutate(GGTP_ge_40 = as.numeric(GGTP >= 40)) %>%
  summlab2(.)
npat(GGTP)


Total_Prot <- Labs %>%
  filter(LOINC == "2885-2") %>%
  summlab(., 4, 50, "Total_Prot", source = LabRS) %>%
  mutate(
    Total_Prot_lt_6 = as.numeric(Total_Prot < 6),
    Total_Prot_ge_8.3 = as.numeric(Total_Prot >= 8.3)
  ) %>%
  summlab2(.)

npat(Total_Prot)

## Vitals

# BP
bp_all <- readRDS(paste0(C2023dir, "vital/Vital.bp.", cohort, ".RDS")) %>%
  inner_join(DMcohortICNs) %>%
  mutate(dt = just_date(VitalSignTakenDateTime)) %>%
  select(-VitalSignTakenDateTime) %>%
  filter(Systolic >= 50 & Systolic <= 300 & Diastolic >= 30 & Diastolic <= 200) %>%
  mutate(PulsePressure = Systolic - Diastolic) %>%
  filter(PulsePressure >= 10 & PulsePressure <= 150)

SBP <- bp_all %>%
  select(PatientICN, dt, value = Systolic, -Diastolic, -PulsePressure) %>%
  mutate(value = as.numeric(value)) %>% # not integer
  summlab(., 50, 300, "SBP", domain = "Vitals") %>%
  mutate(
    Hypotension_SBP_lt_90 = as.numeric(SBP < 90),
    SBP_ge_120 = as.numeric(SBP >= 120),
    m_130_le_SBP_lt_140 = as.numeric(SBP >= 130 & SBP < 140),
    SBP_ge_140 = as.numeric(SBP >= 140)
  ) %>%
  summlab2(., domain = "Vitals")

DBP <- bp_all %>%
  select(PatientICN, dt, value = Diastolic, -Systolic, -PulsePressure) %>%
  mutate(value = as.numeric(value)) %>% # not integer
  summlab(., 30, 200, "DBP", domain = "Vitals")

PulsePressure <- bp_all %>%
  select(PatientICN, dt, value = PulsePressure, -Systolic, -Diastolic) %>%
  mutate(value = as.numeric(value)) %>% # not integer
  summlab(., 10, 150, "PulsePressure", domain = "Vitals") %>%
  mutate(
    PulsePr_lt_30 = as.numeric(PulsePressure < 30),
    m_50_le_PulsePr_lt_70 = as.numeric(PulsePressure >= 50 & PulsePressure < 70),
    m_70_le_PulsePr_lt_90 = as.numeric(PulsePressure >= 70 & PulsePressure < 90),
    PulsePr_ge_90 = as.numeric(PulsePressure >= 90)
  ) %>%
  summlab2(., domain = "Vitals")
rm(bp_all)

# Height/weight/BMI
vitals <- readRDS(paste0(C2023dir, "vital/Vital.other.", cohort, ".RDS")) %>%
  inner_join(DMcohortICNs) %>%
  mutate(dt = just_date(VitalSignTakenDateTime)) %>%
  select(-VitalSignTakenDateTime) %>%
  dplyr::rename(value = VitalResultNumeric)

height <- vitals %>%
  filter(VitalTypeAbbreviation == "HT") %>%
  group_by(PatientICN) %>%
  summarize(height_m = median(value, na.rm = T) * 0.0254)

BMI <- vitals %>%
  filter(VitalTypeAbbreviation == "WT") %>%
  left_join(height) %>%
  mutate(
    weight_kg = value * 0.453592,
    BMI = round(weight_kg / (height_m^2), 1)
  ) %>%
  distinct(PatientICN, dt,
    value = BMI
  ) %>%
  summlab(., 10, 100, "BMI", domain = "Vitals") %>%
  mutate(
    Underweight_BMI_lt_18.5 = as.numeric(BMI < 18.5),
    Overweight_25_le_BMI_lt_30 = as.numeric(BMI >= 25 & BMI < 30),
    Obese_BMI_ge_30 = as.numeric(BMI >= 30)
  ) %>%
  summlab2(., domain = "Vitals")

Pulse <- vitals %>%
  filter(VitalTypeAbbreviation == "P") %>%
  summlab(., 25, 175, "Pulse", domain = "Vitals") %>%
  left_join(DMcohort %>% select(PatientICN, Gender)) %>%
  mutate(
    Low_PulseRate = as.numeric((Gender == "M" & Pulse < 66) | 
                                 (Gender == "F" & Pulse < 70)),
    High_PulseRate = as.numeric((Gender == "M" & Pulse >= 82) | 
                                  (Gender == "F" & Pulse >= 85))
  ) %>%
  summlab2(.)
labs_export_2$upper_chr[labs_export_2$feature_name == "Low_PulseRate"] <- 
  "66 (Male) or 70 (Female)"
labs_export_2$lower_chr[labs_export_2$feature_name == "High_PulseRate"] <- 
  "82 (Male) or 85 (Female)"

Respiration <- vitals %>%
  filter(VitalTypeAbbreviation == "R") %>%
  summlab(., 5, 100, "Respiration", domain = "Vitals") %>%
  mutate(
    m_17_le_RRate_lt_20 = as.numeric(Respiration >= 17 & Respiration < 20),
    RRate_ge_20 = as.numeric(Respiration >= 20)
  ) %>%
  summlab2(., domain = "Vitals")

PO2 <- vitals %>%
  filter(VitalTypeAbbreviation == "PO2") %>%
  summlab(., 80, 100, "PO2", domain = "Vitals") %>%
  mutate(
    PO2_lt_88 = as.numeric(PO2 < 88),
    m_88_le_PO2_lt_91 = as.numeric(PO2 >= 88 & PO2 < 91),
    m_91_le_PO2_lt_95 = as.numeric(PO2 >= 91 & PO2 < 95)
  ) %>%
  summlab2(., domain = "Vitals")

Pain <- vitals %>%
  filter(VitalTypeAbbreviation == "PN") %>%
  summlab(., 0, 10, "Pain", domain = "Vitals") %>%
  mutate(Pain_gt_0 = as.numeric(Pain > 0)) %>%
  summlab2(., domain = "Vitals")

# Cleanup
rm(Labs)
gc()


## READ IN PROCEDURES, DEFINE PROCEDURE VARIABLES #############################
# read in necessary procedures data
proc9 <- readRDS(paste0(C2023dir, "inpat/iProc9.", cohort, ".RDS")) %>%
  distinct(PatientICN, dt = AdmitDateTime, ICD9ProcedureCode) %>% 
  inner_join_quiet(DMcohortICNs) %>%
  mutate(dt = just_date(dt)) %>%
  filter(dt <= C2023_PULL_DATE) %>%
  filter(ICD9ProcedureCode != "*" & !is.na(ICD9ProcedureCode) & !is.na(dt))

# These were saved as numeric but they should really be ccharacter. 
# Fix it to be able to process and join to dict
proc9$code.chr <- as.character(proc9$ICD9ProcedureCode)
proc9 <- proc9 %>%
  mutate(code.chr = case_when(
    nchar(code.chr) == 2 ~ paste0(code.chr, ".0"),
    substr(code.chr, 2, 2) == "." ~ paste0("0", code.chr),
    TRUE ~ code.chr
  ))

proc9 <- proc9 %>%
  mutate(ICD9ProcedureCode = code.chr) %>%
  select(-code.chr) %>%
  mutate(Source = "InpatProc")

proc10 <- readRDS(paste0(C2023dir, "inpat/iProc10.", cohort, ".RDS")) %>%
  distinct(PatientICN, dt = AdmitDateTime, ICD10ProcedureCode) %>% 
  inner_join_quiet(DMcohortICNs) %>%
  mutate(dt = just_date(dt)) %>%
  filter(dt <= C2023_PULL_DATE) %>%
  filter(ICD10ProcedureCode != "*" & !is.na(ICD10ProcedureCode) & !is.na(dt)) %>%
  mutate(Source = "InpatProc")

oCPT <- readRDS(paste0(C2023dir, "outpat/oCPT.", cohort, ".RDS")) %>%
  distinct(PatientICN, dt = VisitDateTime, CPTCode) %>% 
  inner_join_quiet(DMcohortICNs) %>%
  mutate(Source = "OutpatProc")
iCPT <- readRDS(paste0(C2023dir, "inpat/iCPT.", cohort, ".RDS")) %>%
  distinct(PatientICN, dt = CPTProcedureDateTime, CPTCode) %>% 
  inner_join_quiet(DMcohortICNs) %>%
  mutate(Source = "InpatProc")
fCPT <- readRDS(paste0(C2023dir, "fee/FeeService.CPT.", cohort, ".RDS")) %>%
  distinct(PatientICN, dt = InitialTreatmentDateTime, CPTCode) %>% 
  inner_join_quiet(DMcohortICNs) %>%
  mutate(Source = "FeeProc")
CPT <- base::rbind(oCPT, iCPT, fCPT) %>%
  mutate(dt = just_date(dt)) %>%
  filter(dt <= C2023_PULL_DATE) %>%
  filter(CPTCode != "*" & !is.na(CPTCode) & !is.na(dt))
rm(oCPT, iCPT, fCPT)
gc()

KidneyTransplant_Proc <- extract.detail.proc(
  "KidneyTransplant_Proc",
  c("^5032[35789]|503[46]0|^50365|^50370|^50547|^7677[68]|^00868|^S2065", 
    "^55.69", "^0TY[01]0Z[012]")
)
Dialysis_Proc_patterns <- c(
  paste0("^050[57]F$|^36516$|^3690[1-9]$|^405[2-5]F$|^909[69][0-9]$", 
         "|^9092[145]$|^9093[579]$|^9094[012347]$|^9095[1-9]$|^9097[06789]$", 
         "|^9098[234589]$|^99512$|^99559$|^A475[05]$|^E1590$|^G2057$|",
         "^G807[56]$|^G871[45]$|^G8727$|^G8956$|^G924[01]$|^G926[456]$|", 
         "^M0916$|^M092[038]$|^M093[1267]$|^M094[08]$|",
         "^M094[45]$|^M095[26]$|^S933[59]$",
    collapse = ""
  ), # CPT
  "^39.951$|^54.981$", # ICD9 proc
  "5A1D[07]0Z"
) # ICD10 proc
Dialysis_Proc <- extract.detail.proc("Dialysis_Proc", Dialysis_Proc_patterns)
rm(Dialysis_Proc_patterns)
EKG12 <- extract.detail.proc("EKG12", c("93000|93005|93010|93225", NA, NA))
rEKG <- extract.detail.proc("rEKG", c("9304[01]|93272", NA, NA))
doppler <- extract.detail.proc("doppler", c("93307|93320|93325", NA, NA))
StressTest <- extract.detail.proc("StressTest", c("9301[5678]|93350", NA, NA))
ArteryScan <- extract.detail.proc("ArteryScan", 
                                  c("93875|93880|9392[2346]", NA, NA))
VeinousStudy <- extract.detail.proc("VeinousStudy", 
                                    c("93965|9397[061]", NA, NA))
LHeartCath <- extract.detail.proc("LHeartCath", c("93510", NA, NA))
CorAngiography <- extract.detail.proc("CorAngiography", 
                                      c("93539|9354[035]|9355[56]", NA, NA))
Pacemaker <- extract.detail.proc("Pacemaker", c("9373[123]", NA, NA))
CardiacRehab <- extract.detail.proc("CardiacRehab", c("9379[78]", NA, NA))
LungCapacity <- extract.detail.proc("LungCapacity", c("93720", NA, NA))
bpMonitoring <- extract.detail.proc("bpMonitoring", c("93784|93790", NA, NA))
Stent <- extract.detail.proc("Stent", c("92980", NA, NA))
defibrillator <- extract.detail.proc("defibrillator", c("93741", NA, NA))

# Cleanup
rm(CPT, proc9, proc10)
gc()

## READ IN HEALTH FACTORS ####

## Smoking map from health factors
smoke_hf_map <- fread(paste0(input_dir, "VACS_Health_Factors_Smoking.txt"))
HFfile <- readRDS(paste0(C2023dir, "outpat/HF.", cohort, ".RDS")) %>%
  inner_join(DMcohortICNs) %>%
  dplyr::rename(dt = VisitDateTime) %>%
  mutate(dt = just_date(dt))

smoking <- inner_join(
  smoke_hf_map %>% dplyr::rename(HealthFactorType = HEALTHFACTORTYPE),
  HFfile
) %>%
  arrange(PatientICN, dt) %>%
  distinct(PatientICN, dt, SmokingFactor) %>%
  filter(!(SmokingFactor %in% c("UNKNOWN", "unknown"))) %>%
  group_by(PatientICN) %>%
  mutate(EVERSMOKER = as.numeric(cumsum(
    SmokingFactor %in% c("FORMER SMOKER", "CURRENT SMOKER")
  ) > 0)) %>%
  filter(!(SmokingFactor == "NEVER SMOKER" & EVERSMOKER == 1)) %>%
  filter(row_number() == 1 | SmokingFactor != lag(SmokingFactor)) %>%
  ungroup() %>%
  mutate(SmokingFactor = gsub(" ", "", SmokingFactor)) %>%
  td_context()
extraction_details_export <- full_join(
  extraction_details_export,
  data.frame(
    feature_name = c("FORMERSMOKER", "CURRENTSMOKER", "EVERSMOKER"),
    domain = "Demographics", field_name = "SmokingFactor"
  )
)

rm(HFfile)
gc()

## Add time-dependent context to variables
extraction_details_export <- extraction_details_export %>% 
  filter(feature_name != "")
labs_export_1 <- labs_export_1 %>% filter(marker_name != "")
for (f in unique(c(extraction_details_export$feature_name, 
                   labs_export_1$marker_name))) {
  if (exists(f)) {
    suppressMessages(assign(f, get(f) %>% td_context()))
  } else {
    warning(paste0("Feature name ", f, " not found"))
  }
}

## DEFINE DERIVED VARIABLES AND DATASETS ######################################

# eGFR -  calculate from Serum/blood creatinine
EGFR <- Creat_BSP %>%
  inner_join_quiet(DMcohort %>% dplyr::select(PatientICN, BirthDate, Gender)) %>%
  mutate(
    age_creat = dt - BirthDate,
    K = case_when(
      Gender == "M" ~ 0.9,
      Gender == "F" ~ 0.7,
      TRUE ~ NA
    ),
    ALPHA = case_when(
      Gender == "M" ~ -0.302,
      Gender == "F" ~ -0.241,
      TRUE ~ NA
    ),
    F_FACTOR = case_when(
      Gender == "M" ~ 1,
      Gender == "F" ~ 1.012,
      TRUE ~ NA
    )
  ) %>%
  mutate(EGFR = 
           142 * (pmin(Creat_BSP / K, 1)^ALPHA) * 
           (pmax(Creat_BSP / K, 1)^-1.2) * 
           0.9938^age_creat * F_FACTOR) %>%
  dplyr::select(PatientICN, dt, EGFR) %>%
  mutate(
    m_60_le_eGFR_lt_90 = as.numeric(EGFR >= 60 & EGFR < 90),
    m_45_le_eGFR_lt_60 = as.numeric(EGFR >= 45 & EGFR < 60),
    m_30_le_eGFR_lt_45 = as.numeric(EGFR >= 30 & EGFR < 45),
    m_15_le_eGFR_lt_30 = as.numeric(EGFR >= 15 & EGFR < 30),
    eGFR_lt_15 = as.numeric(EGFR < 15)
  ) %>%
  summlab2(.) %>%
  td_context()
names(EGFR)

labs_export_1 <- full_join(labs_export_1, data.frame(
  marker_name = "EGFR", LabChemTestName = "See Creat_BSP", 
  feature_name = "EGFR_continuous", domain = "Labs"
))

# fuction for persistent low egfr given threshold and minimum number of days
plEGFR_func <- function(x, max_EGFR, min_n_days) {
  plEGFR <- x %>%
    group_by(PatientICN) %>%
    # Work with patients who ever had an egfr < threshold
    filter(any(EGFR <= max_EGFR)) %>%
    arrange(PatientICN, dt) %>%
    mutate(
      EGFRltx = EGFR <= max_EGFR,
      nextEGFRltx = lead(EGFRltx),
      previousEGFRltx = lag(EGFRltx)
    ) %>%
    # Filter out the 'middle' measurements within a sequence of low (or high) 
    # eGFR, to get the beginning and end of that sequence.
    filter(!(EGFRltx == nextEGFRltx & EGFRltx == previousEGFRltx) | 
             is.na(previousEGFRltx) | is.na(nextEGFRltx)) %>%
    mutate(days_between = days_between(dt, lead(dt))) %>%
    # extract persistent sequences of 2+ egfr < threshold, lasting # of days
    filter(EGFRltx & nextEGFRltx & days_between >= min_n_days) %>%
    # Multiple sequences may qualify (i.e. egfr was below 15 for 45+ days, 
    # popped above 15, dropped below 15 for 45+ days again)
    # Just keep the first sequence start date
    group_by(PatientICN) %>%
    summarize(first_plEGFR = min(dt))
  names(plEGFR)[2] <- paste0("first_plEGFR_", max_EGFR, "_", min_n_days)
  return(plEGFR)
}

# Persistent low egfr, <= 15, 45+ days apart, with no above 15 in between
plEGFR_15_45 <- plEGFR_func(EGFR, 15, 45)
npr(plEGFR_15_45)

# < 15 for 90 days
plEGFR_15_90 <- plEGFR_func(EGFR, 15, 90)
npr(plEGFR_15_90)

# EGFR less than 30 for 90+days
plEGFR_30_90 <- plEGFR_func(EGFR, 30, 90)
npr(plEGFR_30_90)

# EGFR less than 45 for 90 days
plEGFR_45_90 <- plEGFR_func(EGFR, 45, 90)
npr(plEGFR_45_90) #

# EGFR < 60 for 90 days
plEGFR_60_90 <- plEGFR_func(EGFR, 60, 90)
npr(plEGFR_60_90) #

# EGFR < 90 for 90 days
plEGFR_90_90 <- plEGFR_func(EGFR, 90, 90)
npr(plEGFR_90_90) #


# Combine related categories for renal failure outcome
RenalFailure_AllCodes <- rbind(
  Dialysis_Proc %>% mutate(code_type = "Dialysis_ESRD"),
  KidneyTransplant_Proc %>% mutate(code_type = "KidneyTransplant"),
  ESRD_Dx %>% dplyr::rename(code = ICDCode),
  fill = T
) %>%
  distinct()
npr(RenalFailure_AllCodes)

# # Number of days beyond which we start a new sequence
# # Minimum length of sequence in days to qualify for the event
code_sequence <- function(x, min_sequence_days, new_sequence_days, 
                          min_n_codes, prefix = "seq") {
  seqs <- x %>%
    distinct() %>%
    group_by(PatientICN) %>%
    filter(n() >= min_n_codes) %>%
    arrange(PatientICN, dt) %>%
    mutate(
      days_since_last_dt = days_between(lag(dt), dt),
      new_seq = days_since_last_dt > new_sequence_days | 
        is.na(days_since_last_dt),
      seqno = cumsum(new_seq)
    ) %>%
    group_by(PatientICN, seqno) %>%
    mutate(seq_qual = days_between(min(dt), max(dt)) >= min_sequence_days) %>%
    group_by(PatientICN) %>%
    mutate(
      any_seq_qual = any(seq_qual),
      first_seq_qual = min(seqno[which(seq_qual)]),
      first_seq_qual = ifelse(is.infinite(first_seq_qual), NA, first_seq_qual)
    )
  rtn <- seqs %>%
    filter(seq_qual & seqno == first_seq_qual) %>%
    group_by(PatientICN) %>%
    summarise(first_seq = min(dt))
  names(rtn)[2] <- paste0("first_", prefix, ".", min_n_codes, ".", 
                          min_sequence_days, ".", new_sequence_days)
  return(rtn)
}

RenalFailure <- code_sequence(RenalFailure_AllCodes, 
                              45, 365, 2, "RenalFailure") %>%
  full_join(plEGFR_15_45) %>%
  mutate(first_RenalFailure = pmin(first_RenalFailure.2.45.365, 
                                   first_plEGFR_15_45, na.rm = T))
npat(RenalFailure)
npat(RenalFailure) / cohort_n

table(RenalFailure$PatientICN %in% ACUTERF_Dx$PatientICN)

CKD4_AllCodes <- rbind(
  CKD4_Dx %>% dplyr::rename(code = ICDCode),
  RenalFailure_AllCodes %>% select(-code_vocabulary, -Description)
) %>%
  distinct()
CKD4 <- code_sequence(CKD4_AllCodes, 90, 365, 2, "CKD4") %>%
  full_join_quiet(plEGFR_30_90) %>%
  full_join_quiet(RenalFailure) %>%
  mutate(first_CKD4 = pmin(first_CKD4.2.90.365, first_plEGFR_30_90, 
                           first_RenalFailure, na.rm = T))
npat(CKD4)
npat(CKD4) / cohort_n

CKD3b_AllCodes <- rbind(CKD3b_Dx %>% dplyr::rename(code = ICDCode), 
                        CKD4_AllCodes) %>%
  distinct()
CKD3b <- code_sequence(CKD3b_AllCodes, 90, 365, 2, "CKD3b") %>%
  full_join_quiet(plEGFR_45_90) %>%
  full_join_quiet(CKD4) %>%
  mutate(first_CKD3b = pmin(first_CKD3b.2.90.365, first_plEGFR_45_90, 
                            first_CKD4, na.rm = T))
npat(CKD3b)
npat(CKD3b) / cohort_n

CKD3a_AllCodes <- rbind(CKD3a_Dx %>% dplyr::rename(code = ICDCode), 
                        CKD3b_AllCodes) %>%
  distinct()
CKD3a <- code_sequence(CKD3a_AllCodes, 90, 365, 2, "CKD3a") %>%
  full_join_quiet(plEGFR_60_90) %>%
  full_join_quiet(CKD3b) %>%
  mutate(first_CKD3a = pmin(first_CKD3a.2.90.365, first_plEGFR_60_90, 
                            first_CKD3b, na.rm = T))
npat(CKD3a)
npat(CKD3a) / cohort_n

CKD_AllCodes <- rbind(CKD_Dx %>% dplyr::rename(code = ICDCode), 
                      CKD3a_AllCodes) %>%
  distinct()
CKD <- code_sequence(CKD_AllCodes, 90, 365, 2, "CKD") %>%
  full_join_quiet(CKD3a) %>%
  mutate(first_CKD = pmin(first_CKD.2.90.365, first_CKD3a, na.rm = T))
npat(CKD)
npat(CKD) / cohort_n

# Macroalbuminuria
macroalbuminuria <- UACR %>%
  filter(Macroalbuminuria_UACR_ge_300 == 1) %>%
  group_by(PatientICN, Macroalbuminuria_UACR_ge_300) %>%
  summarize(first_macroalbuminuria = min(dt), .groups = "drop")

# Microalbuminuria
microalbuminuria <- UACR %>%
  filter(Microalbuminuria_30_le_UACR_lt_300 == 1) %>%
  group_by(PatientICN, Microalbuminuria_30_le_UACR_lt_300) %>%
  summarize(first_microalbuminuria = min(dt), .groups = "drop")


# #Combine MI and stroke (non-hemorrhagic) to get CVD
CVD <- base::rbind(MI, OldMI, Stroke_infarct)

## Join event tables to those without event, define indicators and tte

# DR time to event / censoring
DiabeticRetinopathy_tte <- DiabeticRetinopathy %>%
  group_by(PatientICN) %>%
  summarize(first_DiabeticRetinopathy = min(dt)) %>%
  mutate(DiabeticRetinopathy = 1) %>%
  # last visit date for those without DR
  right_join_quiet(DMcohort %>% dplyr::select(PatientICN, DmDx_first, 
                                              censor_date)) %>%
  mutate(DiabeticRetinopathy = coalesce(DiabeticRetinopathy, 0)) %>%
  # Define time to event / time to censoring
  mutate(DiabeticRetinopathy_tte = coalesce(
    first_DiabeticRetinopathy - DmDx_first,
    censor_date - DmDx_first
  )) %>%
  dplyr::select(PatientICN, first_DiabeticRetinopathy, DiabeticRetinopathy, 
                DiabeticRetinopathy_tte)
prop.table(table(DiabeticRetinopathy_tte$DiabeticRetinopathy, useNA = "always"))

# RF time to event / time to censoring
RenalFailure_tte <- RenalFailure %>%
  mutate(RenalFailure = 1) %>%
  right_join_quiet(DMcohort %>% 
                     dplyr::select(PatientICN, DmDx_first, censor_date)) %>%
  mutate(RenalFailure = coalesce(RenalFailure, 0)) %>%
  mutate(RenalFailure_tte = coalesce(first_RenalFailure - DmDx_first, 
                                     censor_date - DmDx_first)) %>%
  dplyr::select(PatientICN, first_RenalFailure, RenalFailure, RenalFailure_tte)
prop.table(table(RenalFailure_tte$RenalFailure, useNA = "always"))

CKD_tte <- CKD %>%
  mutate(CKD = 1) %>%
  right_join_quiet(DMcohort %>% dplyr::select(PatientICN, DmDx_first, 
                                              censor_date)) %>%
  mutate(CKD = coalesce(CKD, 0)) %>%
  mutate(CKD_tte = coalesce(first_CKD - DmDx_first, 
                            censor_date - DmDx_first)) %>%
  dplyr::select(PatientICN, first_CKD, CKD, CKD_tte)

CKD3a_tte <- CKD3a %>%
  mutate(CKD3a = 1) %>%
  right_join_quiet(DMcohort %>% 
                     dplyr::select(PatientICN, DmDx_first, censor_date)) %>%
  mutate(CKD3a = coalesce(CKD3a, 0)) %>%
  mutate(CKD3a_tte = coalesce(first_CKD3a - DmDx_first, 
                              censor_date - DmDx_first)) %>%
  dplyr::select(PatientICN, first_CKD3a, CKD3a, CKD3a_tte)

CKD3b_tte <- CKD3b %>%
  mutate(CKD3b = 1) %>%
  right_join_quiet(DMcohort %>% 
                     dplyr::select(PatientICN, DmDx_first, censor_date)) %>%
  mutate(CKD3b = coalesce(CKD3b, 0)) %>%
  mutate(CKD3b_tte = coalesce(first_CKD3b - DmDx_first, 
                              censor_date - DmDx_first)) %>%
  dplyr::select(PatientICN, first_CKD3b, CKD3b, CKD3b_tte)

CKD4_tte <- CKD4 %>%
  mutate(CKD4 = 1) %>%
  right_join_quiet(DMcohort %>% 
                     dplyr::select(PatientICN, DmDx_first, censor_date)) %>%
  mutate(CKD4 = coalesce(CKD4, 0)) %>%
  mutate(CKD4_tte = coalesce(first_CKD4 - DmDx_first, 
                             censor_date - DmDx_first)) %>%
  dplyr::select(PatientICN, first_CKD4, CKD4, CKD4_tte)
mean(CKD4_tte$CKD4)

rm(
  CKD3a_AllCodes, CKD3a_Dx, CKD3b_AllCodes, CKD3b_Dx, CKD4_AllCodes, CKD4_Dx, 
  RenalFailure_AllCodes, CKD5_Dx, ESRD_Dx, CKD_Dx, CKD_AllCodes
)


## Extract baseline (T0 = DmDx) covariates  ###################################

# Baseline T0 values for biomarkers
baseline_biomarkers <- DMcohortICNs
for (f in unique(c(labs_export_1$marker_name))) { 
  if (exists(f)) {
    rec <- get(f) %>%
      filter(yrs <= 1 / 365 & yrs >= -2) %>%
      group_by(PatientICN) %>%
      slice_max(yrs, with_ties = FALSE) %>%
      distinct(PatientICN, .data[[f]]) %>%
      dplyr::rename_with(function(x) {
        paste0(x, "0")
      }, .cols = all_of(f))
    baseline_biomarkers <- full_join_quiet(baseline_biomarkers, rec)
    rm(rec)
  } else {
    warning(paste0("Feature name ", f, " not found"))
  }
}

# Baseline comorbidity scores and health factors
baseline_DCSI <- DCSI %>%
  filter(yrs <= 1 / 365 & yrs >= -99) %>%
  group_by(PatientICN) %>%
  slice_max(yrs, with_ties = FALSE) %>%
  dplyr::rename_with(function(x) {
    paste0(x, "0")
  }, .cols = "DCSI") %>%
  select(-c(dt, yrs, age))
baseline_DCSI_excl_neph <- DCSI_excl_neph %>%
  filter(yrs <= 1 / 365 & yrs >= -99) %>%
  group_by(PatientICN) %>%
  slice_max(yrs, with_ties = FALSE) %>%
  dplyr::rename_with(function(x) {
    paste0(x, "0")
  }, .cols = "DCSI_excl_neph") %>%
  select(-c(dt, yrs, age))
baseline_DCSI_excl_cvd <- DCSI_excl_cvd %>%
  filter(yrs <= 1 / 365 & yrs >= -99) %>%
  group_by(PatientICN) %>%
  slice_max(yrs, with_ties = FALSE) %>%
  dplyr::rename_with(function(x) {
    paste0(x, "0")
  }, .cols = "DCSI_excl_cvd")
baseline_Elix_RawScore <- elix.score.nodm %>%
  filter(yrs <= 1 / 365 & yrs >= -99) %>%
  group_by(PatientICN) %>%
  slice_max(yrs, with_ties = FALSE) %>%
  dplyr::rename_with(function(x) {
    paste0(x, "0")
  }, .cols = "Elix_RawScore") %>%
  select(-c(dt, yrs, age))
baseline_Elix_excl_RF <- elix_excl_RF %>%
  filter(yrs <= 1 / 365 & yrs >= -99) %>%
  group_by(PatientICN) %>%
  slice_max(yrs, with_ties = FALSE) %>%
  dplyr::rename_with(function(x) {
    paste0(x, "0")
  }, .cols = "Elix_excl_RF") %>%
  select(-c(dt, yrs, age))
baseline_smoking <- smoking %>%
  filter(yrs <= 1 / 365 & yrs >= -99) %>%
  group_by(PatientICN) %>%
  slice_max(yrs, with_ties = FALSE) %>%
  dplyr::rename_with(function(x) {
    paste0(x, "0")
  }, .cols = all_of(c("SmokingFactor", "EVERSMOKER"))) %>%
  select(-c(dt, yrs, age))

MI <- rbind(MI, OldMI) %>% distinct()
rm(OldMI)

med_features <- extraction_results_export %>%
  filter(domain == "Medications") %>%
  distinct(feature_name) %>%
  .$feature_name
length(med_features)
cond_proc_features <- extraction_results_export %>%
  filter(domain != "Medications") %>%
  distinct(feature_name) %>%
  .$feature_name
length(cond_proc_features)

# Baseline T0 values for conditions and procedures, and EVER use of meds
baseline_cond_proc_med <- DMcohortICNs
for (f in c(cond_proc_features, med_features)) {
  if (exists(f)) {
    rec <- get(f) %>%
      filter(yrs <= 1 / 365 & yrs >= -99) %>%
      distinct(PatientICN) %>%
      mutate(hist_present = 1) %>%
      dplyr::rename_with(function(x) {
        paste0(f, "_ever0")
      }, .cols = all_of("hist_present"))
    baseline_cond_proc_med <- full_join_quiet(baseline_cond_proc_med, rec)
    rm(rec)
  } else {
    warning(paste0("Feature name ", f, " not found"))
  }
}
baseline_cond_proc_med <- baseline_cond_proc_med %>%
  mutate(across(ends_with("ever0"), function(x) {
    coalesce(x, 0)
  }))

data_wide <- full_join(DMcohort, baseline_biomarkers) %>%
  full_join(baseline_smoking) %>%
  full_join(baseline_cond_proc_med) %>%
  full_join(baseline_DCSI) %>%
  full_join(baseline_DCSI_excl_cvd) %>%
  full_join(baseline_DCSI_excl_neph) %>%
  full_join(baseline_Elix_RawScore) %>%
  full_join(baseline_Elix_excl_RF) %>%
  full_join(RenalFailure_tte) %>%
  full_join_quiet(CKD4_tte) %>%
  full_join_quiet(CKD3b_tte) %>%
  full_join_quiet(CKD3a_tte) %>%
  full_join_quiet(CKD_tte) %>%
  mutate(cohort)
npr(data_wide)

saveRDS(data_wide, 
        paste0(cohort_dir, "/coh", cohort, "/RDS/data_wide_", cohort, ".RDS"))

## SAVE DATA #################################################################

rm(
  Dx, Rx, Labs, CPT, proc9, proc10, vitals, prelim_cohort, baseline_biomarkers, 
  baseline_cond_proc_med, baseline_smoking, baseline_DCSI, 
  baseline_DCSI_excl_cvd, baseline_DCSI_excl_neph, baseline_Elix_excl_RF,
  baseline_Elix_RawScore
)
gc()

object_sizes <- data.frame(obj = ls(), 
                           size = sapply(ls(), 
                                         function(x) object.size(get(x)))) %>%
  arrange(desc(size)) %>%
  mutate(size_MB = size / 1024 / 1024) %>%
  select(-size) %>%
  mutate(cohort)
saveRDS(object_sizes, 
        paste0(cohort_dir, "/coh", cohort, "/RDS/object_sizes_", cohort, ".RDS"))


# save results
dir.create(results_dir_export_details, showWarnings = FALSE)
saveRDS(extraction_details_export, 
        paste0(results_dir_export_details, "patterns.", cohort, ".RDS"))
saveRDS(extraction_results_export, 
        paste0(results_dir_export_details, "cond_proc_med.", cohort, ".RDS"))
saveRDS(labs_export_1, 
        paste0(results_dir_export_details, "labs_cont.", cohort, ".RDS"))
saveRDS(labs_export_2, 
        paste0(results_dir_export_details, "labs_cat.", cohort, ".RDS"))
rm(results_dir_export_details)

# Save image of all objects
save.image(paste0(cohort_dir, "coh", cohort, "/data.RData"))

# remove the previous checkpoint
file.remove(paste0(cohort_dir, "coh", cohort, "/RDS/cohort.", cohort, ".RDS"))

# Cleanup
rm(list = ls())
gc()

print("End of Script")
