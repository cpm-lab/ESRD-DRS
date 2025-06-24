# Generate the predictions for L5 and L10 using unpenalized models fit at those years

args <- commandArgs(trailingOnly = T)
selection_method <- args[1]
outcome <- args[2]
minCorr <- args[3]
cv_criterion <- args[4]
reduction_method <- args[5]
fit_method <- args[6]
landmark_year <- as.numeric(args[7])
BIC_only <- as.logical(args[8])
UNPENALIZED <- as.logical(args[9])

# Run from parent directory
source("config_mnt.R")
source("functions.R")

library(tidyverse)
library(tictoc)
library(survival)
library(stringr)
library(performance)
library(tidyr)
library(fastcmprsk, lib.loc = fastcmprsk_lib)
library(ggplot2)
library(ggrepel)
library(Hmisc)
library(timeROC)
library(prodlim)
library(purrr)
library(pracma)
library(cmprsk)

SEED <- 12345
set.seed(SEED)

# String to use
subsave <- paste(outcome, "r1", "imputed",
  paste0("mincorr", minCorr),
  paste0("SEED", SEED),
  selection_method,
  paste0("reduce", reduction_method),
  paste0("CVcrit", cv_criterion),
  paste0("L", landmark_year),
  fit_method,
  sep = "."
)
subsave

# Read training results results from these file locations
train_dir_main <- paste0(results_dir_train, "main/")
unpenalized_fcrr <- paste0(train_dir_main, "11.REFIT.unpenalized.main.", 
                           subsave, ".RDS") %>% readRDS(.)

# Read in the data
subsave_datafile <- paste0("data.postxform.landmark_y", 
                           landmark_year, ".test.", outcome, 
                           ".r1.imputed.mincorr", minCorr)
datafile <- paste0(preprocess_dir, 
                   "cohAll/", outcome, "/imputed/", subsave_datafile, ".RDS")
d_test <- readRDS(datafile)

# Process
truth_test <- d_test %>% select(PatientICN, event_CR, CR_tte, censtime)
d_preserve <- d_test %>%
  select(PatientICN, event01:Elix_RawScore, event_CR, CR_tte, censtime) %>%
  select(-any_of(c("Elix_excl_RF", "Gender", "MaritalStatus", "Race", 
                   "Ethnicity", "DCSI")))
d_pred <- d_test %>% select(-any_of(names(d_preserve)[-1]))

# Format/variable names must match that used in training
d_pred <- model.matrix(~., d_pred)[, -1] %>% data.frame()
names(d_pred) <- gsub("1$", "", names(d_pred))
z_test <- d_pred

# Evaluate the performance in the testing set
tic("Generating Predictions")

if (!UNPENALIZED) {
  eval_obj <- eval_measures_fastCrr(dummy_fcrr, z_test, truth_test, 
                                    htimes = c(1, 5, 10))
} else if (UNPENALIZED) {
  eval_obj <- eval_measures_fastCrr(unpenalized_fcrr, z_test, truth_test, 
                                    htimes = c(1, 5, 10))
}

toc()

eval_obj$performance

# Save the prediction and evaluation results
output_file_pred_eval <- paste0(
  results_dir_test, "12.pred_eval.main.", subsave,
  ifelse(UNPENALIZED, ".unpenalized", ".penalized"),
  ".RDS"
)
saveRDS(eval_obj, output_file_pred_eval)

print("End of Script")
