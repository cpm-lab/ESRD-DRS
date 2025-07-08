# Calculate SHAP values for each patient/predictor/landmark/horizon

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
cutgroup <- as.numeric(args[10])
HORIZ <- as.numeric(args[11])


#Run from parent directory
source("config_mnt.R")
source("functions.R")


library(tidyverse)
library(tictoc)
library(survival)
library(stringr)
library(performance)
library(tidyr)
library(fastcmprsk)
library(ggplot2)
library(ggrepel)
library(Hmisc)
library(timeROC)
library(prodlim)
library(purrr)
library(pracma)
library(cmprsk)
library(survival)
library(survminer)
# library(cowplot)
library(fastshap)
library(parallel)
library(doParallel)

SEED <- 12345
set.seed(SEED)


# substring for y1 MCP model
subsave_Y1 <- paste(outcome, "r1", "imputed",
  paste0("mincorr", minCorr),
  paste0("SEED", SEED),
  selection_method,
  paste0("reduce", reduction_method),
  paste0("CVcrit", cv_criterion),
  sep = "."
)

subsave_Y1

# substring for refit models
subsave <- paste(subsave_Y1,
  paste0("L", landmark_year),
  fit_method,
  sep = "."
)
subsave


# Read in the testing set data
subsave_datafile <- paste0("data.postxform.landmark_y", landmark_year, 
                           ".test.", outcome, ".r1.imputed.mincorr", minCorr)
datafile <- paste0(preprocess_dir, "cohAll/", outcome, "/imputed/", 
                   subsave_datafile, ".RDS")
file.exists(datafile)
d_test <- readRDS(datafile)

# read in the training set data
datafile_train <- gsub("test", "train", datafile)
file.exists(datafile_train)
d_train <- readRDS(datafile_train)

# Randomly subset TEST data so that subsets are similar
rids <- sample(1:10, nrow(d_test), replace = T)
table(rids)
nrow(d_test)
cutgroup_patients <- d_test %>%
  select(PatientICN) %>%
  mutate(cutgroup = rids, L = landmark_year)
d_test <- d_test[rids == cutgroup, ] # cutgroup from command arg
nrow(d_test)

# write out subset IDs so that patient IDS and other data can be joined later
shap_dir <- paste0(results_dir_test, "SHAP/")
dir.create(shap_dir)
output_file_IDs <- paste0(shap_dir, "SHAP_IDs.", landmark_year, 
                          ".", subsave, ".RDS")
output_file_IDs
saveRDS(cutgroup_patients, output_file_IDs)

d_preserve_test <- d_test %>%
  select(PatientICN, event01:Elix_RawScore, event_CR, CR_tte, censtime) %>%
  select(-any_of(c("Elix_excl_RF", "Gender", "MaritalStatus", "Race", 
                   "Ethnicity", "DCSI")))
d_pred_test <- d_test %>% select(-any_of(names(d_preserve_test)[-1]))

# Format/variable names must match that used in training
d_pred_test <- model.matrix(~., d_pred_test)[, -1] %>% data.frame()
names(d_pred_test) <- gsub("1$", "", names(d_pred_test))

d_preserve_train <- d_train %>%
  select(PatientICN, event01:Elix_RawScore, event_CR, CR_tte, censtime) %>%
  select(-any_of(c("Elix_excl_RF", "Gender", "MaritalStatus", "Race", 
                   "Ethnicity", "DCSI")))
d_pred_train <- d_train %>% select(-any_of(names(d_preserve_train)[-1]))

# Format/variable names must match that used in training
d_pred_train <- model.matrix(~., d_pred_train)[, -1] %>% data.frame()
names(d_pred_train) <- gsub("1$", "", names(d_pred_train))

# Use Y1M for L1, use refit models for L5 and L10
if (landmark_year == 1) {
  dummy_fcrr <- list.files(paste0(results_dir_train, "main/"),
    full.names = T,
    pattern = "4.y1.dummy.main"
  ) %>% readRDS(.)
} else {
  dummy_fcrr <- list.files(paste0(results_dir_train, "main/"),
    full.names = T,
    pattern = paste0("11.REFIT.unpenalized.*L", landmark_year)
  ) %>% readRDS(.)
}

# File name for saving the  SHAP results
output_file_shap <- paste0(shap_dir, "SHAP.", landmark_year, ".main.h", 
                           HORIZ, ".", subsave, ".group", cutgroup, ".RDS")
output_file_shap

## Run SHAP analysis #####

# Prediction wrappers
pfun1 <- function(object, newdata) {
  pred <- predict_fcrr_multirow(
    model = object, newdata = newdata %>% select(-any_of(c("ftime", "fstatus"))),
    times = 1, silent = T
  )
  return(pred$pred1)
}
pfun5 <- function(object, newdata) {
  pred <- predict_fcrr_multirow(
    model = object, newdata = newdata %>% select(-any_of(c("ftime", "fstatus"))),
    times = 5, silent = T
  )
  return(pred$pred5)
}
pfun10 <- function(object, newdata) {
  pred <- predict_fcrr_multirow(
    model = object, newdata = newdata %>% select(-any_of(c("ftime", "fstatus"))),
    times = 10, silent = T
  )
  return(pred$pred10)
}

# Compute approximate Shapley values using 15 Monte Carlo simulations
d_pred_train <- d_pred_train %>% select(all_of(names(coef(dummy_fcrr))))
d_pred_test <- d_pred_test %>% select(all_of(names(coef(dummy_fcrr))))

print("Using 15 cores")
registerDoParallel(cores = 15) 
n_MCsim <- 15

# Select the right wrapper function
pwrap <- get(paste0("pfun", HORIZ))

tictoc::tic(paste0("Time to run SHAP- with parallel option - all features - ", 
                   HORIZ, " year horizon"))
shap <- fastshap::explain(dummy_fcrr,
  X = d_pred_train %>% data.frame(),
  newdata = d_pred_test %>% data.frame(),
  nsim = n_MCsim,
  pred_wrapper = pwrap,
  shap_only = FALSE,
  parallel = TRUE
)
tictoc::toc()

# Save the SHAP results ####
saveRDS(shap, output_file_shap)

print("End of Script")
