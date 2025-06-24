# Fit the unpenalized models at L5 and L10

args <- commandArgs(trailingOnly = T)
selection_method <- args[1]
outcome <- args[2]
minCorr <- args[3]
cv_criterion <- args[4]
reduction_method <- args[5]
fit_method <- args[6]
landmark_year <- as.numeric(args[7])
BIC_only <- as.logical(args[8])

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


subsave_Y1M <- paste(outcome,
  paste0("r1.imputed.mincorr", minCorr),
  paste0("SEED", SEED),
  selection_method,
  paste0("reduce", reduction_method),
  paste0("CVcrit", cv_criterion),
  sep = "."
)
subsave_Y1M

subsave <- paste(subsave_Y1M,
  paste0("L", landmark_year),
  fit_method,
  sep = "."
)
subsave

# Get the list of predictor variables
train_dir_main <- paste0(results_dir_train, "main/")
Y1M <- readRDS(paste0(train_dir_main, "4.y1.dummy.main.", subsave_Y1M, ".RDS"))
keep_vars <- Y1M$coef %>% names(.)
rm(Y1M)

# Load and process data
read_dir <- paste0(preprocess_dir, "cohAll/", outcome, "/imputed/")
d <- readRDS(paste0(read_dir, "data.postxform.landmark_y", 
                    landmark_year, ".train.", outcome, ".r1.imputed.mincorr", 
                    minCorr, ".RDS"))
d_preserve <- d %>%
  select(PatientICN, event01:Elix_RawScore, event_CR, CR_tte, censtime) %>%
  select(-any_of(c("Elix_excl_RF", "Gender", "MaritalStatus", "Race", 
                   "Ethnicity", "DCSI")))
d_pred <- d %>% select(-any_of(names(d_preserve)[-1]))
rm(d)

d_pred <- model.matrix(~., d_pred)[, -1] %>% data.frame()
names(d_pred) <- gsub("1$", "", names(d_pred))

input_d <- d_pred %>% select(all_of(keep_vars))
truth_train <- d_preserve %>% 
  dplyr::select(PatientICN, event_CR, CR_tte, censtime)

# refit model
unpenalized_fcrr <- fastCrr(
  formula = Crisk(truth_train$CR_tte, truth_train$event_CR, failcode = 1) ~ .,
  data = input_d,
  returnDataFrame = T,
  getBreslowJumps = T,
  variance = F
)

# Save training results results to these file locations

output_file_unpenalized <- paste0(train_dir_main, "11.REFIT.unpenalized.main.",
                                  subsave, ".RDS")
saveRDS(unpenalized_fcrr, output_file_unpenalized)


print("End of Script")
