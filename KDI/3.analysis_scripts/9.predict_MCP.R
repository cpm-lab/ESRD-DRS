# Generate predictions from Y1 model (at L1, L5, L10)
# Covariates selected by MCP - use penalized coefficients

jobid <- Sys.getenv("SLURM_JOB_ID")
print(paste0("SLURM Job ID is ", jobid))

args <- commandArgs(trailingOnly = T)
selection_method <- args[1]
outcome <- args[2]
cv_criterion <- args[3]
BIC_only <- as.logical(args[4])
minCorr <- as.numeric(args[5])
coefs <- args[6]
reduction_method <- args[7]
landmark_year <- args[8]

# Run from parent directory
source("config_mnt.R")
source("functions.R")

library(fastcmprsk, lib.loc = fastcmprsk_lib)
library(data.table)
library(dplyr)
library(tictoc)
library(survival)
library(stringr)
library(performance)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(Hmisc)
library(timeROC)
library(purrr)
library(pracma)
library(prodlim)
library(cmprsk)

SEED <- 12345
set.seed(SEED)

# string to use, specifying the parameters
subsave_MCP <- paste(outcome,
  paste0("r1.imputed.mincorr", minCorr),
  paste0("SEED", SEED),
  selection_method,
  paste0("reduce", reduction_method),
  paste0("CVcrit", cv_criterion),
  sep = "."
)
subsave_MCP

# Read in the data
subsave_datafile <- paste0("data.postxform.landmark_y", landmark_year, 
                           ".test.", outcome, ".r1.imputed.mincorr", minCorr)
datafile <- paste0(preprocess_dir, 
                   "cohAll/", outcome, "/imputed/", subsave_datafile, ".RDS")
file.exists(datafile)
d_test <- readRDS(datafile)

train_dir_main <- paste0(results_dir_train, "main/")

if (coefs == "PENALIZED") {
  dummy_fcrr <- readRDS(paste0(train_dir_main, "4.y1.dummy.main.", 
                               subsave_MCP, ".RDS"))
} else if (coefs == "UNPENALIZED") {
  dummy_fcrr <- readRDS(paste0(train_dir_main, "4.y1.unpenalized.main.", 
                               subsave_MCP, ".RDS"))
}

truth_test <- d_test %>% select(PatientICN, event_CR, CR_tte, censtime)
d_preserve <- d_test %>%
  select(PatientICN, event01:Elix_RawScore, event_CR, CR_tte, censtime) %>%
  select(-any_of(c("Elix_excl_RF", "Gender", "MaritalStatus", "Race", 
                   "Ethnicity", "DCSI")))
d_pred <- d_test %>% select(-any_of(names(d_preserve)[-1]))
rm(d_test)

# Format/variable names must match that used in training
d_pred <- model.matrix(~., d_pred)[, -1] %>% data.frame()
names(d_pred) <- gsub("1$", "", names(d_pred))

# List all the available covariates
covars_all <- names(d_pred)[-1]
length(covars_all)
length(dummy_fcrr$coef)

# Evaluate the performance in the testing set
tic("Evaluating measures")
eval_obj <- eval_measures_fastCrr(dummy_fcrr, d_pred, truth_test,
  htimes = c(1, 5, 10), n_bootstrap = 0
)
toc()

fivenum(eval_obj$pred$pred1)
fivenum(eval_obj$pred$pred5)
fivenum(eval_obj$pred$pred10)
eval_obj$performance

# Save the prediction and evaluation results
output_file_pred_eval <- paste0(results_dir_test, "5.y", landmark_year, 
                                ".pred_eval.main.", subsave_MCP, ".", 
                                coefs, ".RDS")
saveRDS(eval_obj, output_file_pred_eval)

print("End of Script")
