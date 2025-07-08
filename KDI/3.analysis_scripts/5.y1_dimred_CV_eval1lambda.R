# Evaluate each lambda from each CV fold

args <- commandArgs(trailingOnly = T)
CVFOLD <- as.numeric(args[1])
selection_method <- args[2]
outcome <- args[3]
minCorr <- as.numeric(args[4])
lindex <- as.numeric(args[5])
reduction_method <- args[6]

# Run from parent directory
source("config_mnt.R")
source("functions.R")

library(dplyr)
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
library(pracma)
library(cmprsk)

# Set seed for random splitting
SEED <- 12345
set.seed(SEED)


# Create a string which will be used in output file names, specifying the 
# parameters of this run
subsave <- paste(outcome,
  paste0("r1.imputed.mincorr", minCorr),
  paste0("SEED", SEED),
  selection_method,
  paste0("CVFOLD", CVFOLD),
  paste0("reduce", reduction_method),
  sep = "."
)
subsave

# Read in data snapshot at landmark year 1
read_dir <- paste0(preprocess_dir, "cohAll/", outcome, "/imputed/")

# directory for fold
rsplit_dir <- paste0(results_dir_train, "CVfold", CVFOLD, "/")
fold_summary <- readRDS(paste0(rsplit_dir, "2.y1.cv.MODELONLY.", subsave, ".RDS"))

use_lambdas <- fold_summary$model$lambda.path
z.i <- fold_summary$z.i.train
truth.i <- fold_summary$truth.i.train
newdata_z <- fold_summary$z.i.test
newdata_truth <- fold_summary$truth.i.test
l <- fold_summary$model$lambda.path[lindex]
model <- fold_summary$model

tictoc::tic("Predicting and evaluating")

# Only run the evaluations if at lest 1 variable was selected, and convergence 
# was reached
if (any(model$coef[, lindex] != 0) & model$converged[lindex] == 1) {
  dummy_l <- fcrr_from_fcrrp(model, truth.i, input_data = z.i, l, 
                             replace_coefs = TRUE)
  print(paste0("Generating predictions for lambda ", lindex, " of ", 
               length(model$lambda.path), ": ", l))
  htimes <- c(1, 5, 10)
  predprobs <- predict_fcrr_multirow(dummy_l, newdata_z, times = htimes)

  e.l <- data.frame("horiz" = 0) %>% filter(is.na(horiz))
  for (i in 1:length(htimes)) {
    e.i <- CR_auc_simple(newdata_truth$CR_tte, newdata_truth$event_CR,
      marker = predprobs[[i]], time = htimes[i]
    ) %>%
      cbind(CR_calib_simple(predprobs[[i]], newdata_truth, htimes[i])) %>%
      cbind((CR_get_AUPRC(predprobs[[i]], 
                          newdata_truth, htimes[i], cause = 1)$AUPRC))

    e.i <- e.i %>%
      select(-horiz) %>%
      mutate(horiz = htimes[i])
    e.l <- full_join(e.l, e.i)
  }
  e.l <- e.l %>% mutate(lambda = l, lindex)
  predprobs.all <- predprobs %>%
    mutate(lambda = l, lindex) %>%
    select(paste0("pred", htimes), lindex, lambda)
} else {
  print(paste0("Skipping lambda ", lindex, ": ", l, 
               " due to nonconvergence or no variables selected"))
}

eval_measures_wide <- e.l %>%
  pivot_wider(names_from = horiz, 
              values_from = c(AUC_1, AUC_2, BrierScore, 
                              Slope, AUPRC, cif_est_prob)) %>%
  mutate(fold = CVFOLD, reduction_method)

saveRDS(predprobs.all, 
        paste0(rsplit_dir, "2.y1.cv.l", lindex, ".PROB", subsave, ".RDS"))
saveRDS(eval_measures_wide, 
        paste0(rsplit_dir, "2.y1.cv.l", lindex, ".EVAL", subsave, ".RDS"))


print("End of script")
