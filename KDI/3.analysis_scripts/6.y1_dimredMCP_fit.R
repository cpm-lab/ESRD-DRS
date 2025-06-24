# Choose the best lambda for MCP and fit the model

args <- commandArgs(trailingOnly = T)
selection_method <- args[1]
outcome <- args[2]
minCorr <- args[3]
cv_criterion <- args[4]
reduction_method <- args[5]

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

# Create a string which will be used in output file names, specifying the 
# parameters of this run
subsave <- paste(outcome,
  paste0("r1.imputed.mincorr", minCorr),
  paste0("SEED", SEED),
  selection_method,
  ## paste0("reduce", reduction_method),
  sep = "."
)
subsave

# Read in the data for this fold
load(paste0(results_dir_train, "CVfold1/1.y1.data.", subsave, ".CVFOLD1.RData"))

# Read in the cross-validation results
# Evaluations
fls <- list.files(results_dir_train, recursive = T, full.names = T, 
                  pattern = "EVAL.*reduce")
# Model info
fls2 <- list.files(results_dir_train, recursive = T, full.names = T, 
                   pattern = "MODELONLY.*reduce")
readRDSfl2 <- function(x) {
  readRDS(x)$summary %>% mutate(flnm = x)
}
readRDSfl1 <- function(x) {
  readRDS(x) %>% mutate(flnm = x)
}
sdat2 <- fls2 %>% map_df(~ readRDSfl2(.))
sdat2 <- sdat2 %>% mutate(reduction_method = gsub(".*reduce|.RDS", "", flnm))
sdat <- fls %>%
  map_df(~ readRDSfl1(.)) %>%
  mutate(reduction_method = gsub(".*reduce|.RDS", "", flnm)) %>%
  select(-flnm) %>%
  inner_join(sdat2) %>%
  select(-flnm)

# Process the results
sdat <- sdat %>% mutate(
  AUPRCratio_1 = AUPRC_1 / cif_est_prob_1,
  AUPRCratio_5 = AUPRC_5 / cif_est_prob_5,
  AUPRCratio_10 = AUPRC_10 / cif_est_prob_10
)
CVdata_long <- sdat %>%
  pivot_longer(any_of(contains(c("BIC", "AUC", "BrierScore", "Slope", 
                                 "AUPRC", "cif"))),
    names_to = "metric"
  ) %>%
  mutate(metric = gsub("AUC_", "AUC", metric)) %>%
  mutate(metric = gsub("cif_est_prob", "cif.est.prob", metric)) %>%
  separate_wider_delim(metric, delim = "_", names = c("metric", "horiz"), 
                       too_few = "align_start") %>%
  mutate(Horiz = factor(paste0("H", horiz), levels = c("H1", "H5", "H10"))) %>%
  mutate(Fold = factor(fold))
table(CVdata_long$converged)
table(CVdata_long$num_covars_selected > 0)

# AVERAGED across folds:
CVdata_long_avg <- CVdata_long %>%
  filter(num_covars_selected > 0) %>%
  group_by(lambda, lindex, reduction_method, metric, horiz, Horiz) %>%
  filter(n() == 5) %>%
  summarise(mean_value = mean(value), sd_value = sd(value))

# Select the best lambda for fitting the model in the whole training dataset
# best_lambda
REDUCTION_METHOD <- reduction_method
CVdata <- sdat %>% filter(reduction_method == REDUCTION_METHOD)

# what would be the best lambda under different criteria?
lambda_by_criterion <- do.call(bind_rows, lapply(
  c(
    "AUC_1_1", "AUC_1_5", "AUC_1_10",
    "BrierScore_1", "BrierScore_5", "BrierScore_10",
    "Slope_1", "Slope_5", "Slope_10",
    "AUPRC_1", "AUPRC_5", "AUPRC_10" # ,
    # "AUPRCratio_1", "AUPRCratio_5", "AUPRCratio_10"
  ),
  function(criterion) {
    select_best_lambda(CVdata, criterion, min_improve_by = 0.002) %>%
      mutate(criterion)
  }
))
lambda_by_criterion %>%
  ungroup() %>%
  counts(criterion, lindex) %>%
  arrange(criterion, desc(n)) %>%
  print(n = Inf)

best_lambda <- lambda_by_criterion %>%
  filter(criterion == cv_criterion) %>%
  select(any_of(c("lindex", cv_criterion))) %>%
  left_join(CVdata %>% distinct(lindex, lambda)) %>%
  .$lambda
best_lambda

# Update subsave string to include the criterion
subsave <- paste0(
  subsave,
  ".reduce", reduction_method,
  ".CVcrit", cv_criterion
)
subsave

# logistic models
input_d <- z_train %>% 
  select(-c(PatientICN, Creat_BSP, EVERSMOKERUNKNOWN, Creatinine_Clearance_NA, 
            Low_Creatinine_Clearance))

if (grepl("LOGISTIC", reduction_method)) {
  tic("Fiting logistic models...")
  logmod_coefs <- do.call(rbind, lapply(colnames(input_d),
    logmod_coef,
    outcome = as.numeric(d_preserve$event_CR == 1),
    data = input_d
  )) %>%
    tibble::rownames_to_column("predictor")
  toc()
  if (reduction_method == "LOGISTICEFFECTSIZE") {
    # Reduce according to abs logmod coef >= 0.5
    keep_vars <- logmod_coefs %>%
      filter(abs(Estimate) >= 0.5) %>%
      .$predictor
  } else if (reduction_method == "LOGISTICPVAL") {
    keep_vars <- logmod_coefs %>%
      filter(P <= 0.05) %>%
      .$predictor
  } else if (reduction_method == "LOGISTIC2STEP") {
    keep_vars <- logmod_coefs %>%
      filter(P <= 0.05 & abs(Estimate) >= 0.1) %>%
      .$predictor
  }
  length(keep_vars)
  keep_vars
}

input_d <- input_d %>% select(all_of(keep_vars))
ncol(input_d)

# We cannot fit the fcrrp with just one lambda.  We need to add the optimal 
# lambda to the list we previously used, then extract it into a dummy model.
# Fit the main model.
tic("Fitting main penalized model")
cmpmod_penalized <- fastCrrp(
  formula = Crisk(truth_train$CR_tte, truth_train$event_CR, failcode = 1) ~ .,
  data = input_d,
  lambda = sort(unique(c(use_lambdas, best_lambda))),
  standardize = FALSE, 
  penalty = selection_method,
  getBreslowJumps = TRUE
)
toc()
rownames(cmpmod_penalized$coef) <- colnames(input_d)


# Use the above object to create a dummy fcrr object to be used for predictions
dummy_fcrr <- fcrr_from_fcrrp(
  fcrrp_model = cmpmod_penalized,
  truth = truth_train,
  input_data = input_d,
  lambda = best_lambda,
  replace_coefs = TRUE
)

dummy_fcrr$coef

# Save training results results to these file locations
train_dir_main <- paste0(results_dir_train, "main/")
dir.create(train_dir_main)
output_file_fcrrp <- paste0(train_dir_main, "3.y1.fcrrp.main.", subsave, ".RDS")
output_file_dummy <- paste0(train_dir_main, "4.y1.dummy.main.", subsave, ".RDS")
saveRDS(cmpmod_penalized, output_file_fcrrp)
saveRDS(dummy_fcrr, output_file_dummy)


print("End of Script")
