# Perform 5-fold cross-validation to choose the lambda for L1 MCP
# Also apply a prior step of variable selection
# Fit the models and save, but do not evaluate them
# Each fold/lambda will be evaluated in a separate parallel job


args <- commandArgs(trailingOnly = T)
CVFOLD <- as.numeric(args[1])
selection_method <- args[2]
outcome <- args[3]
minCorr <- as.numeric(args[4])
reduction_method <- args[5]

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
d <- readRDS(paste0(read_dir, "data.postxform.landmark_y1.train.", outcome, 
                    ".r1.imputed.mincorr", minCorr, ".RDS"))


# separate by predictors and (outcomes+ other non-predictors)
d_preserve <- d %>%
  select(PatientICN, event01:Elix_RawScore) %>%
  select(-Elix_excl_RF)
d_pred <- d %>% select(-any_of(names(d_preserve)[-1]))
rm(d)

# While the fastCrrp function can handle multi-level factors, creating dummy 
# variables in the process, the resulting object does not store the variable 
# names anywhere and they are not recoverable.  Therefore, it is better to do 
# the dummy # coding ourselves so we know which coefficients correspond to 
# which variables or factor levels.
d_pred <- model.matrix(~., d_pred)[, -1] %>% data.frame()
names(d_pred) <- gsub("1$", "", names(d_pred))

# List all the available covariates
covars_all <- names(d_pred)[-1]
covars_all

input_d <- d_pred[-1] %>%
  # Use eGFR rather than creatinine.
  select(-c(Creat_BSP, EVERSMOKERUNKNOWN)) 

if (reduction_method == "CORR") {
  # Reduce the input matrix by taking only variable with correlation greater 
  # than X with outcome
  corr_vals <- sapply(input_d, cor, y = as.numeric(d_preserve$event_CR == 1), 
                      use = "pairwise.complete.obs")
  # hist(corr_vals, nclass = 100)
  mean(corr_vals)
  fivenum(corr_vals)
  length(corr_vals)
  sum(abs(corr_vals) > 0.01)
  keep_vars <- names(corr_vals[abs(corr_vals) >= 0.01])
} else if (grepl("LOGISTIC", reduction_method)) {
  # Or using a logistic regression with age, sex and race
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

# Covariate matrix for training set. Drop invariant columns, if any
z_train <- d_pred %>%
  select(all_of(keep_vars)) %>%
  select_if(~ !(n_distinct(.) == 1))
truth_train <- d_preserve %>% 
  dplyr::select(PatientICN, event_CR, CR_tte, censtime)


# Choose lambdas to try for cross-validation.  The ones automatically chosen 
# by the program aren't good.
# min lambda gets too low with sample sizes > 100k
lmin <- 1E-5
lmax <- 0.01
nlambda <- 20
use_lambdas <- signif(10^(seq(log10(lmin), log10(lmax), len = nlambda)), 4)
use_lambdas

# Cross validation to choose optimal lambda
fold.n <- 5
set.seed(SEED)
fold.i <- sample(1:fold.n, nrow(z_train), replace = T)

# Create a directory for CV fold, and save PatientIDs there
rsplit_dir <- paste0(results_dir_train, "CVfold", CVFOLD, "/")
dir.create(rsplit_dir, recursive = TRUE)

save(
  list = c(
    "z_train", "truth_train", "d_preserve", "fold.i",
    "use_lambdas", "covars_all"
  ),
  file = paste0(rsplit_dir, "1.y1.data.", subsave, ".RData")
)

# # Run cross-validation steps on this fold, not yet getting the summary of
# performance and calibration for different penalty values (This will be done
# in parallel in separate script)
tic(paste0("Time to fit penalized model for CV test fold ", CVFOLD))
fold_summary <- fastCrrpCV(
  z = z_train %>% select(-any_of(c(
    "PatientICN", "Creat_BSP", "EVERSMOKERUNKNOWN",
    "Creatinine_Clearance_NA", "Low_Creatinine_Clearance"
  ))),
  truth = truth_train,
  fold.i = fold.i, fold = CVFOLD,
  penalization_method = selection_method,
  lambdas = use_lambdas,
  htimes = c(1, 5, 10),
  evaluate = FALSE
)
toc()

saveRDS(fold_summary, paste0(rsplit_dir, "2.y1.cv.MODELONLY.", subsave, ".RDS"))

print("End of script")
