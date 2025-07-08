# Apply recalibration prior to evaluating external scores

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
subset <- args[10]
USE_Y1 <- as.logical(args[11])

# Run from parent directory
source("config_mnt.R")
source("functions.R")


if (landmark_year == 1 & !USE_Y1) {
  stop("Landmark year 1 must use the year 1 MCP model. 
       Later landmarks may also use it.")
}

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
library(riskRegression)

SEED <- 12345
set.seed(SEED)


subsave_Y1 <- paste(outcome, "r1", "imputed",
  paste0("mincorr", minCorr),
  paste0("SEED", SEED),
  selection_method,
  paste0("reduce", reduction_method),
  paste0("CVcrit", cv_criterion),
  sep = "."
)

subsave_Y1
subsave <- paste(subsave_Y1,
  paste0("L", landmark_year),
  fit_method,
  sep = "."
)
subsave

# Read in the test set data
subsave_datafile <- paste0(
  "data.postxform.landmark_y", landmark_year,
  ".test.", outcome, ".r1.imputed.mincorr", minCorr)
subsave_datafile_observed <- paste0(
  "landmark_y", landmark_year, ".test.", outcome, ".observed")
subsave_datafile_prexform <- paste0(
  "data.prexform.landmark_y", landmark_year, ".test.", outcome, 
  ".r1.imputed.mincorr", minCorr)
datafile <- paste0(
  preprocess_dir, "cohAll/", outcome, "/imputed/", subsave_datafile, ".RDS")
datafile_observed <- paste0(preprocess_dir, "cohAll/", outcome, "/observed/", 
                            subsave_datafile_observed, ".RDS")
datafile_prexform <- paste0(preprocess_dir, "cohAll/", outcome, "/imputed/", 
                            subsave_datafile_prexform, ".RDS")
file.exists(datafile)
file.exists(datafile_observed)
file.exists(datafile_prexform)

d_test <- readRDS(datafile)
d_test_raw <- readRDS(datafile_prexform) %>%
  select(PatientICN,
    Age_DmDx_raw = Age_DmDx, EGFR_raw = EGFR, SBP_raw = SBP,
    A1C_raw = A1C, TotChol_raw = TotChol, Creat_raw = Creat_BSP, 
    HDLC_raw = HDLC
  ) # no UACR here because it was not imputed
d_test_UACR_observed <- readRDS(datafile_observed) %>%
  select(PatientICN, UACR_raw = UACR)
d_test <- left_join(d_test, d_test_raw) %>%
  left_join(d_test_UACR_observed)
table(is.na(d_test$UACR_raw))
fivenum(d_test$UACR_raw)

RECODE_coefs <- c(
  age = -0.01938,
  GenderF = -0.01129,
  RaceBLACK = 0.08812,
  EthnicityHIS = 0.23380,
  SmokingFactorCURRENTSMOKER = 0.14830,
  SBP = 0.00303,
  CVDHIST = -0.02164,
  BPLOWERINGDRUG = -0.07952,
  ORALDM = -0.1256,
  Anticoagulants = 0.03199,
  A1C = 0.13690,
  TotChol = -0.00111,
  HDLC = 0.00629,
  Creat = 0.86090,
  UACR = 0.00036
)

# Add derived variables necessary for calculating RECODE score
d_test <- d_test %>% mutate(
  # Blood pressure lowering drugs
  BPLOWERINGDRUG = as.numeric(
    BetaBlockers == 1 | BetaB2nd == 1 | BetaB3rd == 1 | CChannelBlockers == 1 |
      ACEInhibitors == 1 | AlphaBlockers == 1 | AngiotensinIIRB == 1 |
      AntiHypertensiveComb == 1 | ThiazideDiuretics == 1 |
      PotassiumSparingDiuretics == 1 | PeriphVasodilators == 1 | 
      LoopDiuretics == 1 | Other_Antihypertensives == 1
  ),
  # History of cardiovascular disease
  CVDHIST = as.numeric(
    EH_HEARTFAILURE == 1 | EH_VALVDIS == 1 | EH_PERIVASC == 1 |
    CAD == 1 | MI == 1 | Stroke_infarct == 1),
  ORALDM = as.numeric(Thiazolidinedione == 1 |
    DPP4 == 1 | SGLT2 == 1 | Sulfonylurea == 1 |
    Metformin == 1 | Other_GlucoseLowering == 1)
)

# Imputed UACR
Mode(d_test$UACR_raw)
median(d_test$UACR_raw, na.rm = T)
median(d_test$UACR_raw[d_test$UACR_raw < 30], na.rm = T)
d_test$UACR_raw[is.na(d_test$UACR_raw)] <- 7.5

# Get Model coefs
train_dir_main <- paste0(results_dir_train, "main/")

if (USE_Y1) {
  dummy_fcrr <- readRDS(paste0(train_dir_main, "4.y1.dummy.main.", 
                               subsave_Y1, ".RDS"))
} else if (landmark_year != 1) {
  dummy_fcrr <- paste0(train_dir_main, "11.RIDGE.unpenalized.main.", 
                       subsave, ".RDS") %>% readRDS(.)
}

KDI_coefs <- coef(dummy_fcrr)
KDI_coefs

# Calculate the KDI score
KDIScore <- d_test %>%
  mutate(
    GenderF = as.numeric(Gender == "F"),
    RaceBLACK = as.numeric(Race == "BLACK"),
    SmokingFactorUNKNOWN = as.numeric(SmokingFactor == "UNKNOWN")
  ) %>%
  select(PatientICN, all_of(names(KDI_coefs))) %>%
  mutate_if(is.factor, fac_numeric) %>%
  pivot_longer(!PatientICN, names_to = "variable") %>%
  left_join(data.frame(variable = names(KDI_coefs), coef = KDI_coefs)) %>%
  mutate(score_sub = value * coef) %>%
  group_by(PatientICN) %>%
  summarise(KDIScore = sum(score_sub))

# Calculate the RECODe score
setdiff(names(RECODE_coefs), names(d_test))

recodeScore_sub <- d_test %>%
  mutate(
    age = Age_DmDx_raw + landmark_year,
    SBP = SBP_raw,
    A1C = A1C_raw,
    TotChol = TotChol_raw,
    HDLC = HDLC_raw,
    Creat = Creat_raw,
    UACR = UACR_raw,
    GenderF = as.numeric(Gender == "F"),
    RaceBLACK = as.numeric(Race == "BLACK"),
    EthnicityHIS = as.numeric(Ethnicity == "HIS"),
    SmokingFactorCURRENTSMOKER = as.numeric(SmokingFactor == "CURRENTSMOKER")
  ) %>%
  select(PatientICN, all_of(names(RECODE_coefs))) %>%
  mutate_if(is.factor, fac_numeric) %>%
  pivot_longer(!PatientICN, names_to = "variable") %>%
  left_join(data.frame(variable = names(RECODE_coefs), coef = RECODE_coefs)) %>%
  mutate(score_sub = value * coef)

recodeScore_sub %>%
  group_by(variable) %>%
  summarise(mas = mean(abs(score_sub))) %>%
  arrange(desc(mas))

recodeScore <- recodeScore_sub %>%
  group_by(PatientICN) %>%
  summarise(RECODe_riskscore = sum(score_sub))

# Add the calculated scores to the data
d_test <- left_join(d_test, KDIScore) %>%
  left_join(recodeScore)

# Recalibrate
htimes <- c(1, 5, 10)
predprobs <- predict_prob_calibrated_allH(
  ftime = d_test$CR_tte,
  fstatus = d_test$event_CR,
  horizons = htimes,
  betax = d_test$KDIScore
)
names(predprobs) <- paste0(names(predprobs), ".KDI")
predprobs %>% apply(., 2, mean)

# For RECODE score - with competing risks
fivenum(d_test$RECODe_riskscore)
mean(d_test$RECODe_riskscore)
sd(d_test$RECODe_riskscore)

predprobs_RECODE <- predict_prob_calibrated_allH(
  ftime = d_test$CR_tte,
  fstatus = d_test$event_CR,
  horizons = htimes,
  betax = d_test$RECODe_riskscore
)
names(predprobs_RECODE) <- paste0(names(predprobs_RECODE), ".RECODe")
predprobs_RECODE %>% apply(., 2, mean)


# Predprobs for RECODE when ignoring competing risk
tic("Generating Predprobs for RECODE when ignoring competing risk")
predprobs_RECODE_nocmprsk_obj <- predict_prob_calibrated_cox(
  ftime = d_test$CR_tte,
  fstatus = d_test$event_CR,
  horizons = htimes,
  betax = d_test$RECODe_riskscore,
  getScoreObj = F
)
toc()
predprobs_RECODE_nocmprsk <- predprobs_RECODE_nocmprsk_obj$pred
names(predprobs_RECODE_nocmprsk) <- paste0(
  names(predprobs_RECODE_nocmprsk), ".RECODE_nocmprsk")
predprobs_RECODE_nocmprsk %>% apply(., 2, mean)

# Add the predicted probabilities to the dataset before subsetting
d_test <- cbind(d_test, predprobs, predprobs_RECODE, predprobs_RECODE_nocmprsk)
names(d_test)

# Subset the data
d_subset <- subset_data(d_test, subset)

# Glimpse the predicted probabilities
predprobs <- d_subset %>%
  select(contains("pred")) %>%
  select(contains("KDI"))
head(predprobs)
predprobs_RECODE <- d_subset %>%
  select(contains("pred")) %>%
  select(contains("RECODe", ignore.case = F))
head(predprobs_RECODE)
predprobs_RECODE_nocmprsk <- d_subset %>%
  select(contains("pred")) %>%
  select(contains("RECODE_nocmprsk"))
head(predprobs_RECODE_nocmprsk)

# Outcome
newdata_truth <- d_subset %>% select(PatientICN, event_CR, CR_tte, censtime)

# Event rate
table(newdata_truth$event_CR)

## TTE for cases
newdata_truth %>%
  filter(event_CR == 1) %>%
  .$CR_tte %>%
  fivenum(.)

n_bootstrap <- 500

# Evaluate model - KDI 
e.l <- data.frame("horiz" = 0) %>% filter(is.na(horiz))
for (i in 1:length(htimes)) {
  tictoc::tic(paste0("Evaluating metrics at horizon ", htimes[i]))
  e.i <- CR_auc_simple(newdata_truth$CR_tte, newdata_truth$event_CR,
                      marker = predprobs[[i]], time = htimes[i]) %>% 
    cbind(CR_calib_simple(predprobs[[i]], newdata_truth, htimes[i])) %>% 
    mutate(horiz = htimes[i])  %>%
    cbind(  (CR_get_AUPRC(predprobs[[i]], newdata_truth, htimes[i], 
                          cause =1)$AUPRC   )) 
  if (n_bootstrap > 0) {
    e.i <- e.i %>%
      cbind((CR_var_bootstrap(newdata_truth, predprobs[[i]],
        cause = 1, htime = htimes[i],
        n_bootstrap = n_bootstrap, AUC = FALSE, AUPRC = FALSE
      )$summary))
  }

  e.i <- e.i %>%
    select(-horiz) %>%
    mutate(horiz = htimes[i])
  e.l <- full_join_quiet(e.l, e.i)

  tictoc::toc()
}

e.l <- e.l %>% mutate(landmark_year,
  covar_year = landmark_year,
  model_year = ifelse(USE_Y1, 1, landmark_year),
  subset, model_type = "KDI"
)
e.l

subsave_out <- paste0(
  subsave,
  ifelse(UNPENALIZED, ".unpenalized.", ".penalized."),
  subset,
  ifelse(USE_Y1 & landmark_year != 1, ".fromY1MCP", "")
)
output_file <- paste0(
  results_dir_test, "evaluations/KDI_RECALIBRATE_eval_subset_", 
  subsave_out, ".RDS")
saveRDS(e.l, output_file)

# Calibration quantiles
calib_deciles <- man_cal_quantiles(d_subset$pred1.KDI, 
                                   newdata_truth, htime = 1) %>%
  bind_rows(man_cal_quantiles(d_subset$pred5.KDI, 
                              newdata_truth, htime = 5)) %>%
  bind_rows(man_cal_quantiles(d_subset$pred10.KDI, 
                              newdata_truth, htime = 10)) %>%
  mutate(landmark_year,
    covar_year = landmark_year, subset, model_type = "KDI",
    model_year = ifelse(USE_Y1, 1, landmark_year)
  )
dir.create(paste0(results_dir_test, "evaluations/"))


# evaluate RECODE (if use_Y1 = T, otherwise redundant)
if (USE_Y1) {
  RECODE.l <- data.frame("horiz" = 0) %>% filter(is.na(horiz))
  for (i in 1:length(htimes)) {
    tictoc::tic(paste0("Evaluating metrics for RECODE at horizon ", htimes[i]))
    RECODE.i <- CR_auc_simple(newdata_truth$CR_tte, newdata_truth$event_CR,
      marker = predprobs_RECODE[[i]], time = htimes[i]) %>%
      cbind(CR_calib_simple(predprobs_RECODE[[i]], newdata_truth, htimes[i])) %>% 
      mutate(horiz = htimes[i]) %>%
      cbind(  (CR_get_AUPRC(predprobs_RECODE[[i]], newdata_truth, htimes[i], 
                            cause =1)$AUPRC   ))

    if (n_bootstrap > 0) {
      RECODE.i <- RECODE.i %>%
        cbind((CR_var_bootstrap(newdata_truth, predprobs_RECODE[[i]],
          cause = 1, htime = htimes[i],
          n_bootstrap = n_bootstrap, calib = TRUE, AUC = FALSE, AUPRC = FALSE
        )$summary))
    }
    RECODE.i <- RECODE.i %>%
      select(-horiz) %>%
      mutate(horiz = htimes[i])
    RECODE.l <- full_join_quiet(RECODE.l, RECODE.i)

    tictoc::toc()
  }

  RECODE.l <- RECODE.l %>% mutate(landmark_year,
    covar_year = landmark_year,
    model_year = NA,
    subset, model_type = "RECODe"
  )
  RECODE.l

  # evaluate RECODE WHEN IGNORING COMPETING RISKS (if use_Y1 = F )
  RECODE_nocmprsk.l <- data.frame("horiz" = 0) %>% filter(is.na(horiz))
  for (i in 1:length(htimes)) {
    tictoc::tic(paste0(
      "Evaluating metrics for RECODE (no CRR) at horizon ", htimes[i]))
    RECODE_nocmprsk.i <- CR_auc_simple(newdata_truth$CR_tte, 
                                       newdata_truth$event_CR,
      marker = predprobs_RECODE[[i]], time = htimes[i]) %>%
      cbind(CR_calib_simple_nocmprsk(predprobs_RECODE_nocmprsk[[i]], 
                                     newdata_truth, htimes[i])) %>% 
      mutate(horiz = htimes[i]) %>%
      cbind(  (CR_get_AUPRC(predprobs_RECODE[[i]], newdata_truth, htimes[i], 
                            cause =1)$AUPRC   ))

    if (n_bootstrap > 0) {
      RECODE_nocmprsk.i <- RECODE_nocmprsk.i %>%
        cbind((CR_var_bootstrap(newdata_truth, predprobs_RECODE_nocmprsk[[i]],
          cause = 1, htime = htimes[i],
          n_bootstrap = n_bootstrap, calib = TRUE, AUC = FALSE, AUPRC = FALSE,
          calib_nocmprsk = TRUE
        )$summary))
    }
    RECODE_nocmprsk.i <- RECODE_nocmprsk.i %>%
      select(-horiz) %>%
      mutate(horiz = htimes[i])
    RECODE_nocmprsk.l <- full_join_quiet(RECODE_nocmprsk.l, RECODE_nocmprsk.i)

    tictoc::toc()
  }

  RECODE_nocmprsk.l <- RECODE_nocmprsk.l %>% mutate(landmark_year,
    covar_year = landmark_year,
    model_year = NA,
    subset, model_type = "RECODe_nocmprsk"
  )
  RECODE_nocmprsk.l

  calib_deciles_RECODE <- man_cal_quantiles(d_subset$pred1.RECODe, 
                                            newdata_truth, htime = 1) %>%
    bind_rows(man_cal_quantiles(d_subset$pred5.RECODe, 
                                newdata_truth, htime = 5)) %>%
    bind_rows(man_cal_quantiles(d_subset$pred10.RECODe, 
                                newdata_truth, htime = 10)) %>%
    mutate(landmark_year,
      covar_year = landmark_year, subset, model_type = "RECODe",
      model_year = ifelse(USE_Y1, 1, landmark_year)
    )

  calib_deciles_RECODE_nocmprsk <- man_cal_quantiles(
    d_subset$pred1.RECODE_nocmprsk,
    newdata_truth,
    htime = 1, nocmprsk = TRUE
  ) %>%
    bind_rows(man_cal_quantiles(d_subset$pred5.RECODE_nocmprsk, 
                                newdata_truth, htime = 5, nocmprsk = TRUE)) %>%
    bind_rows(man_cal_quantiles(d_subset$pred10.RECODE_nocmprsk, 
                                newdata_truth, htime = 10, nocmprsk = TRUE)) %>%
    mutate(landmark_year,
      covar_year = landmark_year, subset, model_type = "RECODe_nocmprsk",
      model_year = ifelse(USE_Y1, 1, landmark_year)
    )

  recode.l_out <- bind_rows(RECODE.l, RECODE_nocmprsk.l)

  output_file_RECODE <- paste0(
    results_dir_test, "evaluations/RECODE_RECALIBRATE_eval_subset_",
    gsub(".fromY1MCP", "", subsave_out), ".RDS"
  )
  saveRDS(recode.l_out, output_file_RECODE)

  calib_deciles <- calib_deciles %>%
    bind_rows(calib_deciles_RECODE) %>%
    bind_rows(calib_deciles_RECODE_nocmprsk)
}

# view summary
calib_deciles %>% distinct(landmark_year, htime, model_type, Slope, BrierScore)

output_file_calib <- paste0(
  results_dir_test, "evaluations/RECALIBRATE_calib_deciles_subset_", 
  subsave_out, ".RDS")
saveRDS(calib_deciles, output_file_calib)


print("End of Script")
