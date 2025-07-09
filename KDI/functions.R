# Functions to use throughout pipeline

# for repeated joins, may want to make them quiet (use with caution)
full_join_quiet <- function(...) {
  suppressMessages(full_join(...))
}
inner_join_quiet <- function(...) {
  suppressMessages(inner_join(...))
}
left_join_quiet <- function(...) {
  suppressMessages(left_join(...))
}
right_join_quiet <- function(...) {
  suppressMessages(right_join(...))
}
anti_join_quiet <- function(...) {
  suppressMessages(anti_join(...))
}

# Mode
Mode <- function(x, na.rm = T) {
  if (all(is.na(x))) {
    return(as(NA, class(x)))
  }
  if (length(x) == 0) {
    return(as(NA, class(x)))
  }
  ux <- sort(unique(x), na.last = TRUE)
  if (na.rm) {
    ux <- na.omit(ux)
  }
  rt <- ux[which.max(tabulate(match(x, ux)))]
  return(rt)
}

fac_numeric <- function(x) {
  as.numeric(as.character(x))
}

back_to_date <- function(x) {
  # dates were expressed in years since 1/1/2000
  # they were converted using as.numeric(julian(DeathDateTime,origin=as.Date("2000-01-01")))/365.25]
  # this function is to reverse the above back into the original datetime
  as.POSIXct(x * 365.25 * 24 * 60 * 60, origin = "2000-01-01")
}

just_date <- function(x) {
  # from the converted values, convert back to date, strip time-of-day info, then use
  # lubridate::decimal_date to scale back to 2000-based representation
  original_datetime <- back_to_date(x)
  original_date <- as.Date(original_datetime)
  new_date_numeric <- decimal_date(original_date) - 2000
  return(new_date_numeric)
}

# Function equivalent to dplyr::count with sort=T as default
counts <- function(data, ...) {
  dplyr::count(data, ..., sort = T)
}




## Function to extract data from a table while documenting the extraction details
extract.detail.icd <- function(obj, feature_name, pattern, field_name = "ICDCode") {
  # save original pattern - icd cdoes will be separated by icd10 and icd9
  pattern_og <- pattern
  domain <- "Conditions"
  if (length(pattern) != 1) {
    stop("Provide exactly 1 patterns for ICDCode: ICD9 vs ICD10 will be detected")
  }
  pattern_separated <- base::strsplit(pattern_og, "|", fixed = T)[[1]]
  # ICD code patterns should always start with "^" (start of string)
  if (any(substr(pattern_separated, 0, 1) != "^")) {
    stop(paste0("You forgot to use '^' with the ICD patterns for ", feature_name))
  }
  pattern_separated <- gsub("\\^", "", pattern_separated)
  pattern_icd9 <- pattern_separated[substr(pattern_separated, 0, 1) %in% c("e", "v", as.character(0:9))]
  pattern_icd10 <- setdiff(pattern_separated, pattern_icd9)
  pattern_icd9 <- paste0("^", pattern_icd9, collapse = "|")
  pattern_icd10 <- paste0("^", pattern_icd10, collapse = "|")
  pattern <- c(pattern_icd9, pattern_icd10)
  field_name <- c("ICD9Code", "ICD10Code")
  pattern_export <- data.frame(feature_name, domain, pattern, field_name)

  # Push the extraction details to the global object
  feature_name_current <- feature_name
  extraction_details_export <<- extraction_details_export %>%
    filter(feature_name != feature_name_current) %>% # remove previous info, if present
    full_join_quiet(pattern_export) %>%
    distinct()
  # Perform the extraction
  output_full <- obj[grepl(pattern_og, ICDCode), ] %>%
    distinct()

  # If cohort_n is not yet defined (e.g. when extracting DM records)
  if (!exists("cohort_n")) {
    cohort_n <- NA
  }

  # Summarize the results
  output_summary <- output_full %>%
    group_by(ICDCode) %>%
    summarise(
      n_records = n(),
      n_patients = length(unique(PatientICN)),
      n_OutpatDx = sum(Source == "OutpatDx"),
      n_InpatDx = sum(Source == "InpatDx"),
      .groups = "drop"
    ) %>%
    left_join_quiet(ICDdict) %>%
    mutate(feature_name, domain) %>%
    dplyr::rename(code = ICDCode) %>%
    mutate(cohort, cohort_n) %>%
    arrange(desc(n_patients))
  # Push the extraction results to the global object
  extraction_results_export <<- extraction_results_export %>%
    filter(feature_name != feature_name_current) %>%
    full_join_quiet(output_summary)
  # Return the raw results
  return(output_full)
}

extract.detail.proc <- function(feature_name, pattern, field_name = c("CPTCode", "ICD9ProcedureCode", "ICD10ProcedureCode"), exclude = F, exact_match = F) {
  domain <- "Procedures"
  # Push the extraction details to the global object
  feature_name_current <- feature_name
  pattern_export <- data.frame(feature_name, domain, pattern, field_name)
  extraction_details_export <<- extraction_details_export %>%
    filter(feature_name != feature_name_current) %>% # remove previous info, if present
    full_join_quiet(pattern_export) %>%
    distinct()
  # Perform the extraction
  output_CPT <- CPT[grepl(pattern_export$pattern[pattern_export$field_name == "CPTCode"], CPTCode), ] %>%
    left_join_quiet(CPTDim) %>%
    mutate(code_vocabulary = "CPT") %>%
    select(-CPTName)
  output_ICD9Proc <- proc9[grepl(pattern_export$pattern[pattern_export$field_name == "ICD9ProcedureCode"], ICD9ProcedureCode), ] %>%
    left_join_quiet(ICD9ProcDim) %>%
    mutate(code_vocabulary = "CPT")
  output_ICD10Proc <- proc10[grepl(pattern_export$pattern[pattern_export$field_name == "ICD10ProcedureCode"], ICD10ProcedureCode), ] %>%
    left_join_quiet(ICD10ProcDim) %>%
    mutate(code_vocabulary = "CPT")
  names(output_CPT)[1:5] <- c("PatientICN", "dt", "code", "Source", "Description")
  names(output_ICD9Proc)[1:5] <- c("PatientICN", "dt", "code", "Source", "Description")
  names(output_ICD10Proc)[1:5] <- c("PatientICN", "dt", "code", "Source", "Description")
  output_full <- rbind(output_CPT, output_ICD10Proc, output_ICD9Proc)

  # Summarize the results
  output_summary <- output_full %>%
    group_by(code, Description, code_vocabulary) %>%
    summarise(
      n_records = n(),
      n_patients = length(unique(PatientICN)),
      n_OutpatProc = sum(Source == "OutpatProc"),
      n_InpatProc = sum(Source == "InpatProc"),
      n_FeeProc = sum(Source == "FeeProc"),
      .groups = "drop"
    ) %>%
    # left_join(ProcDict %>% dplyr::rename(Description = ProcDescription)) %>%
    mutate(feature_name, domain) %>%
    # dplyr::rename(code = ProcCode) %>%
    arrange(desc(n_patients)) %>%
    mutate(cohort, cohort_n)
  # Push the extraction results to the global object
  extraction_results_export <<- extraction_results_export %>%
    filter(feature_name != feature_name_current) %>%
    full_join_quiet(output_summary)
  # Return the raw results
  return(output_full)
}

extract.detail.Rx <- function(obj, feature_name, pattern, field_name, exclude = F, exact_match = F) {
  if (any(!(field_name %in% c("VAClassification", "LocalDrugNameWithDose", "DrugNameWithDose", "DrugNameWithoutDose")))) {
    stop("Not among the supported variable names for medications")
  }
  domain <- "Medications"
  pattern_export <- data.frame(feature_name, domain, pattern, field_name, exclude, exact_match)
  # Push the extraction details to the global object
  feature_name_current <- feature_name
  extraction_details_export <<- extraction_details_export %>%
    filter(feature_name != feature_name_current) %>% # remove previous info, if present
    full_join_quiet(pattern_export) %>%
    distinct()

  # Perform the extraction
  mdtmp <- obj %>% filter(is.na(PatientICN))
  mdtmp_excl <- mdtmp
  for (i in 1:length(pattern)) {
    if (pattern_export$exact_match[i] == F) {
      mdtmp.i <- obj[grepl(pattern[i], obj[, get(pattern_export$field_name[i])])] %>% distinct()
    } else if (pattern_export$exact_match[i] == T) {
      mdtmp.i <- obj[obj[, get(pattern_export$field_name[i]) == pattern[i]], ] %>% distinct()
    }
    if (pattern_export$exclude[i]) {
      mdtmp_excl.i <- mdtmp.i
      rm(mdtmp.i)
      mdtmp_excl <- full_join_quiet(mdtmp_excl, mdtmp_excl.i) %>% distinct()
    } else {
      mdtmp <- full_join_quiet(mdtmp, mdtmp.i) %>% distinct()
    }
  }
  output_full <- anti_join_quiet(mdtmp, mdtmp_excl) %>% distinct()

  # If cohort_n is not yet defined (e.g. when extracting glucose-lowering records)
  if (!exists("cohort_n")) {
    cohort_n <- NA
  }

  # Summarize the results
  output_summary <- output_full %>%
    mutate(drug_name = coalesce(DrugNameWithoutDose, DrugNameWithDose, LocalDrugNameWithDose)) %>%
    group_by(VAClassification, drug_name) %>%
    summarise(
      n_records = n(),
      n_patients = length(unique(PatientICN)),
      n_Outpat = sum(Source == "Outpat"),
      n_Fee = sum(Source == "Fee"),
      n_IV = sum(Source == "IV"),
      n_BCMA = sum(Source == "BCMA"),
      .groups = "drop"
    ) %>%
    mutate(feature_name, domain) %>%
    mutate(cohort, cohort_n) %>%
    arrange(desc(n_patients))

  # Push the extraction results to the global object
  extraction_results_export <<- extraction_results_export %>%
    filter(feature_name != feature_name_current) %>%
    full_join_quiet(output_summary)

  # Return the raw results
  return(output_full)
}

## Define function for deduplicating labs or vitals (and taking median per day)
## while logging summary-level info
summlab <- function(data, lower, upper, name = "value", source = NA, domain = "Labs") {
  # Only unique values for each person-date
  cln <- data %>%
    distinct(PatientICN, dt, value, .keep_all = T) %>%
    filter(value >= lower & value <= upper)

  # If labs domain, keep/summarize LOINC, Topography, and Consensus info
  if (domain == "Labs") {
    rt <- cln[, .(
      median_val = median(value),
      count.dt = .N,
      LOINC = Mode(LOINC),
      LabChemTestName = Mode(LabChemTestName),
      Topography = Mode(Topography),
      Consensus = Mode(Consensus)
    ), by = .(PatientICN, dt)] %>%
      filter(count.dt <= 10) %>%
      dplyr::rename_with(function(x) {
        name
      }, .cols = median_val) %>%
      select(-count.dt) %>%
      distinct() %>%
      arrange(PatientICN, dt)
    out_summary <- rt %>%
      # Add the grouping variable for later summary
      group_by(LabChemTestName) # , LOINC, Consensus)

    # If Vitals, just summarize by person-date
  } else if (domain == "Vitals") {
    rt <- cln[, .(
      median_val = median(value),
      count.dt = .N
    ),
    by = .(PatientICN, dt)
    ] %>%
      filter(count.dt <= 10) %>%
      dplyr::rename_with(function(x) {
        name
      }, .cols = median_val) %>%
      select(-count.dt) %>%
      arrange(PatientICN, dt) %>%
      distinct()
    out_summary <- rt # No grouping here
  }

  # If cohort_n is not yet defined (e.g. when extracting LDLC, A1C, Serum Creat)
  if (!exists("cohort_n")) {
    cohort_n <- NA
  }

  # Summarize the results to export
  out_summary <- out_summary %>%
    summarise(
      n_records = n(),
      n_patients = length(unique(PatientICN)),
      .groups = "drop"
    ) %>%
    mutate(
      marker_name = name, feature_name = paste0(name, "_continuous"),
      lower, upper, domain, source
    ) %>%
    arrange(desc(n_patients)) %>%
    mutate(cohort, cohort_n)

  # If there are no results, log as a row with 0 records/patients
  if (nrow(out_summary) == 0) {
    out_summary <- data.frame(
      marker_name = name, n_records = 0, n_patients = 0,
      cohort_n, feature_name = paste0(name, "_continuous"),
      lower, upper, domain, source
    )
  }

  # Add the the summary-level data to the file to export
  labs_export_1 <<- labs_export_1 %>%
    filter(marker_name != name) %>%
    full_join_quiet(out_summary)

  # Return the patient-level info to the session
  return(rt)
}

# Infer the upper and lower bounds of biomarker categories by their names
limits_from_name <- function(name) {
  strs <- strsplit(name, "_")[[1]]
  suppressWarnings(numeric_cutoffs <- as.numeric(strs))
  numeric_cutoffs <- numeric_cutoffs[!is.na(numeric_cutoffs)]

  if (length(numeric_cutoffs) == 2) {
    return(sort(numeric_cutoffs))
  } else if (length(numeric_cutoffs) == 1) {
    if (any(strs %in% c("gt", "ge"))) {
      return(c(numeric_cutoffs, NA))
    } else if (any(strs %in% c("lt"))) {
      return(c(NA, numeric_cutoffs))
    } else {
      stop("Cannot infer if upper or lower bound")
    }
  } else if (length(numeric_cutoffs) == 0) {
    return(c(NA, NA))
  } else {
    stop("Must not be more than 2 numbers in the name")
  }
}

# Summarize each of the biomarker categories to log
summlab2 <- function(data, domain = "Labs") {
  current_lab <- names(data)[3]
  data_core <- data %>% select(-any_of(c(
    "BirthDate", "DmDx_first", "yrs", "age", "Gender", "Topography",
    "LabChemTestName", "Sta3n", "LOINC", "Consensus", "LabChemTestSID",
    "ShortName", "class", "subclass", "LabPanelSID", "LabChemResultValue"
  )))
  # If cohort_n is not yet defined (e.g. when extracting glucose-lowering records)
  if (!exists("cohort_n")) {
    cohort_n <- NA
  }

  feature_names <- names(data_core)[4:ncol(data_core)]
  for (f in feature_names) {
    d <- data[(data[, ..f] == 1)[, 1]]
    out_summary <- data.frame(
      marker_name = current_lab,
      feature_name = f,
      n_records = nrow(d),
      n_patients = length(unique(d$PatientICN)),
      lower = limits_from_name(f)[1],
      upper = limits_from_name(f)[2]
    ) %>%
      mutate(cohort, cohort_n, domain)
    labs_export_2 <<- labs_export_2 %>%
      filter(feature_name != f) %>%
      full_join_quiet(out_summary)
  }
  ref_cat_d <- data %>%
    filter(if_all(all_of(feature_names), ~ . == 0))
  ref_na_out_summary <- data.frame(
    marker_name = current_lab,
    feature_name = c(paste0(current_lab, "_Ref"), paste0(current_lab, "_NA")),
    n_records = c(nrow(ref_cat_d), 0),
    n_patients = c(length(unique(ref_cat_d$PatientICN)), cohort_n - npat(data))
  ) %>%
    mutate(cohort, cohort_n, domain)
  labs_export_2 <<- labs_export_2 %>%
    filter(!(feature_name %in% unique(ref_na_out_summary$feature_name))) %>%
    full_join_quiet(ref_na_out_summary)
  return(data)
}


# Quick function to get the number of patients
npat <- function(data) {
  return(length(unique(data$PatientICN)))
}

# Print the number of rows AND patients
npr <- function(data) {
  print(paste0(nrow(data), " rows, ", length(unique(data$PatientICN)), " patients"))
}

# Add context for time-stamped objects: years since t0, age
td_context <- function(obj) {
  rt <- obj %>%
    left_join_quiet(DMcohort %>% select(PatientICN, BirthDate, DmDx_first)) %>%
    mutate(
      yrs = dt - DmDx_first,
      age = dt - BirthDate
    ) %>%
    select(-c(BirthDate, DmDx_first))
}

days_between <- function(t1, t2) {
  d1 <- date_decimal(t1 + 2000)
  d2 <- date_decimal(t2 + 2000)
  days_elapsed <- time_length(interval(d1, d2), "days")
  return(days_elapsed)
}


standardize <- function(x) {
  (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
}

landmark <- function(data, t) {
  # Get most recent data at time (tstart)
  data %>%
    filter(tstart <= t) %>%
    group_by(PatientICN) %>%
    slice_max(tstart) %>%
    ungroup()
}

# #Create an alternative to cal_plot_breaks from tidy/probably, because it doesn't handle pseudovalues correctly.
# #plots look more or less the same despite very different brier scores because of pseudovlaues. Falsely appears as underprediction.
man_cal_quantiles <- function(pred, truth, htime, conf_level = 0.95, ntile = 10, nocmprsk = F) {
  if (nocmprsk) {
    truth <- truth %>% mutate(event_CR = as.numeric(event_CR == 1)) # convert to 0/1 if not already
  }

  if (max(truth$CR_tte) >= htime) {
    # USE PSEUDOVALUES
    f <- prodlim(Hist(CR_tte, event_CR) ~ 1, data = truth)
    pseudovalues <- jackknife(f, times = htime, cause = 1) %>% data.frame()
    names(pseudovalues) <- "pseudo.t"
    #
    if (nocmprsk) {
      pseudovalues$pseudo.t <- 1 - pseudovalues$pseudo.t # surv given in absence of competing risks
    }

    brierScore <- mean((pseudovalues$pseudo.t - pred)^2)
    tmp <- data.frame("pseudo.t" = pseudovalues$pseudo.t, "pred" = pred)
    lmod <- lm(formula = I(pseudo.t) ~ 0 + pred, data = tmp)
    slp <- signif(lmod$coefficients[[1]], 3)
    slpSE <- summary(lmod)$coefficients[1, 2]
    binned <- tmp %>%
      mutate(pred_grp = ntile(pred, ntile)) %>%
      group_by(pred_grp) %>%
      dplyr::summarize(
        event_rt = mean(pseudo.t),
        # bin_midpt = median(pred),
        bin_meanpt = mean(pred),
        total = n(), events = sum(pseudo.t)
      ) %>%
      ungroup() %>%
      filter(events >= 0) %>% # deal with rare issue with pseudovalues in very small bins - remove entire point
      filter(events <= total) %>% # deal with rare issue with pseudoevents > total (e.g. 1.06 events out of 1 total)
      rowwise() %>%
      mutate(
        lower = prop.test(events, total, conf.level = conf_level)$conf.int[[1]],
        upper = prop.test(events, total, conf.level = conf_level)$conf.int[[2]]
      ) %>%
      ungroup()
    return(binned %>% mutate(htime, Slope = slp, SlopeSE = slpSE, BrierScore = brierScore))
  } else {
    warning("The requested time is later than exists in the data: returning NA")
    return(data.frame(htime))
  }
}

# Turn numeric indicators into factor
factorize.i <- function(x, ignore.failed = FALSE) {
  if (identical(sort(unique(x)), c(0, 1)) | identical(sort(unique(x)), c(0)) | identical(sort(unique(x)), c(1))) {
    x <- factor(x, levels = c(0, 1))
  } else if (ignore.failed) {
    x <- x # do nothing, helpful when converting many variables at once
  } else {
    stop("Error - must be a binary/indicator variable with no missing values")
  }
  return(x)
}

# Summary statistics rounded as character
mymedian <- function(x) {
  as.character(round(quantile(x, 0.5, na.rm = T), 1))
}
q25 <- function(x) {
  as.character(round(quantile(x, 0.25, na.rm = T), 1))
}
q75 <- function(x) {
  as.character(round(quantile(x, 0.75, na.rm = T), 1))
}
iqrsumm <- function(x) {
  paste0(mymedian(x), " (", q25(x), " - ", q75(x), ")")
}

# Mask counts <20
summcat_mask20_NAs <- function(x) {
  outpt <- paste0(sum(is.na(x)), " (", signif(mean(is.na(x)) * 100, 3), "%)")
  if (sum(is.na(x)) < 20 & sum(is.na(x)) != 0) {
    outpt <- paste0(sum(is.na(x)), " (", signif(mean(is.na(x)) * 100, 3), "%)")
  }
  return(outpt)
}

# From an fcrrp object, choose the lambda and fit an UNpenalized model using the chosen covariates
fcrr_from_fcrrp <- function(fcrrp_model, truth, input_data, lambda, replace_coefs = TRUE) {
  # Get the index of the chosen lambda
  lindex <- match(lambda, fcrrp_model$lambda.path)
  coefs_for_lambda <- coef(fcrrp_model)[, lindex]
  names(coefs_for_lambda) <- names(input_data)
  coefs_for_lambda <- coefs_for_lambda[coefs_for_lambda != 0]
  input_z <- input_data %>% select(all_of(names(coefs_for_lambda)))
  rm(input_data)
  gc()

  # Fit the skeleton for the dummy object
  dummy_skel <- fastCrr(
    formula = Crisk(truth$CR_tte, truth$event_CR, failcode = 1) ~ .,
    data = input_z,
    returnDataFrame = T,
    getBreslowJumps = T,
    variance = F
  )

  # If replace_coefs, use the above model as a skeleton and replace values.
  # Otherwise, return the object directly (new fit using selected covariates)
  if (replace_coefs) {
    # Replace the values in the skeleton with those from the fcrrp object, index of the chosen lambda
    cmpmod_dummy <- dummy_skel
    cmpmod_dummy$coef <- coefs_for_lambda
    cmpmod_dummy$logLik <- fcrrp_model$logLik[lindex]
    cmpmod_dummy$logLik.null <- fcrrp_model$logLik.null[lindex]
    cmpmod_dummy$lrt <- NULL
    cmpmod_dummy$iter <- NULL
    cmpmod_dummy$converged <- fcrrp_model$converged[lindex]
    cmpmod_dummy$call <- "dummy"
    ## breslowjump in output was removed in version 1.24.5
    cmpmod_dummy$breslowJump$jump <- fcrrp_model$breslowJump[[lindex + 1]]

    # The returned object also includes the input data, with vars renamed to fstatus and ftime
    return(cmpmod_dummy)
  } else {
    names(dummy_skel$coef) <- names(coefs_for_lambda)
    return(dummy_skel)
  }
}

predict_fcrr_multirow <- function(model, newdata, times = c(1, 5, 10), covars = NULL, silent = F) {
  if (!silent) {
    tictoc::tic("Generating predicted probabilities from CIF")
  }
  if (is.null(covars)) {
    covars_from_model <- names(coef(model)) # if from dummy function
  } else {
    covars_from_model <- covars
  }

  z_pred <- data.frame(matrix(nrow = 0, ncol = length(times)))
  names(z_pred) <- paste0("pred", times)
  for (obs_n in 1:nrow(newdata)) {
    if (!silent) {
      if (obs_n %% 10000 == 1) {
        print(paste0("Predicting for patient ", obs_n, " of ", nrow(newdata)))
      }
    }
    z0 <- newdata[obs_n, ] %>% select(all_of(covars_from_model))
    test.pred.event <- predict(model, newdata = z0, getBootstrapVariance = FALSE, type = "none")
    CIFdat <- data.frame(ftime = test.pred.event$ftime, pred.prob = test.pred.event$CIF)
    if (any(!(times %in% unique(test.pred.event$ftime)))) {
      warning(paste("The requested horizon time(s) did not exist in unique failure times: ", times[!(times %in% test.pred.event$ftime)]))
    }
    CIF_horiz <- CIFdat %>% filter(ftime %in% times)
    CIF_wide <- CIF_horiz %>% pivot_wider(names_from = ftime, values_from = pred.prob, names_prefix = "pred")
    z_pred <- rbind(z_pred, CIF_wide)
  }
  if (!silent) {
    tictoc::toc()
  }
  return(z_pred)
}


predict_prob_calibrated <- function(ftime, fstatus, horizon, betax, outlier_threshold = 15) {
  # For the purpose of recalibrating for the mean patient predictors, exclude extreme outliers
  nonoutlier_i <- !abs(scale(betax)) > outlier_threshold
  warning(paste0(
    "For recalibration/baseline hazard purposes, excluding ",
    sum(!nonoutlier_i), " extreme outliers"
  ))


  # Use the BetaX Score to refit the breslow jumps (akin to baseline hazard)
  recalibLP <- fastCrr(Crisk(ftime[nonoutlier_i], fstatus[nonoutlier_i]) ~ betax[nonoutlier_i],
    variance = F, returnDataFrame = T
  )

  print(paste0(
    "The effect estimate for the risk score in recalibrated model was: ",
    coef(recalibLP) %>% signif(3)
  ))

  # Select the horizon time that will be used, if the requested one did not occur in the data
  if (!(horizon %in% recalibLP$uftime)) {
    horizon_use <- max(recalibLP$uftime[recalibLP$uftime < horizon])
    warning(paste0("Htime ", horizon, " did not exist, using htime ", horizon_use, " instead"))
  } else {
    horizon_use <- horizon
  }

  # Calculate the predicted CIF for the theoretical patient with the mean predictor values
  CIF0.recalibLP <- predict(recalibLP, newdata = mean(betax))

  # Extract CIF at the desired time point
  CIF0.t.recalibLP <- CIF0.recalibLP$CIF[CIF0.recalibLP$ftime == horizon_use]

  # Get S0(t) (survival) at mean predictor values
  S0.t.recalibLP <- 1 - CIF0.t.recalibLP

  # Calculate recalibrated S(t) for the rest of the patients
  surv.t.recalibLP <- S0.t.recalibLP^(exp(betax - mean(betax))) # for mean values, is S0

  # Calculate recalibrated p(t) for the rest of the patients
  pred.t.recalibLP <- 1 - surv.t.recalibLP
  return(pred.t.recalibLP)
}


# When ignoring competing risk, we need to also generate the predictions differently
predict_prob_calibrated_cox <- function(ftime, fstatus, horizons, betax, getScoreObj = T) {
  # Create the dataset for the model to be recalibrated
  tmpdatIN <- data.frame(ftime, fstatus, betax) %>%
    mutate(fstatus = as.numeric(fstatus == 1)) # convert 0/1/2 to 0/1 if not already done

  # Fit the model using the whole available followup time and the defined risk score
  recalibLP_cox <- coxph(Surv(ftime, fstatus) ~ betax, data = tmpdatIN, y = T, x = T)

  # get risk at horizon times h
  pred.t.recalibLP <- riskRegression::predictRisk(recalibLP_cox, newdata = tmpdatIN, times = horizons)
  pred.t.recalibLP <- pred.t.recalibLP %>% data.frame()
  names(pred.t.recalibLP) <- paste0("pred", horizons)

  # This provides calibration/performance measures at each horizon - also run and store object
  if (getScoreObj) {
    rrS <- riskRegression::Score(
      object = list("myModel" = recalibLP_cox),
      formula = Surv(ftime, fstatus) ~ 1,
      data = tmpdatIN,
      metrics = c("auc", "brier"),
      summary = c("risks", "IPA", "riskQuantile", "ibs"),
      plots = c("ROC", "Calibration", "boxplot"),
      times = horizons,
      cens.method = "pseudo",
      cens.model = "km",
      seed = 12345
    )
    return(list(
      pred = pred.t.recalibLP,
      model = recalibLP_cox,
      data = tmpdatIN,
      ScoreObj = rrS,
      horizons = horizons
    ))
  } else {
    return(list(
      pred = pred.t.recalibLP,
      model = recalibLP_cox,
      data = tmpdatIN,
      horizons = horizons
    ))
  }
}


predict_prob_calibrated_allH <- function(ftime, fstatus, horizons, betax) {
  init <- data.frame(betaX = betax)
  for (horizon in horizons) {
    prd <- predict_prob_calibrated(ftime, fstatus, horizon, betax)
    init <- init %>% mutate(prd)
    names(init)[length(names(init))] <- paste0("pred", horizon)
  }
  rt <- init %>% select(-betaX)
  return(rt)
}

# #Get AUC in competing risk + time dependent setting. Simple method for e.g. cross-validation steps.  No bootstrapping or CIs, or subgroups.
CR_auc_simple <- function(tte, status, marker, time) {
  aucdat_init <- data.frame("AUC_1" = 0, "AUC_2" = 0, horiz = 0) %>% filter(is.na(AUC_1))
  troc <- timeROC(tte, status, marker = marker, cause = 1, times = time, ROC = F, iid = FALSE) # auc_confint)
  aucdat <- cbind(
    "AUC_1" = troc$AUC_1[2],
    "AUC_2" = troc$AUC_2[2],
    horiz = time
  ) %>%
    data.frame()
  return(aucdat)
}

# #Get Brier score in competing risk + time dependent setting. Simple method for e.g. cross-validation steps.  No bootstrapping or CIs, or subgroups.
CR_calib_simple <- function(pred, truth, time) {
  if (max(truth$CR_tte) >= time) {
    f <- prodlim(Hist(CR_tte, event_CR) ~ 1, data = truth)
    pseudovalues <- jackknife(f, times = time, cause = 1) %>% data.frame()
    names(pseudovalues) <- "pseudo.t"
    brierScore <- mean((pseudovalues$pseudo.t - pred)^2)
    tmp <- data.frame("pseudo.t" = pseudovalues$pseudo.t, "pred" = pred)
    lmod <- lm(formula = I(pseudo.t) ~ 0 + pred, data = tmp)
    slp <- signif(lmod$coefficients[[1]], 3)
    return(data.frame("BrierScore" = brierScore, "Slope" = slp))
  } else {
    warning(paste0("The requested time is later than exists in the data: returning NA"))
    return(data.frame("BrierScore" = NA, "Slope" = NA))
  }
}


# Get calibration similar to above but ignoring the competing risks
CR_calib_simple_nocmprsk <- function(pred, truth, time) {
  # First, treat competing risk as censoring, if not already done
  truth <- truth %>% mutate(event_CR = as.numeric(event_CR == 1))

  # Then get pseudovalues for 'truth'
  if (max(truth$CR_tte) >= time) {
    f <- prodlim(Hist(CR_tte, event_CR) ~ 1, data = truth)
    pseudovalues <- jackknife(f, times = time, cause = 1) %>% data.frame()
    # In theis context, pseudovalues are given as survival probabilities, so take 1-
    names(pseudovalues) <- "pseudo.t"
    pseudovalues$pseudo.t <- 1 - pseudovalues$pseudo.t
    brierScore <- mean((pseudovalues$pseudo.t - pred)^2)
    tmp <- data.frame("pseudo.t" = pseudovalues$pseudo.t, "pred" = pred)
    lmod <- lm(formula = I(pseudo.t) ~ 0 + pred, data = tmp)
    slp <- signif(lmod$coefficients[[1]], 3)
    slpSE <- summary(lmod)$coefficients[1, 2]
    return(data.frame("BrierScore" = brierScore, "Slope" = slp, "SlopeSE" = slpSE))
  } else {
    warning(paste0("The requested time is later than exists in the data: returning NA"))
    return(data.frame("BrierScore" = NA, "Slope" = NA))
  }
}


CR_get_PRC <- function(pred, truth, htime, cause = 1) {
  # Get raw precision-recall curve values for a given single time and predictor
  pred <- signif(pred, 2) # Don't need too many points
  init <- data.frame(cutpoint = 0, precision = 0, recall = 0) %>% filter(is.na(cutpoint))
  for (cutp in unique(pred)) {
    sspn_dat <- SeSpPPVNPV(cutpoint = cutp, truth$CR_tte, truth$event_CR, pred, cause = cause, times = htime)
    suppressMessages(
      r_cut <- data.frame(cutpoint = cutp, precision = sspn_dat$PPV, recall = sspn_dat$TP) %>%
        cbind(sspn_dat$Stats %>% data.frame()) %>% .[2, ]
    )
    suppressMessages(init <- full_join(init, r_cut))
  }
  return(init)
}

CR_get_AUPRC <- function(pred, truth, htime, cause = 1) {
  PRC <- CR_get_PRC(pred, truth, htime, cause)
  AUPRC <- PRC %>%
    filter(!is.nan(precision)) %>%
    arrange(recall) %>% ## THIS MUST BE ARRANGED BY RECALL FIRST or else invalid result
    dplyr::summarize(AUPRC = trapz(recall, precision))
  real_cif <- cuminc(truth$CR_tte, truth$event_CR)
  cif_time <- timepoints(real_cif, times = htime)
  AUPRC <- AUPRC %>% mutate(cif_est_prob = cif_time$est[1, ])
  return(list("PRC" = PRC, "AUPRC" = AUPRC))
}


CR_var_bootstrap <- function(truth, pred, cause = 1, htime, n_bootstrap = 0,
                             AUC = TRUE, calib = TRUE, AUPRC = TRUE,
                             calib_nocmprsk = FALSE) {
  if (calib_nocmprsk) {
    # convert
    truth <- truth %>% mutate(event_CR = as.numeric(event_CR == 1))
  }
  if (n_bootstrap > 0) {
    # add bootstrap CIs and sds to AUC, auPRC, Brier Score, Slope,
    bootstrap_init <- data.frame(boot_n = 0) %>% filter(is.na(boot_n))
    tictoc::tic(paste0("Total time for bootstrapping htime ", htime, ":"))
    for (i in 1:n_bootstrap) {
      if (i %% 100 == 1) {
        print(paste0("Bootstrap number ", i, " of ", n_bootstrap))
      }
      indices_boot <- sample(1:nrow(truth), nrow(truth), replace = TRUE)
      truth_boot <- truth[indices_boot, ]
      pred_boot <- pred[indices_boot]
      i_boot <- data.frame(boot_n = i)
      # AUC
      if (AUC) {
        aucdat_boot <- CR_auc_simple(
          tte = truth_boot$CR_tte, status = truth_boot$event_CR,
          marker = pred_boot, time = htime
        )
        i_boot <- cbind(i_boot, aucdat_boot)
      }
      # AUPRC
      if (AUPRC) { # THIS SLOWS DOWN FUNCTION BY 100X
        auprcdat_boot <- CR_get_AUPRC(pred_boot, truth_boot, htime, cause)
        i_boot <- cbind(i_boot, auprcdat_boot$AUPRC)
      }
      # Brier Score and Slope
      if (calib) {
        if (calib_nocmprsk) {
          calibdat_boot <- CR_calib_simple_nocmprsk(pred = pred_boot, truth = truth_boot, time = htime)
        } else {
          calibdat_boot <- CR_calib_simple(pred = pred_boot, truth = truth_boot, time = htime)
        }
        i_boot <- cbind(i_boot, calibdat_boot)
      }
      # combine for current bootstrap sample
      i_boot <- i_boot %>% # , auprcdat_boot$AUPRC) %>%
        data.frame() ### %>% mutate(boot_n = i)
      # add current bootstrap to overall results
      bootstrap_init <- bind_rows(bootstrap_init, i_boot)
    }
    tictoc::toc()
    # if (any(is.na(bootstrap_init$AUC_1) | is.na(bootstrap_init$AUC_2))) {
    #   n_miss <- nrow(bootstrap_init %>% filter(is.na(AUC_1) | is.na(AUC_2)))
    #   warning(paste0("There are ", n_miss, " bootstrap AUCs with NA value"))
    # }

    bootstrap_CI_sd <- data.frame(horiz = htime)
    if (AUC) {
      suppressMessages(
        bootstrap_CI_sd_AUC <- bootstrap_init %>%
          dplyr::summarize(
            AUC_1_lower = quantile(AUC_1, 0.025, na.rm = T),
            AUC_1_upper = quantile(AUC_1, 0.975, na.rm = T),
            AUC_2_lower = quantile(AUC_2, 0.025, na.rm = T),
            AUC_2_upper = quantile(AUC_2, 0.975, na.rm = T),
            AUC_1_sd = sd(AUC_1, na.rm = T),
            AUC_2_sd = sd(AUC_2, na.rm = T)
          )
      )
      bootstrap_CI_sd <- cbind(bootstrap_CI_sd, bootstrap_CI_sd_AUC)
    }
    if (AUPRC) {
      suppressMessages(
        bootstrap_CI_sd_AUPRC <- bootstrap_init %>%
          dplyr::summarize(
            AUPRC_lower = quantile(AUPRC, 0.025, na.rm = T),
            AUPRC_upper = quantile(AUPRC, 0.975, na.rm = T),
            cif_est_prob_lower = quantile(cif_est_prob, 0.025, na.rm = T),
            cif_est_prob_upper = quantile(cif_est_prob, 0.975, na.rm = T),
            AUPRC_sd = sd(AUPRC, na.rm = T),
            cif_est_prob_sd = sd(cif_est_prob, na.rm = T)
          )
      )
      bootstrap_CI_sd <- cbind(bootstrap_CI_sd, bootstrap_CI_sd_AUPRC)
    }

    if (calib) {
      suppressMessages(bootstrap_CI_sd_calib <- bootstrap_init %>%
        dplyr::summarize(
          BrierScore_lower = quantile(BrierScore, 0.025, na.rm = T),
          BrierScore_upper = quantile(BrierScore, 0.975, na.rm = T),
          Slope_lower = quantile(Slope, 0.025, na.rm = T),
          Slope_upper = quantile(Slope, 0.975, na.rm = T),
          BrierScore_sd = sd(BrierScore, na.rm = T),
          Slope_sd = sd(Slope, na.rm = T)
        ))
      bootstrap_CI_sd <- cbind(bootstrap_CI_sd, bootstrap_CI_sd_calib)
    }

    return(list("raw" = bootstrap_init, "summary" = bootstrap_CI_sd)) # %>% mutate(horiz = htime)))
  } else {
    return("raw" = data.frame(horiz = htime), "summary" = data.frame(horiz = htime))
  }
}


# # This function gets evaluation metrics for ALL elements of a fastcrrp object, for use in cross-validation/selection of optimal lambda.
eval_measures_fastCrrp <- function(fcrrp_model, input_data, model_truth, newdata_z, newdata_truth,
                                   htimes = c(1, 5, 10), n_bootstrap = 0, replace_coefs = TRUE) {
  if (replace_coefs & packageVersion("fastcmprsk") > "1.1.1") {
    warning("replace_coefs may not be valid for this version - breslowjumps from unpenalized model used")
  }
  # e.all <- data.frame(matrix(nrow = 0, ncol = 9 + 6*3))
  # predprobs.all <- data.frame(matrix(nrow = 0, ncol = 3))
  e.all <- data.frame(lambda = 0) %>% filter(is.na(lambda))
  predprobs.all <- data.frame(lambda = 0) %>% filter(is.na(lambda))
  for (l in fcrrp_model$lambda.path) {
    lindex <- match(l, fcrrp_model$lambda.path)
    # Only run the evaluations if at lest 1 variable was selected, and convergence was reached
    if (any(fcrrp_model$coef[, lindex] != 0) & fcrrp_model$converged[lindex] == 1) {
      # An fcrr object, either 'dummy' with coefs from fcrrp, or unpenalized (replace_coefs)
      dummy_l <- fcrr_from_fcrrp(fcrrp_model, model_truth, input_data = input_data, l, replace_coefs = replace_coefs)
      print(paste0("Generating predictions for lambda ", lindex, " of ", length(fcrrp_model$lambda.path), ": ", l))
      predprobs <- predict_fcrr_multirow(dummy_l, newdata_z, htimes)
      # e.l <- data.frame(matrix(nrow = 0, ncol = 7 + 6*3))
      e.l <- data.frame("horiz" = 0) %>% filter(is.na(horiz))
      for (i in 1:length(htimes)) {
        e.i <- CR_auc_simple(newdata_truth$CR_tte, newdata_truth$event_CR,
          marker = predprobs[[i]], time = htimes[i]
        ) %>%
          cbind(CR_calib_simple(predprobs[[i]], newdata_truth, htimes[i])) %>%
          cbind((CR_get_AUPRC(predprobs[[i]], newdata_truth, htimes[i], cause = 1)$AUPRC))

        if (n_bootstrap > 0) {
          e.i <- e.i %>%
            cbind((CR_var_bootstrap(newdata_truth, predprobs[[i]],
              cause = 1, htime = htimes[i],
              n_bootstrap = n_bootstrap
            )$summary))
        }

        e.i <- e.i %>%
          select(-horiz) %>%
          mutate(horiz = htimes[i])
        ## if (i == 1) { names(e.l) <- names(e.i) }
        ## e.l <- rbind(e.l, e.i)
        e.l <- full_join_quiet(e.l, e.i)
      }
      e.l <- e.l %>% mutate(lambda = l, lindex)
      ## if (nrow(e.all) == 0) {names(e.all) <- names(e.l)}
      ## if (nrow(predprobs.all) == 0) {names(predprobs.all) <- names(predprobs)}
      ## e.all <- rbind(e.all, e.l)
      e.all <- full_join_quiet(e.all, e.l)
      predprobs.all <- full_join_quiet(predprobs.all, predprobs %>% mutate(lambda = l, lindex)) %>%
        select(paste0("pred", htimes), lindex, lambda)
    } else {
      print(paste0("Skipping lambda ", lindex, ": ", l, " due to nonconvergence or no variables selected"))
    }
  }
  return(list("pred" = predprobs.all, "performance" = e.all))
}
#
# This function gets evaluation metrics for fcrr (or dummy) objects.
eval_measures_fastCrr <- function(fcrr_model, newdata_z, newdata_truth, htimes = c(1, 5, 10),
                                  n_bootstrap = 0) {
  if (any(!(htimes %in% fcrr_model$uftime))) {
    stop("Requested horizon time did not exist in the unique failure times during training.")
  }
  predprobs <- predict_fcrr_multirow(fcrr_model, newdata_z, htimes)
  e.l <- data.frame("horiz" = 0) %>% filter(is.na(horiz))
  for (i in 1:length(htimes)) {
    e.i <- CR_auc_simple(newdata_truth$CR_tte, newdata_truth$event_CR, marker = predprobs[[i]], time = htimes[i]) %>%
      cbind(CR_calib_simple(predprobs[[i]], newdata_truth, htimes[i])) %>%
      mutate(horiz = htimes[i]) %>%
      cbind((CR_get_AUPRC(predprobs[[i]], newdata_truth, htimes[i], cause = 1)$AUPRC))

    if (n_bootstrap > 0) {
      e.i <- e.i %>%
        cbind((CR_var_bootstrap(newdata_truth, predprobs[[i]],
          cause = 1, htime = htimes[i],
          n_bootstrap = n_bootstrap
        )$summary))
    }

    e.i <- e.i %>%
      select(-horiz) %>%
      mutate(horiz = htimes[i])
    e.l <- full_join(e.l, e.i)
  }
  return(list("pred" = predprobs, "performance" = e.l))
}

# #Perform a fold of cross validation to tune lambda for LASSO fastcmprsk (train.fastcmprsk.penalized.FG.R)
fastCrrpCV <- function(z, truth, fold.i, fold, penalization_method, lambdas, htimes = c(1, 5, 10),
                       BIC_only = FALSE, n_bootstrap = 0, evaluate = TRUE) {
  # Prep data in fold
  truth.i <- truth[fold.i != fold, ]
  z.i <- z[fold.i != fold, ] %>% select_if(~ !(n_distinct(.) == 1))
  truth.test.i <- truth[fold.i == fold, ]
  z.test.i <- z[fold.i == fold, ]

  # Fit penalized models
  tictoc::tic("Fitting penalized model")
  cmpmod_penalized <- fastCrrp(
    formula = Crisk(truth.i$CR_tte, truth.i$event_CR, failcode = 1) ~ .,
    data = z.i,
    lambda = lambdas,
    standardize = FALSE, # numeric must already be standardized, avoid standardizing binary factors
    penalty = penalization_method,
    getBreslowJumps = TRUE
  )
  tictoc::toc()

  rownames(cmpmod_penalized$coef) <- colnames(z.i)

  if (evaluate) {
    if (!BIC_only) {
      tictoc::tic("Predicting and evaluating")
      eval_measures_obj <- eval_measures_fastCrrp(
        fcrrp_model = cmpmod_penalized, input_data = z.i, model_truth = truth.i,
        newdata_z = z.test.i, newdata_truth = truth.test.i, htimes = htimes,
        n_bootstrap = n_bootstrap, replace_coefs = TRUE
      )
      tictoc::toc()
      eval_measures <- eval_measures_obj$performance
      ## predprobs <- eval_measures$pred
      eval_measures_wide <- eval_measures %>%
        pivot_wider(names_from = horiz, values_from = c(
          AUC_1, AUC_2, BrierScore, Slope, AUPRC, cif_est_prob,
          # AUC_1_lower, AUC_2_lower, BrierScore_lower, Slope_lower, ##AUPRC_lower, cif_est_prob_lower,
          # AUC_1_upper, AUC_2_upper, BrierScore_upper, Slope_upper, ##AUPRC_upper, cif_est_prob_upper,
          # AUC_1_sd, AUC_2_sd, BrierScore_sd, Slope_sd ##, AUPRC_sd, cif_est_prob_sd
        ))
    }
  }

  # coefs <- cbind(predictor = colnames(z.i), cmpmod_penalized$coef)
  # plot(cmpmod_penalized)
  penalized_summary <- data.frame(
    logLik = cmpmod_penalized$logLik,
    lambda = cmpmod_penalized$lambda.path,
    iters_needed = cmpmod_penalized$iter,
    converged = cmpmod_penalized$converged,
    num_covars_selected = apply(cmpmod_penalized$coef, 2, function(x) {
      sum(x != 0)
    })
  ) %>%
    mutate(BIC = -2 * logLik + num_covars_selected * log(nrow(z.i))) %>%
    mutate(lindex = row_number())

  if (!BIC_only & evaluate) {
    penalized_summary <- penalized_summary %>%
      full_join(eval_measures_wide)
  }

  print(penalized_summary)

  # penalized_summary <- left_join(penalized_summary, cindices) %>% mutate(best_Cindex = Cindex == max(Cindex), fold)
  if (!BIC_only & evaluate) {
    return(list(
      "z.i.train" = z.i, "z.i.test" = z.test.i,
      "truth.i.train" = truth.i, "truth.i.test" = truth.test.i,
      "model" = cmpmod_penalized,
      "evaluation" = eval_measures_obj,
      "summary" = penalized_summary %>% mutate(fold)
    ))
  } else {
    return(list(
      "z.i.train" = z.i, "z.i.test" = z.test.i,
      "truth.i.train" = truth.i, "truth.i.test" = truth.test.i,
      "model" = cmpmod_penalized,
      "evaluation" = NULL,
      "summary" = penalized_summary %>% mutate(fold)
    ))
  }
}

select_best_lambda <- function(CVdata, criterion, minimum_improvement = TRUE, min_improve_by) {
  CVdata <- CVdata %>% filter(converged == 1 & num_covars_selected > 0)
  if (nrow(CVdata) == 0) {
    stop("No rows in CVdata, or no options converged")
  }
  if (criterion == "BIC") {
    dt <- CVdata %>%
      group_by(lindex) %>%
      summarise(BIC = mean(BIC)) %>%
      arrange(lindex) %>%
      mutate(
        min_BIC = BIC == min(BIC, na.rm = T),
        best_BIC = ifelse(min_BIC & lag(BIC) - BIC >= 2, TRUE, FALSE),
        best_BIC = ifelse(best_BIC | (lead(min_BIC) & !lead(best_BIC)), TRUE, FALSE)
      )
    min_BIC <- dt %>% filter(min_BIC)
    best_BIC <- dt %>% filter(best_BIC)
    if (minimum_improvement) {
      return(best_BIC)
    } else {
      return(min_BIC)
    }
  } else if (criterion %in% c(
    "mean_AUC_1", "mean_AUC_2", "AUC_1_1", "AUC_1_5",
    "AUC_1_10", "AUC_2_1", "AUC_2_5", "AUC_2_10",
    "AUC_1_mean", "AUC_2_mean",
    "mean_AUC1", "mean_AUC2", "AUC1_1", "AUC1_5",
    "AUC1_10", "AUC2_1", "AUC2_5", "AUC2_10",
    "AUC1_mean", "AUC2_mean"
  )) {
    suppressWarnings(dt <- CVdata %>% dplyr::rename(AUC = criterion))
    dt <- dt %>%
      group_by(lindex) %>%
      summarise(AUC = mean(AUC)) %>%
      arrange(lindex) %>%
      mutate(
        max_AUC = AUC == max(AUC, na.rm = T),
        AUC_improvement = AUC - lag(AUC),
        AUC_improved = AUC_improvement >= min_improve_by
      )
    max_AUC <- dt %>%
      filter(max_AUC) %>%
      slice(1) %>%
      select(lindex, AUC)
    names(max_AUC)[2] <- criterion
    # max_AUC <- left_join(max_AUC, CVdata)
    best_AUC <- dt %>%
      filter(AUC_improved) %>%
      tail(1) %>%
      select(lindex, AUC)
    names(best_AUC)[2] <- criterion
    if (minimum_improvement) {
      return(best_AUC)
    } else {
      return(max_AUC)
    }
  } else if (criterion %in% c("BrierScore_1", "BrierScore_5", "BrierScore_10", "mean_BrierScore", "BrierScore_mean")) {
    suppressWarnings(dt <- CVdata %>% dplyr::rename(BrierScore = criterion))
    dt <- dt %>%
      group_by(lindex) %>%
      summarise(BrierScore = mean(BrierScore)) %>%
      arrange(lindex) %>%
      mutate(min_BrierScore = BrierScore == min(BrierScore, na.rm = T))
    min_BrierScore <- dt %>%
      filter(min_BrierScore) %>%
      slice(1)
    names(min_BrierScore)[2] <- criterion
    # min_BrierScore <- left_join(min_BrierScore, CVdata)
    # best_AUC <- dt %>% filter(best_AUC)
    return(min_BrierScore)
  } else if (criterion %in% c("Slope_1", "Slope_5", "Slope_10", "mean_Slope", "Slope_mean")) {
    suppressWarnings(dt <- CVdata %>% dplyr::rename(Slope = criterion))
    dt <- dt %>%
      group_by(lindex) %>%
      summarise(Slope = mean(Slope)) %>%
      arrange(lindex) %>%
      mutate(err.factor = pmax(Slope, 1 / Slope, na.rm = T)) %>%
      mutate(best_Slope = err.factor == min(err.factor, na.rm = T)) %>%
      select(-err.factor)
    best_Slope <- dt %>%
      filter(best_Slope) %>%
      slice(1)
    names(best_Slope)[2] <- criterion
    return(best_Slope)
  } else if (criterion %in% c("AUPRC_1", "AUPRC_5", "AUPRC_10")) {
    cif.var <- gsub("AUPRC", "cif_est_prob", criterion)
    h <- gsub("AUPRC_", "", criterion)
    suppressWarnings(dt <- CVdata %>% dplyr::rename(
      AUPRC = criterion,
      cif_est_prob = cif.var
    ))
    dt <- dt %>%
      select(all_of(c("fold", "lindex", "AUPRC", "cif_est_prob"))) %>%
      mutate(AUPRC_cif_ratio = AUPRC / cif_est_prob) %>%
      group_by(lindex) %>%
      summarise(AUPRC_cif_ratio = mean(AUPRC_cif_ratio)) %>%
      arrange(lindex) %>%
      mutate(best_AUPRC = AUPRC_cif_ratio == max(AUPRC_cif_ratio, na.rm = T))
    best_AUPRC <- dt %>%
      filter(best_AUPRC) %>%
      slice(1)
    names(best_AUPRC)[2] <- paste0(names(best_AUPRC)[2], "_", h)
    # names(best_AUPRC)[3] <- cif.var
    return(best_AUPRC)
  } else {
    stop(paste0("Criterion ", criterion, " not yet implemented"))
  }
}

subset_data <- function(ydat, subset) {
  n_orig <- nrow(ydat)
  if (subset %in% c("Overall", "OVERALL")) {
    # do nothing
  } else if (subset %in% c("Black/African American", "BLACK")) {
    ydat <- ydat %>% filter(Race %in% c("BLACK OR AFRICAN AMERICAN", "BLACK"))
  } else if (subset %in% c("White/Caucasian", "WHITE")) {
    ydat <- ydat %>% filter(Race %in% c("WHITE"))
  } else if (subset %in% c("Hispanic/Latino", "HIS")) {
    ydat <- ydat %>% filter(Ethnicity %in% c("HISPANIC OR LATINO", "HIS"))
  } else if (subset %in% c("Non-Hispanic", "NHIS")) {
    ydat <- ydat %>% filter(Ethnicity %in% c("NOT HISPANIC OR LATINO", "NHIS"))
  } else if (subset %in% c("Female", "FEMALE")) {
    ydat <- ydat %>% filter(Gender %in% c("F"))
  } else if (subset %in% c("Male", "MALE")) {
    ydat <- ydat %>% filter(Gender %in% c("M"))
  } else if (subset == "DXYOUNG") {
    ydat <- ydat %>% filter(Age_DmDx_raw < 45)
  } else if (subset == "DXMIDAGE") {
    ydat <- ydat %>% filter(Age_DmDx_raw >= 45 & Age_DmDx_raw < 65)
  } else if (subset == "DXSENIOR") {
    ydat <- ydat %>% filter(Age_DmDx_raw >= 65)
  } else if (subset == "DXUNDER65") {
    ydat <- ydat %>% filter(Age_DmDx_raw < 65)
  } else if (subset == "DX1990") {
    ydat <- ydat %>% filter(DmDx_first >= -10 & DmDx_first < 0)
  } else if (subset == "DX2000") {
    ydat <- ydat %>% filter(DmDx_first >= 0 & DmDx_first < 10)
  } else if (subset == "DX2010") {
    ydat <- ydat %>% filter(DmDx_first >= 10 & DmDx_first < 20)
  } else if (subset == "EGFR30") {
    ydat <- ydat %>% filter(EGFR_raw < 30)
  } else if (subset == "EGFR45") {
    ydat <- ydat %>% filter(EGFR_raw >= 30 & EGFR_raw < 45)
  } else if (subset == "EGFR60") {
    ydat <- ydat %>% filter(EGFR_raw >= 45 & EGFR_raw < 60)
  } else if (subset == "EGFR90") {
    ydat <- ydat %>% filter(EGFR_raw >= 60 & EGFR_raw < 90)
  } else if (subset == "EGFRHEALTHY") {
    ydat <- ydat %>% filter(EGFR_raw >= 90)
  } else if (subset == "EGFRlt60") {
    ydat <- ydat %>% filter(EGFR_raw < 60)
  } else if (subset == "EGFRgt60") {
    ydat <- ydat %>% filter(EGFR_raw >= 60)
  } else {
    stop("Unrecognized subset")
  }
  print(paste0("Subset ", subset, " contains ", nrow(ydat), "/", n_orig, " patients (", signif(100 * nrow(ydat) / n_orig, 3), "%)"))
  return(ydat)
}


logmod_coef <- function(data, outcome, variable_name) {
  covars <- unique(c(variable_name, 
                     names(data)[grepl("Gender|Race|age", names(data))]))
  data <- cbind(data, "outcome" = outcome)
  mod.obj <- glm(as.formula(paste0("outcome ~ ", 
                                   paste0(covars, collapse = " + "))),
                 family = "binomial", data = data
  )
  rt <- data.frame(
    "Estimate" = coef(mod.obj)[variable_name],
    "P" = summary(mod.obj)$coefficients %>% .[rownames(.) == variable_name, 4]
  )
  return(rt)
}

# # Get N measures after t (t is years since DM), plus 1 measure if not NA 
# at/before that time point.
get_n_measures <- function(object_name) {
  d <- get(object_name) %>%
    right_join_quiet(table1_start %>% distinct(person_id)) %>%
    mutate(pre_y1 = (yrs <= 1)) %>%
    group_by(person_id, pre_y1) %>%
    count() %>%
    ungroup() %>%
    mutate(n_adj = ifelse(is.na(pre_y1), 0,
                          ifelse(pre_y1, pmin(n, 1, na.rm = T), n)
    )) %>%
    group_by(person_id) %>%
    summarise(repeated_measures = sum(n_adj))
  names(d)[2] <- object_name
  return(d)
}

process_marker <- function(marker_name, marker_file) {
  bmdat <- fread(marker_file) %>%
    dplyr::rename(person_id = PatientICN) %>%
    inner_join(table1_start %>% ungroup() %>% select(person_id))
  
  bmdat <- bmdat %>%
    filter(yrs >= -2) %>%
    select(person_id, yrs, all_of(marker_name))
  assign(marker_name, bmdat, envir = globalenv())
  # rm(bmdat, marker_name, marker_file)
}

coalesce0 <- function(x) { 
  coalesce(x, 0)
}


# For meds, define gaps as 30 days.
# process MEDS
med.gap <- function(data, min_gap_days = 30) {
  meds_time <- data %>%
    mutate(DaysSupply = coalesce(DaysSupply, 30)) %>%
    right_join_quiet(coh %>% dplyr::select(PatientICN, DeathDate)) %>%
    mutate(
      tstart = yrs,
      tstop = yrs + DaysSupply / 365.25
    ) %>%
    filter(tstop > tstart & (dt <= DeathDate | is.na(DeathDate))) %>%
    distinct(PatientICN, tstart, tstop) %>% ## , VAClassification) %>%
    arrange(PatientICN, tstart) %>%
    group_by(PatientICN) %>%
    # Collapse overlapping date ranges
    mutate(indx = c(0, cumsum(as.numeric(lead(tstart)) > 
                                cummax(as.numeric(tstop)))[-n()])) %>%
    group_by(PatientICN, indx) %>%
    summarise(tstart = min(tstart), tstop = max(tstop), .groups = "keep") %>%
    group_by(PatientICN) %>%
    mutate(
      med = 1,
      gap_tstart = ifelse(lead(tstart) > tstop, tstop, NA),
      gap_tstop = ifelse(lead(tstart) > tstop, lead(tstart), NA)
    )
  meds_gaps <- meds_time %>%
    dplyr::select(PatientICN, gap_tstart, gap_tstop) %>%
    filter(!is.na(gap_tstart) & !is.na(gap_tstop)) %>%
    mutate(
      med = 0,
      gap_length_days = (gap_tstop - gap_tstart) * 365.25
    )
  
  meds_gaps <- meds_gaps %>%
    filter(gap_length_days > 30) %>%
    dplyr::distinct(PatientICN, tstart = gap_tstart, med)
  meds_time <- full_join_quiet(meds_time, meds_gaps) %>%
    arrange(PatientICN, tstart) %>%
    dplyr::distinct(PatientICN, tstart, med) %>%
    group_by(PatientICN) %>%
    filter(row_number() == 1 | !(med == lag(med))) %>%
    mutate(tstart = round(tstart, 1)) %>%
    dplyr::distinct(PatientICN, tstart, med) %>%
    group_by(PatientICN, tstart) %>%
    summarize(med = max(med), .groups = "drop") %>%
    ungroup()
  
  meds_time <- meds_time %>%
    inner_join_quiet(coh_ids) %>%
    dplyr::rename(yrs = tstart)
  return(meds_time)
}

yr_round <- function(x) {
  x %>%
    inner_join_quiet(coh_rounded %>% dplyr::select(PatientICN, DeathDate)) %>%
    filter(dt <= DeathDate | is.na(DeathDate)) %>%
    select(-dt, -age, -DeathDate) %>%
    mutate(yrs_rounded = round(yrs, 1)) %>%
    group_by(PatientICN, yrs_rounded) %>%
    slice_max(yrs, with_ties = F) %>% 
    # If multiple round to the same time value, take the most recent
    ungroup() %>%
    select(-yrs) %>%
    dplyr::rename(yrs = yrs_rounded)
}

# Define a key for prettier names from variable names ####
pretty_key <- c(
  "RenalFailure" = "Renal Failure",
  "EthnicityHISPANICORLATINO" = "Hispanic/Latino",
  "EthnicityNOTHISPANICORLATINO" = "Non-Hispanic",
  "HIS" = "Hispanic/Latino",
  "NHIS" = "Non-Hispanic",
  "GenderF" = "Female",
  "GenderM" = "Male",
  "F" = "Female",
  "M" = "Male",
  "FEMALE" = "Female",
  "MALE" = "Male",
  "Female" = "Female",
  "Male" = "Male",
  "overall" = "Overall population",
  "OVERALL" = "Overall population",
  "RaceAMERICANINDIANORALASKANATIVE" = "American Indian/Alaska Native",
  "AIAN" = "American Indian/Alaska Native",
  "RaceASIAN" = "Asian",
  "ASIAN" = "Asian",
  "RaceBLACKORAFRICANAMERICAN" = "Black/African American",
  "BLACK" = "Black/African American",
  "RaceNATIVEHAWAIIANOROTHERPACIFICISLANDER" = "Native Hawaiian/Pacific Islander",
  "NHPI" = "Native Hawaiian/Pacific Islander",
  "RaceWHITE" = "White/Caucasian",
  "WHITE" = "White/Caucasian",
  "EthnicityHISPANIC.OR.LATINO1" = "Hispanic/Latino",
  "EthnicityNOT.HISPANIC.OR.LATINO" = "Non-Hispanic",
  "GenderF1" = "Female",
  "GenderM" = "Male",
  "overall" = "Overall population",
  "RaceAMERICAN.INDIAN.OR.ALASKA.NATIVE1" = "American Indian/Alaska Native",
  "RaceASIAN1" = "Asian",
  "RaceBLACK.OR.AFRICAN.AMERICAN1" = "Black/African American",
  "RaceBLACK" = "Black/African American",
  "RaceNATIVE.HAWAIIAN.OR.OTHER.PACIFIC.ISLANDER1" = "Native Hawaiian/Pacific Islander",
  "RaceWHITE" = "White/Caucasian",
  "EGFRgt60" = "Most recent eGFR at landmark > 60",
  "EGFRlt60" = "Most recent eGFR at landmark < 60",
  "DXSENIOR" = "Diagnosed with Diabetes after age 65",
  "DXYOUNG" = "Diagnosed with Diabetes before age 45",
  "DXMIDAGE" = "Diagnosed with Diabetes at age 45-65",
  "DXUNDER65" = "Diagnosed with Diabetes before age 65",
  "DX1990" = "Diagnosed with Diabetes in the 1990s",
  "DX2000" = "Diagnosed with Diabetes in the 2000s",
  "DX2010" = "Diagnosed with Diabetes in the 2010s",
  "Age_DmDx" = "Age at Index Date",
  "Gender" = "Gender",
  "Race" = "Race",
  "Ethnicity" = "Ethnicity",
  "SmokingFactor" = "Smoking Status",
  "ACEInhibitors" = "ACE Inhibitors",
  "AngiotensinIIRB" = "Angiotensin II Receptor Blockers",
  "Anydrug" = "Any Drug Prescribed",
  "BetaB1st" = "Beta Blockers",
  "BetaB2nd" = "2nd Generation Beta Blockers",
  "BetaB3rd" = "3rd Generation Beta Blockers",
  "Statin_High" = "High-Dose Statins",
  "Statin_Medium" = "Medium-Dose Statins",
  "Statin_Low" = "Low-Dose Statins",
  "Statin_Any" = "Statins",
  "Antidepressant_Rx" = "Antipsychotics", # This var was badly named
  "Antipsychotics_Rx" = "Antipsychotics",
  "Stimulant_Rx" = "Stimulants",
  "OpioidForPain_Rx" = "Opioids for Pain",
  "EEstradiol" = "Ethinyl Estradiol",
  "Levothyroxine" = "Levothyroxine",
  "Ezetimibe" = "Ezetimibe",
  "TriG_Severe" = "Gemfibrozil",
  "BAS" = "Bile Acid Sequestrants",
  "Aspirin" = "Aspirin",
  "Warfarin" = "Warfarin",
  "AntiAnginals" = "AntiAnginals",
  "LoopDiuretics" = "Loop Diuretics",
  "ThiazideDiuretics" = "Thiazide Diuretics",
  "CChannelBlockers" = "Calcium ChannelBlockers",
  "AlphaBlockers" = "Alpha Blockers",
  "AntiHypertensiveComb" = "Antihypertensive Combination Drugs",
  "PotassiumSparingDiuretics" = "Potassium Sparing Diuretics",
  "PeriphVasodilators" = "Peripheral Vasodilators",
  "DigitalisGlycosides" = "Digitalis Glycosides",
  "AntiArrhythmics" = "Antiarrhythmics",
  "Anticoag" = "Anticoagulants",
  "MI" = "Myocardial Infarction",
  "CVD" = "CVD",
  "stroke_infarct" = "Ischemic Stroke",
  "stroke_hem" = "Hemorrhagic Stroke",
  "stroke_any" = "Stroke",
  "old_MI" = "Old Myocardial Infarction",
  "DR" = "Diabetic Retinopathy",
  "ESRD" = "End-Stage Renal Disease",
  "DKD3p" = "DKD3 - Diabetic Kidney Disease or CKD Stage 3+",
  "DKD3b" = "DKD3b - Diabetic Kidney Disease or CKD Stage 3b+",
  "DKD4" = "DKD4 - Diabetic Kidney Disease or CKD Stage 4+",
  "alcdx_poss" = "Possible Alcohol-related Disorder (Dx)",
  "Nicdx_poss" = "Possible Nicotine Dependence (Dx)",
  "AmphetamineUseDisorder" = "Amphetamine Use Disorder",
  "OpioidOverdose" = "OpioidOverdose",
  "OUD" = "Opoid Use Disorder",
  "SedativeUseDisorder" = "Sedative Use Disorder",
  "SAE_sed" = "Adverse Effects of Antiepileptic, Sedatives-Hypnotics, and Antiparkinsonism drugs",
  "SUD_CatchAll" = "Other Substance Use Disorders", # Not all-inclusive, includes depend in icd 9 vs 10
  "SAE_Acet" = "Adverse Effects of Non-opioid Analgesics, Antipyretics and Antirheumatics",
  "SAE_OtherDrug" = "Adverse Effects of Other Drugs",
  "MH_CatchAll" = "Certain Mental Health Disorders",
  "ODEPRdx_poss" = "Possible Depression (Dx)",
  "TBI_Dx" = "Traumatic Brain Injury",
  "Concuss" = "Concussion",
  "Headache" = "Headaches",
  "SAE_Falls" = "Falls",
  "Amputation" = "Amputation",
  "SpinalCordInj" = "Spinal Chord Injury",
  "Backpain" = "Back Pain",
  "EH_PARALYSIS" = "Paralysis",
  "DEMENTIA" = "Dementia",
  "Parkinsons" = "Parkinsons",
  "DeliriumTremens" = "Delirium Tremens",
  "HIV" = "HIV",
  "Visual" = "Blindness, Low Vision, or Visual Disturbances",
  "Neuro" = "Neuropathy",
  "Homeless" = "Homelessness",
  "CAD" = "Coronary Artery Disease",
  "ChestPain_Dx" = "Chest Pain",
  "EH_ARRHYTH" = "Arrhythmias",
  "EH_BLANEMIA" = "Hemolytic Anemias",
  "EH_DefANEMIA" = "Deficiency Anemias",
  "EH_CHRNPULM" = "Chronic Pulmonary Disease",
  "EH_COAG" = "Coagulopathies",
  "EH_COMDIAB" = "Complications of Diabetes",
  "EH_ELECTRLYTE" = "Electrolyte Disorders",
  "EH_HEART" = "Heart Failure",
  "EH_HYPERTENS" = "Hypertension",
  "EH_LIVER" = "Liver Disease",
  "EH_OBESITY" = "Obesity (Dx)",
  "EH_OTHNEURO" = "Other Neurological Disorders",
  "EH_PERIVASC" = "Vascular Disease?",
  "EH_PULMCIRC" = "Pulmonary Heart Disease",
  "EH_RENAL" = "Prior Diagnosis of Renal Disease",
  "EH_RHEUMART" = "Arthropathies and autoinflammatory syndromes",
  "EH_UNCDIAB" = "Diabetes without mention of complications",
  "EH_VALVDIS" = "Valve Disease",
  "EH_WEIGHTLS" = "Weight Loss (Dx)",
  "PROTEINURIA" = "Previous diagnosis code for proteinuria",
  "Backpain" = "Back Pain",
  "EGFR" = "eGFR",
  "Anticoagulants" = "Anticoagulants",
  "age_td" = "Current Age",
  "ProtonPumpInhib" = "Proton Pump Inhibitors",
  "Albumin" = "Serum Albumin",
  "Corticosteroids" = "Corticosteroids",
  "Hematocrit" = "Hematocrit",
  "INR_NA" = "Missing INR",
  "Mg_NA" = "Missing Magnesium",
  "Uric_Acid_BSP_NA" = "Missing Serum Uric Acid",
  "SleepDisorder" = "SleepDisorder",
  "LymphFra" = "Lymphocyte Fraction",
  "RDW" = "RDW",
  "Bicarbonate" = "Bicarbonate",
  "Glucose" = "Glucose",
  "TotChol" = "Total Cholesterol",
  "WBC" = "log WBC",
  "AlkalinePhosphatase" = "AlkalinePhosphatase",
  "PulsePressure" = "PulsePressure",
  "HDLC" = "HDLC",
  "AST_NA" = "Missing AST",
  "SmokingFactorUNKNOWN" = "Unknown Smoking Status",
  "Bilirubin_BSP_conjugated_NA" = "Missing Conjugated Serum Bilirubin",
  "DCSI" = "DCSI",
  "Neut_NA" = "Missing Neutrophils",
  "Baso_NA" = "Missing Basophils",
  "Cl" = "Chloride",
  "BUN" = "BUN",
  "Trig" = "log Triglycerides",
  "A1C" = "HbA1c",
  "High_Uric_Acid" = "High Serum Uric Acid",
  "SBP" = "SBP",
  "Sulfonylurea" = "Sulfonylurea",
  "UACR_NA" = "Missing UACR",
  "Microalbuminuria_30_le_UACR_lt_300" = "Microalbuminuria",
  "Proteinuria_Dx" = "History of Proteinuria Dx",
  "Macroalbuminuria_UACR_ge_300" = "Macroalbuminuria"
)

# Function version: translate to pretty name but return original name if not found and warn
prettier <- function(name, key = pretty_key, required = F, warn.missing = T) {
  pretty_name <- pretty_key[name]
  if (is.na(pretty_name)) {
    if (required) {
      stop(paste0("There is no pretty name defined for variable '", name, "' in the key, which is required."))
    } else {
      if (warn.missing) {
        warning(paste0("There is no pretty name defined for variable '", name, "' in the key. Keeping original variable name."))
      }
      pretty_name <- name
      names(pretty_name) <- name
    }
  }
  return(pretty_name)
}
