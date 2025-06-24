# combine biomarker datasets from sub-cohorts so that overall summary statistics 
# can be calculated across the whole cohort.

library(dplyr)
library(tictoc)
library(data.table)
library(ggplot2)

# Run from parent directory
source("config_mnt.R")
source("functions.R")

combined_dir <- paste0(cohort_dir, "cohAll/biomarkers_combined/")
dir.create(combined_dir)
outcome <- "RenalFailure"
feature_key_loc <- paste0(preprocess_dir, "feature_key.", outcome, ".RData")
load(feature_key_loc)

tictoc::tic("All Combining biomarkers")
for (cohort in 99:34) {
  tictoc::tic(paste0("Adding data for cohort ", cohort, "..."))
  load(paste0(cohort_dir, "coh", cohort, "/data.RData"))
  objs_to_rm <- setdiff(ls(), 
                        c(BBM_obj, "BBM_obj", "cohort", 
                          "combined_dir", "cohort_dir"))
  rm(list = objs_to_rm)
  gc()
  for (bm in c(BBM_obj)) {
    fwrite(
      get(bm) %>% select(all_of(c("PatientICN", "dt", "yrs", bm))) %>%
        mutate(cohort), paste0(combined_dir, bm, "_combined.txt"),
      sep = "\t",
      append = (cohort != 99), col.names = (cohort == 99)
    )
    tictoc::toc()
  }
}
tictoc::toc()
