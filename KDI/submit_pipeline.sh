#Run this script to run the entire pipeline with appropriate job dependencies. 
#To run part of the pipeline, comment out the unnecessary steps. 

# Capture the job submission output string which includes the job id as the last word
JOB1_1=`sbatch 1.cohort_scripts/1.batch_define_comorbidity_ICD.sh`

# Strip the string to just the JobID, so we can use it to create dependency for next job
JOBID_1_1=${JOB1_1##* } 
echo Submitted Job 1.1, comorbidities dim, ${JOBID_1_1}


JOB2_1=`sbatch --dependency=afterok:${JOBID_1_1} 2.cohort_scripts/1.batch_generate_diabetes_cohort_data.sh`
JOBID_2_1=${JOB2_1##* } 
echo Submitted Job 2.1, data generation, ${JOBID_2_1}, with dependency on ${JOBID_1_1}


JOB2_2=`sbatch --dependency=afterok:${JOBID_2_1} 2.cohort_scripts/2.batch_biomarker_combine.sh`
JOBID_2_2=${JOB2_2##* } 
echo Submitted Job 2.2, data generation, ${JOBID_2_2}, with dependency on ${JOBID_2_1}


# Continue with the rest of the pipeline
JOB3_1=`sbatch --dependency=afterok:${JOBID_2_1} 3.analysis_scripts/1.batch_preprocess.sh`
JOBID_3_1=${JOB3_1##* } 
echo Submitted Job 3.1, preprocessing tdept, ${JOBID_3_1}, with dependency on ${JOBID_2_1}
 
 
JOB3_2=`sbatch --dependency=afterok:${JOBID_3_1}:${JOBID_2_2} 3.analysis_scripts/2.batch_combine_landmark_table1y1.sh`
JOBID_3_2=${JOB3_2##* } 
echo Submitted Job 3.2, ${JOBID_3_2}, combine landmarks, with dependency on ${JOBID_3_1} and ${JOBID_2_2}


### SUBMIT THIS TWICE ONCE FOR TRAIN AND TEST
JOB3_3a=`sbatch --dependency=afterok:${JOBID_3_2} 3.analysis_scripts/3.batch_impute_t1.sh` #NEED ARG
JOBID_3_3a=${JOB3_3a##* } 
echo Submitted Job 3.3a, ${JOBID_3_3a}, impute for training set, with dependency on ${JOBID_3_2}

JOB3_3b=`sbatch --dependency=afterok:${JOBID_3_2} 3.analysis_scripts/3.batch_impute_t1.sh` #NEED ARG
JOBID_3_3b=${JOB3_3b##* } 
echo Submitted Job 3.3b, ${JOBID_3_3b}, impute for testing set, with dependency on ${JOBID_3_2}


JOB3_4=`sbatch --dependency=afterok:${JOBID_3_3a} 3.analysis_scripts/4.batch_y1_dimred_CV_fit.sh`
JOBID_3_4=${JOB3_4##* } 
echo Submitted Job 3.4, ${JOBID_3_4}, with dependency on ${JOBID_3_3a}


JOB3_5=`sbatch --dependency=afterok:${JOBID_3_4} 3.analysis_scripts/5.batch_y1_dimred_CV_eval1lambda.sh`
JOBID_3_5=${JOB3_5##* }
echo Submitted Job 3.5, ${JOBID_3_5}, with dependency on ${JOBID_3_4}


JOB3_6=`sbatch --dependency=afterok:${JOBID_3_5} 3.analysis_scripts/6.batch_y1_dimredMCP_fit.sh`
JOBID_3_6=${JOB3_6##* }
echo Submitted Job 3.6, ${JOBID_3_6}, with dependency on ${JOBID_3_5}


JOB3_7=`sbatch --dependency=afterok:${JOBID_3_6} 3.analysis_scripts/7.batch_L5_unpenalized_fit.sh `
JOBID_3_7=${JOB3_7##* }
echo Submitted Job 3.7, ${JOBID_3_7}, with dependency on ${JOBID_3_6}


JOB3_8=`sbatch --dependency=afterok:${JOBID_3_7} 3.analysis_scripts/8.batch_predict_L5_unpenalized.sh`
JOBID_3_8=${JOB3_8##* }
echo Submitted Job 3.8, ${JOBID_3_8}, with dependency on ${JOBID_3_7}


JOB3_9=`sbatch --dependency=afterok:${JOBID_3_8} 3.analysis_scripts/9.batch_predict_MCP.sh`
JOBID_3_9=${JOB3_9##* }
echo Submitted Job 3.9, ${JOBID_3_9}, with dependency on ${JOBID_3_8}


JOB3_10=`sbatch --dependency=afterok:${JOBID_3_9} 3.analysis_scripts/10.batch_eval_subsets_recode.sh`
JOBID_3_10=${JOB3_10##* }
echo Submitted Job 3.10, ${JOBID_3_10}, with dependency on ${JOBID_3_9}


JOB3_11=`sbatch --dependency=afterok:${JOBID_3_10} 3.analysis_scripts/11.batch_SHAP.sh `
JOBID_3_11=${JOB3_11##* }
echo Submitted Job 3.11, ${JOBID_3_11}, with dependency on ${JOBID_3_10}

