#!/bin/bash
#SBATCH -J predict_L5_unpenalized
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 80G
#SBATCH -t 4:00:00
#SBATCH --array=5,10
#SBATCH --output=logs/slurm-predict-%A_%a.out
#SBATCH --error=logs/slurm-predict-%A_%a.out

LANDMARK=${SLURM_ARRAY_TASK_ID}
SELECTION_METHOD=MCP
OUTCOME=RenalFailure
MINCORR=0.05
REDUCTION_METHOD=LOGISTIC2STEP
FIT_METHOD=REFIT
CV_CRITERION=AUC_1_10
BIC_ONLY=F
UNPENALIZED=T

#set config in R script
R CMD BATCH --no-save --no-restore \
"--args ${SELECTION_METHOD} ${OUTCOME} ${MINCORR} ${CV_CRITERION} ${REDUCTION_METHOD} ${FIT_METHOD} ${LANDMARK} ${BIC_ONLY} ${UNPENALIZED} " \
3.analysis_scripts/8.predict_L5_unpenalized.R \
logs/log.10.predict_L5_unpenalized.select${SELECTION_METHOD}.${OUTCOME}.mincorr${MINCORR}.CVcrit${CV_CRITERION}.reduce${REDUCTION_METHOD}.${FIT_METHOD}.L${LANDMARK}.BIC${BIC_ONLY}.unpenalized${UNPENALIZED}.Rout


