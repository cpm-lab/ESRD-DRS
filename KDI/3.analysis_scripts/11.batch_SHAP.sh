#!/bin/bash
#SBATCH -J SHAP
#SBATCH -n 1
#SBATCH --cpus-per-task 15
#SBATCH --mem 80G
#SBATCH -t 24:00:00
#SBATCH --array=1-10
#SBATCH --output=logs/slurm-shap-%A_%a.out
#SBATCH --error=logs/slurm-shap-%A_%a.out

# Need to submit this one 9 times for each landmark/horizon
# The array is for splitting the population randomly into 10 groups, 
# as it is slow to run all at once

CUTGROUP=${SLURM_ARRAY_TASK_ID}
LANDMARK=10
SELECTION_METHOD=MCP
OUTCOME=RenalFailure
MINCORR=0.05
REDUCTION_METHOD=LOGISTIC2STEP
FIT_METHOD=REFIT
CV_CRITERION=AUC_1_10
BIC_ONLY=F
UNPENALIZED=T
HORIZ=1

echo "landmark is ${LANDMARK} and subset is ${CUTGROUP}"

#set config in R script
R CMD BATCH --no-save --no-restore \
"--args ${SELECTION_METHOD} ${OUTCOME} ${MINCORR} ${CV_CRITERION} ${REDUCTION_METHOD} ${FIT_METHOD} ${LANDMARK} ${BIC_ONLY} ${UNPENALIZED} ${CUTGROUP} ${HORIZ}" \
3.analysis_scripts/11.SHAP.R \
logs/log.11.SHAP.select${SELECTION_METHOD}.${OUTCOME}.mincorr${MINCORR}.CVcrit${CV_CRITERION}.reduce${REDUCTION_METHOD}.${FIT_METHOD}.L${LANDMARK}.BIC${BIC_ONLY}.unpenalized${UNPENALIZED}.${CUTGROUP}.${HORIZ}.Rout


