#!/bin/bash
#SBATCH -J y1_dimred_CV_eval1lambda
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 80G
#SBATCH -t 4:00:00
#SBATCH --array=0-99

INDX=${SLURM_ARRAY_TASK_ID}
SELECTION_METHOD=MCP
OUTCOME=RenalFailure
MINCORR=0.05
REDUCTION_METHOD=LOGISTIC2STEP

CVFOLD=$((INDX % 5 + 1))
LINDEX=$((INDX / 5 + 1))

#set config in R script
R CMD BATCH --no-save --no-restore \
"--args ${CVFOLD} ${SELECTION_METHOD} ${OUTCOME} ${MINCORR} ${LINDEX} ${REDUCTION_METHOD}" \
3.analysis_scripts/5.y1_dimred_CV_eval1lambda.R \
logs/log.5.y1_dimred_CV_eval1lambda.l.CVfold${CVFOLD}.select${SELECTION_METHOD}.${OUTCOME}.mincorr${MINCORR}.${LINDEX}.reduce${REDUCTION_METHOD}.Rout


