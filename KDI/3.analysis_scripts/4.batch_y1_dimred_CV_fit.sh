#!/bin/bash
#SBATCH -J y1_dimred_CV_fit
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 80G
#SBATCH -t 4:00:00
#SBATCH --array=1-5

CVFOLD=${SLURM_ARRAY_TASK_ID}
SELECTION_METHOD=MCP
OUTCOME=RenalFailure
MINCORR=0.05
REDUCTION_METHOD=LOGISTICPVAL

#set config in R script
R CMD BATCH --no-save --no-restore \
"--args ${CVFOLD} ${SELECTION_METHOD} ${OUTCOME} ${MINCORR} ${REDUCTION_METHOD}" \
3.analysis_scripts/4.y1_dimred_CV_fit.R \
logs/log.4.y1_dimred_CV_fit_l.CVfold${CVFOLD}.select${SELECTION_METHOD}.${OUTCOME}.mincorr${MINCORR}.reduce${REDUCTION_METHOD}.Rout


