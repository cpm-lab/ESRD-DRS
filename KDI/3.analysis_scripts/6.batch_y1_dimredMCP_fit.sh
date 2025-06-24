#!/bin/bash
#SBATCH -J y1_dimredMCP_fit
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 80G
#SBATCH -t 24:00:00

SELECTION_METHOD=MCP
OUTCOME=RenalFailure
MINCORR=0.05
CV_CRITERION=AUC_1_10
REDUCTION_METHOD=LOGISTIC2STEP

R CMD BATCH --no-save --no-restore \
"--args ${SELECTION_METHOD} ${OUTCOME} ${MINCORR} ${CV_CRITERION} ${REDUCTION_METHOD}" \
3.analysis_scripts/6.y1_dimredMCP_fit.R \
logs/log.6.y1_dimredMCP_fit.select${SELECTION_METHOD}.${OUTCOME}.minCorr${MINCORR}.CVcrit${CV_CRITERION}.reduce${REDUCTION_METHOD}.Rout

