#!/bin/bash
#SBATCH -J impute_t1
#SBATCH -n 1
#SBATCH --cpus-per-task 8
#SBATCH --mem 120G
#SBATCH -t 24:00:00
#SBATCH --array=1-5

module load modules/R
module load modules/gcc/10.1.0 

OUTCOME=RenalFailure
REP=${SLURM_ARRAY_TASK_ID}
MINCORR=0.05
TRAINTEST=test #train or test set - must run both

R CMD BATCH --no-save --no-restore \
"--args ${REP} ${OUTCOME} ${MINCORR} ${TRAINTEST}" \
3.analysis_scripts/3.impute_t1.R \
logs/impute.r${REP}.${OUTCOME}.mincorr${MINCORR}.${TRAINTEST}.Rout



