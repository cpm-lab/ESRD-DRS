#!/bin/bash
#SBATCH -J preprocess
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH -t 4:00:00
#SBATCH --array=34-99

module load modules/R
module load modules/gcc/10.1.0 

COH=${SLURM_ARRAY_TASK_ID}
OUTCOME=RenalFailure

echo cohort is $COH

R CMD BATCH --no-save --no-restore \
"--args ${COH} ${OUTCOME}" \
3.analysis_scripts/1.preprocess.R \
logs/log.1.preprocess.coh${COH}.Rout



