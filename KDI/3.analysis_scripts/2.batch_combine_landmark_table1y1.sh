#!/bin/bash
#SBATCH -J combine_landmark_table1y1
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH -t 3:00:00

module load modules/R
module load modules/gcc/10.1.0 

OUTCOME=RenalFailure

R CMD BATCH --no-save --no-restore \
"--args ${OUTCOME}" \
3.analysis_scripts/2.combine_landmark_table1y1.R \
logs/2.table1y1.Rout



