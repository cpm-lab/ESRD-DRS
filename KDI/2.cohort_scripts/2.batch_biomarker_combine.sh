#!/bin/bash
#SBATCH -J biomarker_combine
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH -t 8:00:00

module load modules/R
module load modules/gcc/10.1.0 

COH=${SLURM_ARRAY_TASK_ID}
echo cohort is $COH

R CMD BATCH --no-save --no-restore \
2.cohort_scripts/2.biomarker_combine.R \
logs/biomarker_combine.Rout


