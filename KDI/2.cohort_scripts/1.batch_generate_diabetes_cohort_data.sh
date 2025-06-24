#!/bin/bash
#SBATCH -J data_extract
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH -t 8:00:00
#SBATCH --array=34-100

module load modules/R
module load modules/gcc/10.1.0 

COH=${SLURM_ARRAY_TASK_ID}
echo cohort is $COH

R CMD BATCH --no-save --no-restore \
"--args ${COH}" \
2.cohort_scripts/1.generate_diabetes_cohort_data.R \
logs/datagen.coh${COH}.Rout


