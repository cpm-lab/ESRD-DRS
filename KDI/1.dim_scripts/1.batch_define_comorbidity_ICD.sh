#!/bin/bash
#SBATCH -J comor_dim
#SBATCH -n 1
#SBATCH --cpus-per-task 4
#SBATCH --mem 20G
#SBATCH -t 8:00:00

module load modules/R
module load modules/gcc/10.1.0 

R CMD BATCH --no-save --no-restore \
1.dim_scripts/1.define_comorbidity_ICD.R \
logs/1.dim.define_comorbidity_ICD.Rout


