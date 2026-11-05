#!/bin/bash -l
#SBATCH --job-name=solar_system
#SBATCH --partition=normal
#SBATCH --cpus-per-task=64
#SBATCH --mem=64G
#SBATCH --time=24:00:00
#SBATCH --output=sim_output/%x_%j.log

# Runs Theoretical_SS.R (all 15 scenarios) in its own output directory, which
# keeps a copy of the script that was run as a record. Submit from the
# repository root.
module load R/4.6.1

model=Theoretical_SS
out=sim_output/${model}_$(date +%Y%m%d_%H%M%S)
mkdir -p $out
cp ${model}.R $out/
echo "model: $model"
grep -nE "^n_runs|^n_years|^base_seed|^test_run" $out/${model}.R
cd $out
Rscript ${model}.R
