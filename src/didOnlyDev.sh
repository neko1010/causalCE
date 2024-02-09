#!/bin/bash
#SBATCH -J HUC8didOnlyDev
#SBATCH -N 1
#SBATCH -n 48
#SBATCH -o ../slurmtemp/.o%j
#SBATCH -p bsudfq
#SBATCH -t 24:00:00

# Activate your environment
~/.bashrc
conda activate r-env

export TMPDIR=/local 

Rscript /bsuhome/nicholaskolarik/ch3/src/DiDonlyDev.R
