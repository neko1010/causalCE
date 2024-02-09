#!/bin/bash
#SBATCH -J didPrivate 
#SBATCH -N 1
#SBATCH -n 48
#SBATCH -o ../slurmtemp/.o%j
#SBATCH -p bsudfq
#SBATCH -t 24:00:00

# Activate your environment
~/.bashrc
conda activate r-env

export TMPDIR=/local 

Rscript /bsuhome/nicholaskolarik/ch3/src/didPrivate.R
