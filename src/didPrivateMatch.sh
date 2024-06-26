#!/bin/bash
#SBATCH -J didPrivateMatch8k 
#SBATCH -N 1
#SBATCH -n 48
#SBATCH -o ../slurmtemp/.o%j
#SBATCH -p bsudfq
#SBATCH -t 96:00:00

# Activate your environment
~/.bashrc
conda activate r-env

export TMPDIR=/local 

Rscript /bsuhome/nicholaskolarik/ch3/src/didPrivate.R
