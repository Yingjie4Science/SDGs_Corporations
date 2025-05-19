#!/bin/bash
#
#SBATCH --time=30:00:00
#SBATCH --nodes=1-6
#SBATCH --ntasks-per-node=32
#SBATCH --mem-per-cpu=16G
#SBATCH --mail-type=ALL
#SBATCH --mail-user=yingjiel@stanford.edu
#SBATCH --partition=hns,normal
#SBATCH --job-name="tm_sdg"

ml R/4.3.2

Rscript 21_Text_mining_batch_sherlock.R