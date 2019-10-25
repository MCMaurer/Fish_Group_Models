#!/bin/bash

# setting the home directory
#SBATCH -D /home/mjculsha/Fish_Group_Models_FARM/other_models

# set number of cpus per task
#SBATCH --cpus-per-task=4

# set max memory to 16G
#sbatch --mem=16000s

# set number of processes per node
#SBATCH --ntasks-per-node=4

# set max wall time to 5 days
#SBATCH --time=5-0

# set the name of the job
#SBATCH --job-name=lat_chases

# mail alerts at beginning and end
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# send mail here
#SBATCH --mail-user=mjculshawmaurer@ucdavis.edu

# where to send standard output
#SBATCH --output=./slurm_outputs/slurm-%j-%A-%a.out

# start job from the directory it was submitted
cd $SLURM_SUBMIT_DIR

# load R
module load R

Rscript scripts/model_scripts/known_lat_all_chases.R
