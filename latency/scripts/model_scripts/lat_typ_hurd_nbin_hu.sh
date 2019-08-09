#!/bin/bash

# setting the home directory
#SBATCH -D /home/mjculsha/Fish_Group_Models_FARM/latency

# set the number of nodes to 1
#SBATCH --nodes=1

# total number of tasks
#SBATCH --ntasks=3

# set number of processes per node
#SBATCH --ntasks-per-node=3

# set max memory to 8gb per process
#sbatch --mem-per-cpu=8000

# set max wall time to 5 days
#SBATCH --time=5-0

# set the name of the job
#SBATCH --job-name=lat_typ_nbin_hu

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

Rscript scripts/model_scripts/lat_typ_hurd_nbin_hu.R
