#!/bin/bash

#SBATCH -J vary_gen_time                 # Job name
#SBATCH -o vary_gen_time.%j.o            # Name of stdout output file (%j expands to jobId)
#SBATCH -e vary_gen_time.%j.e            # Name of stdout output file (%j expands to jobId)
#SBATCH -p normal                        # Queue name, small is for <=2 nodes
#SBATCH -N 3                   	         # Total number of nodes requested (24 cores/node)
#SBATCH -n 4                             # Total number of tasks to run
#SBATCH -t 01:00:00            	         # Run time (hh:mm:ss)
#SBATCH -A A-ib1                         # Allocation name
#SBATCH --mail-user=emjavan@utexas.edu   # Email for notifications
#SBATCH --mail-type=all                  # Type of notifications, begin, end, fail, all

# Load newest R module on Frontera
module load Rstats/4.0.3

# Load launcher
module load launcher

# Configure launcher
EXECUTABLE=$TACC_LAUNCHER_DIR/init_launcher
PRUN=$TACC_LAUNCHER_DIR/paramrun
CONTROL_FILE=commands_vary_R0_close_to_1.txt
export LAUNCHER_JOB_FILE=commands_vary_R0_close_to_1.txt
export LAUNCHER_WORKDIR=`pwd`
export LAUNCHER_SCHED=interleaved

# Start launcher
$PRUN $EXECUTABLE $CONTROL_FILE
