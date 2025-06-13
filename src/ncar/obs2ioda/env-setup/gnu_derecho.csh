#!/bin/csh

# Load modules
module purge
module load ncarenv/23.09
module load gcc/12.2.0 
module load netcdf/4.9.2
module load cmake
module load conda/latest

# Check if conda is available
if ( ! $?CONDA_EXE ) then
    echo "Error: conda not found. Check if the 'conda/latest' module loaded correctly."
    exit 1
endif

# Check if obs2ioda conda environment exists
setenv HAS_ENV `conda info --envs | awk '{print $1}' | grep -x "obs2ioda"`

if ( "$HAS_ENV" == "" ) then
    echo "Creating conda environment 'obs2ioda'..."
    conda env create -f environment.yml
endif

# Initialize conda if needed (optional if not already initialized in .cshrc)
if ( $?CONDA_SHLVL == 0 ) then
    source `conda info --base`/etc/profile.d/conda.csh
endif

# Activate the environment
conda activate obs2ioda

