#!/bin/bash

# Load modules
module --force purge
module load ncarenv/23.09
module load gcc/12.2.0
module load netcdf/4.9.2
module load cmake
module load conda/latest 

# Check if conda is available
if ! command -v conda &> /dev/null; then
    echo "Error: conda not found. Check if the 'anaconda' module loaded correctly."
    exit 1
fi

# Check if obs2ioda conda environment exists
if ! conda info --envs | awk '{print $1}' | grep -qx "obs2ioda"; then
    echo "Creating conda environment 'obs2ioda'..."
    conda env create -f environment.yml
fi

# Activate the environment
conda activate obs2ioda

