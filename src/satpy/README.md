# Satpy converter setup guide

The `ioda-converter/src/satpy` directory contains IODA converters that use the ([Satpy](https://satpy.readthedocs.io/en/stable/index.html)) python library.  To use these converters it is asssumed one can build ioda-converters:
- Use ([spack-stack](https://jointcenterforsatellitedataassimilation-jedi-docs.readthedocs-hosted.com/en/latest/using/jedi_environment/spackbuild.html)) environment
- Have a python virtual environment created via:  `python3 -m venv --system-site-packages venv` followed by `source venv/bin/activate`

If one has these prerequisites they can proceed to install satpy into that virtual environment

## Configuration Guide

**satpy**
   - Recommend [pip install](https://satpy.readthedocs.io/en/stable/install.html#pip-based-installation) to virtual environment
