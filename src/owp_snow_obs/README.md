## Converter for NOAA Office of Water Prediction (OWP) snow observations
These are the observations assimilated into SNODAS, prepared for us as daily csv files by OWP.

The converter converts daily obs csv files to daily ioda-converters netcdf files.  

Related work to preparing and running these scripts can be found in [https://github.com/NCAR/nwm_jedi_dev/tree/master/owp_obs/ioda-converters](https://github.com/NCAR/nwm_jedi_dev/tree/master/owp_obs/ioda-converters).  

More details on the converter can be found as in the following example (which needs keept up to date).
```
(whp) vagrant@vagrant[793]:~/jedi/bundle-ioda/ioda-bundle/iodaconv/src/owp_snow_obs> export PYTHONPATH=/home/vagrant/jedi/bundle-ioda/build/lib/python3.8/pyioda
(whp) vagrant@vagrant[794]:~/jedi/bundle-ioda/ioda-bundle/iodaconv/src/owp_snow_obs>  /home/vagrant/jedi/bundle-ioda/build/bin/owp_snow_obs.py --help
usage: owp_snow_obs.py [-h] -i INPUT [-o OUTPUT] [--thin_swe THIN_SWE] [--thin_depth THIN_DEPTH]
                       [--thin_random_seed THIN_RANDOM_SEED] [--err_fn ERR_FN]

Reads snow OWP observations in CSV files and converts to IODA output files.

optional arguments:
  -h, --help            show this help message and exit

required arguments:
  -i INPUT, --input INPUT
                        path of OWP snow observation input file(s)
  -o OUTPUT, --output OUTPUT
                        path of IODA output file

optional arguments:
  --thin_swe THIN_SWE   percentage of random thinning for SWE, from 0.0 to 1.0. Zero indicates no thinning is
                        performed. (default: 0.0)
  --thin_depth THIN_DEPTH
                        percentage of random thinning for snow depth, from 0.0 to 1.0. Zero indicates no thinning
                        is performed. (default: 0.0)
  --thin_random_seed THIN_RANDOM_SEED
                        A random seed for reproducible random thinning. Default is total # seconds from
                        1970-01-01 to the day of the data provided.
  --err_fn ERR_FN       Name of error function to apply. The options are hardcoded in the module,
                        currently:[dummy_error]
```