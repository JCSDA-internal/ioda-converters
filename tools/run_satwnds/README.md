# Run Sat Winds

Split a sat wind bufr file into its subsets, and runs bufr2ioda.x on each one 
with the proper configuration for that subset.

## Dependencies

* **Python 3.6+**

The following executables must be available in the shell path.

* **bufr2ioda.x** - From ioda_converters _feature/bufr-enhanced-exports_
* **split_by_subset.x** - From NCEPLibs-Bufr _feature/split_by_subset_

## Usage

./run_satwnds.py -t<num_threads> **path/to/satwnds.bufr**

## Output

Script will create a directory that follows the pattern 
**satwnd_processing**__datetime_, where _datetime_ is the timestamp when the 
script was run. All resulting .nc files will appear here.
