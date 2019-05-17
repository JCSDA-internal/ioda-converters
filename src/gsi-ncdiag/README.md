# gsi-ncdiag

## gsincdiag.py
Currently the gsincdiag library supports four different classes of GSI netCDF diag files:
* Conventional
* Radiances
* AOD
* Ozone

There are a series of dictionaries at the top of the file gsincdiag.py that can be modified to:
* add additional observing platforms for conventional data
* change the output variable name for IODA or UFO
* change the input variable name from GSI
* add additional sensors for radiances, ozone, or AOD observations

These dictionaries are:

conv_platforms - a dictionary where the key is the type of input GSI diag file for conventional observations (conv_ps)
and the value is a list of possible output platforms (surface, sondes, etc.)
```
conv_platforms = {
    "conv_ps": [
        'sfc',
        'sondes',
        'sfcship',
    ],
    }
````
