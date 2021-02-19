# GSI Satbias Converter

GSI Satbias Converter is a tool that converts GSI files with bias correction coefficients (satbias)
to the NetCDF files written using IODA ObsGroup capability. One NetCDF file is created for each
sensor.

## Configuration

The configuration file specifies which sensors need to be processed from the input GSI satbias files,
the output file names and the names of the predictors for the new file.
An example of a configuration file can be found in `ioda_converters/test/testinput/satbias_converter.yaml`:

```yaml
input file: satbias_crtm_in     # input file name
output:
- sensor: amsua_n15             # name of sensor as written in GSI satbias_in file
  output file: satbias_amsua_n15.nc4  # output file name
  predictors: *default_preds          # list of predictors (use default predictors for amsu*/mhs/atms)
- sensor: amsua_n19
  output file: satbias_amsua_n19.nc4
  predictors: *default_preds
```

According to [GSI code (as of Feb 19 2021)](https://github.com/NOAA-EMC/GSI/blob/a9447258bb911d73ded0331141d0bf56abdfd74d/src/gsi/setuprad.f90#L992)),
the following predictors should be used for most sensors:

```yaml
      default predictors: &default_preds
      - constant
      - zenith_angle
      - cloud_liquid_water
      - lapse_rate_order_2
      - lapse_rate
      - cosine_of_latitude_times_orbit_node
      - sine_of_latitude
      - emissivity
      - scan_angle_order_4
      - scan_angle_order_3
      - scan_angle_order_2
      - scan_angle
```

For the following sensors:

```f90
     goessndr .or. goes_img .or. ahi .or. seviri .or. ssmi .or. ssmis .or. gmi .or. abi
```

use these predictors:

```yaml
      scanpos predictors: &scanpos_preds
      - constant
      - zenith_angle
      - cloud_liquid_water
      - lapse_rate_order_2
      - lapse_rate
      - cosine_of_latitude_times_orbit_node
      - sine_of_latitude
      - emissivity
      - scan_position_order_4
      - scan_position_order_3
      - scan_position_order_2
      - scan_position
```

## Running the converter

To convert GSI bias correction coefficient files (satbias), run `satbias2ioda.x` executable, passing it a yaml configuration file:

```bash
satbias2ioda.x satbias_converter.yaml
```
