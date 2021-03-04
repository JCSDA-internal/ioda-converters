# Bufr Converter

BUFR Converter is a tool that converts BUFR file to IODA ObsGroup objects (with the ability to save 
as NetCDF if desired) where YAML files are used to describe the mapping between the input and the 
output.

## Configuration

```yaml
observations:
  - obs space:
        name: bufr
        # Describe BUFR data to read
        
        exports:
        # Describe how to export data to IODA encoder

    ioda:
        # Describe how to make Obs group objects from exported data
```

The configuration files map the input BUFR format to the output which are ObsGroup objects, or 
NetCDF files if you'd rather think of it that way. At a high level it consists of `observations` 
where each observation contains an `obs space` section that describes the input BUFR file to parse
and an `ioda` section that describes the output object we want to create.

### Obs Space

The obs space describes how to read data from the BUFR file and then how to expose that data to the
`ioda` encoder object.

#### BUFR Data Description

```yaml
      name: bufr
      obsdatain: "./testinput/gdas.t18z.1bmhs.tm00.bufr_d"
      isWmoFormat: true  # Optional
      tablepath: "./testinput/bufr_tables"  # Optional
        
      mnemonicSets:
        - mnemonics: [SAID, FOVN, YEAR, MNTH, DAYS, HOUR, MINU, SECO, CLAT, CLON, CLATH, CLONH, HOLS]
        - mnemonics: [SAZA, SOZA, BEARAZ, SOLAZI]
        - mnemonics: [TMBR]
          channels : 1-5
```

Defines how to read data from the input BUFR file. Its sections are as follows:

* `name` ID of input type
* `obsdatain` Relative path of the BUFR file to ingest (relative to working directory).
* `isWmoFormat` _(optional)_ Bool value that indicates whether the bufr file is in the standard WMO 
   format (BUFR table data is not included in the message and must be loaded seperatly). Defaults
   to false if missing.
* `tablepath` _(optional)_ Path string to the directory that contains the bufr tables when using 
   standard WMO formated files. Only applies if `isWmoFormat` is `true`. If this field is missing 
   and`isWmoFormat` is `true` then NCEPLib-bufr will look for the table data in its default
   directory.
* `mnemonicSets` Defines the list of mnemonic sets to read from the BUFR file.
  * `mnemonics` Defines a group of mnemonics to parse from a BUFR subset. 
    * _(optional)_ `channels` specifies channels to capture. This could be a disjoint set such as 
      “1, 3, 8-15” or just "1-5" as in the example.
    * Internally _NCEPLib BUFR_ **ufbint** used for single value mnemonics, and **ufbrep** used for 
      multi channels mnemonics.

#### Exports

```yaml
      exports:
        splits:
          satId:
           category:
              mnemonic: SAID
              map: 
                _3: sat_1  # can't use integers as keys
                _5: sat_2
                _8: sat_3
                
        filters:
          - bounding:
              mnemonic: CLON
              upperBound: -68  # optional
              lowerBound: -86.3  # optional

        variables:
          timestamp:
            datetime:
              year: YEAR
              month: MNTH
              day: DAYS
              hour: HOUR
              minute: MINU
              second: SECO
              hoursFromUtc: 0  # optional
          longitude:
            mnemonic: CLON
            transforms:
              - offset: -180
          latitude:
            mnemonic: CLAT
          radiance:
            mnemonic: TMBR
```
Exports is a dictionary of key value pairs which define a name to the data element to expose the 
ioda encoder. It has the following sections:


* `variables`
  * **keys** are arbitrary strings (anything you want). They can be referenced in the ioda section.
  * **values** (One of these types):
    * `mnemonic` Associate **key** with data for mnemonic listed in mnemonic set. _(optional)_ Can 
      apply a list of `tranforms` to the data. Possible transforms are `offset` and `scale`.
    * `datetime` Associate **key** with datetime formatted strings. Supply mnemonics for `year`, 
      `month`, `day`, `hour`, `minute`, _(optional)_ `second`, and _(optional)_ `hoursFromUtc` (must
      be an **integer**).
      

* _(optional)_ `splits` List of key value pair (splits) that define how to split the data into 
  subsets of data. Any number of splits can be applied. Possible categories within each split will 
  be combined to form sets which describe all unique combinations of those categories. For example 
  the splits with categories ("a", "b") and ("x", "y") will be combined into four split categories 
  ("a", "x"), ("a", "y"), ("b", "x"), ("b", "y").
  * **keys** are arbitrary strings (anything you want). They can be referenced in the ioda section.
  * **values** Type of split to apply (currently supports `category`)
    * `category` Splits data based on values assocatied with a BUFR mnemonic. Constists of:
      * `mnemonic` The mnemonic to use.
      * _(optional)_ `map` Associates integer values in BUFR mnemonic data to a string. Please not 
        that integer keys must be prepended with an `_` (ex: `_2`). Rows where where the mnemonic 
        value is not defined in the map will be rejected (won't appear in output).
  

* _(optional)_ `filters`List of filters to apply to the data before exporting. Filters exclude data
  which does not meet their requirements. The following filters are supported:
    * `bounding`
      * `mnemonic` The mnemonic to use.
      * _(optional)_ `upperBound` The highest possible value to accept
      * _(optional)_ `lowerBound` The lowest possible value to accept
  
    _note: either `upperBound`, `lowerBound`, or both must be present._
        

### Ioda

```yaml
    ioda:
      backend: netcdf
      obsdataout: "./testrun/gdas.t00z.1bhrs4.tm00.{splits/satId}.nc"

      dimensions:
        - name: "nlocs"
          size: "variables/radiance.nrows"
        - name: "nchans"
          size: "variables/radiance.ncols"

      variables:
        - name: "datetime@MetaData"
          source: "variables/timestamp"
          dimensions: [ "nlocs" ]
          longName: "Datetime"
          units: "datetime"

        - name: "latitude@MetaData"
          source: "variables/latitude"
          dimensions: ["nlocs"]
          longName: "Latitude"
          units: "degrees_north"
          range: [-90, 90]

        - name: "longitude@MetaData"
          source: "variables/longitude"
          dimensions: ["nlocs"]
          longName: "Longitude"
          units: "degrees_east"
          range: [-180, 180]

        - name: "radiance@ObsValue"
          coordinates: "longitude latitude nchans"
          source: "variables/radiance"
          dimensions: ["nlocs", "nchans"]
          longName: "Radiance"
          units: "K"
          range: [120, 500]
          chunks: [1000, 15]
          compressionLevel: 4
```

The `ioda` section defines the ObsGroup objects that will be created. 

* `backend` can be `inmemory` or `netcdf`.
* `obsdataout` required for “netcdf” backend. Should be a templated string for example: 
  **./testrun/gdas.t00z.1bhrs4.tm00.{splits/satId}.nc**. Substrings such as **{splits/satId}** are 
  replaced with the relevant split category ID for that file to form a unique name for every file.
* `dimensions` used to define dimension information in variables
    * `name` arbitrary name for the dimension
    * `size` can be either a integer or a reference to exported data ex: 
      **variables/radiance.nrows**
* `variables` List of output variable objects to create.
  * `name` standardized pathname **var_name**@**group**. 
    * **var_name** name for the variable
    * **group** group name to which this variable belongs.
  * `source` reference to exported data ex: **variables/radiance**
  * `coordinates` (optional):
  * `dimensions` list of ids defined in dimensions section
  * `longName`any arbitrary string.
  * `units` tring representing units (arbitrary).
  * _(optional)_ `range` Possible range of values (list of 2 ints).
  * _(optional)_ `chunks`Size of chunked data elements ex: `[1000, 1000]`.
  * _(optional)_ `compressionLevel` GZip compression level (0-9).
  
