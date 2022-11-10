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

#### Exports

```yaml
      exports:
        group_by_variable: longitude  # Optional
        subsets:
          - NC004001
          - NC004002
          - NC004003
        variables:
          timestamp:
            datetime:
              year: "*/YEAR"
              month: "*/MNTH"
              day: "*/DAYS"
              hour: "*/HOUR"
              minute: "*/MINU"
              second: "*/SECO"  # default assumed zero if skipped or found as missing
              hoursFromUtc: 0  # Optional

          # Or, sometimes BUFR data use an offset time related to model analysis/cycle.
          timestamp:
            timeoffset:
              timeOffset: "*/PRSLEVEL/DRFTINFO/HRDR"
              transforms:
                - scale: 3600
              referenceTime: "2020-11-01T12:00:00Z"

          satellite_id:
            query: "*/SAID"
            type: int64
          longitude:
            query: "*/CLON"
            transforms:
              - offset: -180
          latitude:
            query: "*/CLAT"
          channels:
            query: "[*/BRITCSTC/CHNM, */BRIT/CHNM]"
          radiance:
            query: "[*/BRITCSTC/TMBR, */BRIT/TMBR]"

        splits:
          satId:
            category:
              variable: satellite_id
              map:
                _3: sat_1  # can't use integers as keys
                _5: sat_2
                _8: sat_3

        filters:
          - bounding:
              variable: longitude
              upperBound: -68  # optional
              lowerBound: -86.3  # optional
```
Exports is a dictionary of key value pairs which define a name to the data element to expose the 
ioda encoder. It has the following sections:

* `group_by_variable` _(optional)_ String value that defines the name of the variable to group 
   observations by. If this field is missing then observations will not be grouped.
* `subsets` _(optional)_ List of subsets that you want to process. If the field is not present then
   all subsets will be processed in accordance with the query definitions.
* `variables`
  * **keys** are arbitrary strings (anything you want). They can be referenced in the ioda section.
  * **values** (One of these types):
    * `query` Query string which is used to get the data from the BUFR file. _(optional)_ Can 
      apply a list of `tranforms` to the numeric (not string) data. Possible transforms are 
      `offset` and `scale`. You can also manually override the type by specifying the `type` as 
      **int**, **int64**, **float**, or **double**.
    * `datetime` Associate **key** with data for mnemonics for `year`, `month`, `day`, `hour`,
      `minute`, _(optional)_ `second`, and _(optional)_ `hoursFromUtc` (must be an **integer**).
      Internally, the value stored is number of seconds elapsed since a reference epoch, currently
      set to 1970-01-01T00:00:00Z.
    * `timeoffset` Associate **key** with data for mnemonic for `timeOffset`, that should result
      in seconds relative to an ISO-8601 string of date and time (e.g., `2020-11-01T11:42:56Z`).
      If the timeOffset mnemonic is a floating-point value in hours, then simply use **transforms**
      and scale by 3600 seconds.  Internally, the value stored is number of seconds elapsed since
      a reference epoch, currently set to 1970-01-01T00:00:00Z.
      

* _(optional)_ `splits` List of key value pair (splits) that define how to split the data into 
  subsets of data. Any number of splits can be applied. Possible categories within each split will 
  be combined to form sets which describe all unique combinations of those categories. For example 
  the splits with categories ("a", "b") and ("x", "y") will be combined into four split categories 
  ("a", "x"), ("a", "y"), ("b", "x"), ("b", "y").
  * **keys** are arbitrary strings (anything you want). They can be referenced in the ioda section.
  * **values** Type of split to apply (currently supports `category`)
    * `category` Splits data based on values assocatied with a BUFR mnemonic. Constists of:
      * `variable` The variable from the `variables` section to split on.
      * _(optional)_ `map` Associates integer values in BUFR mnemonic data to a string. Please not 
        that integer keys must be prepended with an `_` (ex: `_2`). Rows where where the mnemonic 
        value is not defined in the map will be rejected (won't appear in output).
  

* _(optional)_ `filters`List of filters to apply to the data before exporting. Filters exclude data
  which does not meet their requirements. The following filters are supported:
    * `bounding`
      * `variable` The variable from the `variables` section to filter on.
      * _(optional)_ `upperBound` The highest possible value to accept
      * _(optional)_ `lowerBound` The lowest possible value to accept
  
    _note: either `upperBound`, `lowerBound`, or both must be present._
        

### Ioda

```yaml
    ioda:
      backend: netcdf
      obsdataout: "./testrun/gdas.t00z.1bhrs4.tm00.{splits/satId}.nc"

      dimensions:
        - name: nchans
          paths:
            - "*/BRIT"
            - "*/BRITCSTC"
          source: variables/channels

      variables:
        - name: "MetaData/dateTime"
          source: "variables/timestamp"
          longName: "dateTime"
          units: "seconds since 1970-01-01T00:00:00Z"

        - name: "MetaData/latitude"
          source: "variables/latitude"
          longName: "Latitude"
          units: "degrees_north"
          range: [-90, 90]

        - name: "MetaData/longitude"
          source: "variables/longitude"
          longName: "Longitude"
          units: "degrees_east"
          range: [-180, 180]

        - name: "ObsValue/radiance"
          coordinates: "longitude latitude nchans"
          source: "variables/radiance"
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
    * `paths` list of subqueries for that dimension (different paths for different BUFR subsets 
              only) **or** `path` Single subquery for that dimension ex: **\*/BRITCSTC**
    * `source` (optional) The exported data that acts as the source field for this dimension. 
               The data dimension values (labels) will reflect this field. The source is validated
               to make sure it makes sense for the dimension and that it is made up of repeated
               values for each occurrence of the sequence. The source field must be inside the
               dimension and be 1:1 with it.
* `variables` List of output variable objects to create.
  * `name` standardized pathname **group**/**var_name**. 
    * **var_name** name for the variable
    * **group** group name to which this variable belongs.
  * `source` reference to exported data ex: **variables/radiance**
  * `coordinates` (optional):
  * `longName`any arbitrary string.
  * `units` string representing units (arbitrary but following udunits).
  * _(optional)_ `range` Possible range of values (list of 2 ints).
  * _(optional)_ `chunks`Size of chunked data elements ex: `[1000, 1000]`.
  * _(optional)_ `compressionLevel` GZip compression level (0-9).
  
