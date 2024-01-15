# GNSS-R L2 IODA-converter

This simple converter has been developed by Spire Global UK Ltd under NASA CYGNSS ROSES-2020 proposal Grant number: 80NSSC21K1120

The converter takes as input a yaml file defining inputs for the converter such as path to the native CYGNSS L2 windspeed data files, path to output folder where to store ioda format L2 ocean windspeed files, dates for which to convert files, and data assimilation window parameters and if to take data quality control from the files into account, i.e. filter the data based on data provider quality control values. 

The code is distribited under license: Apache Licence Version 2.0
 which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
