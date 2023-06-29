# (C) Copyright 2023 NOAA/NWS/NCEP/EMC
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

import csv

DATA_PATH = './testinput/'
OUTPUT_PATH = './testrun/'

# csv_write: fill a CSV file with coordinate-variable and draw-variable data for JEDI DrawValueFromFile
#
# INPUTS
#
# csvFileName: name of a write-allowable file for CSV output, will be generated or clobbered (string)
# coefFileName: name of a read-allowable space-delimited GDAS bias correction file (string)
# coordVariableName: <Group>/<name> of coordinate variable (string)
# coordVariableType: variable-type for coordinate variable (string)
# coordVariableColumn: column-number for coordinate variable in coefFile (integer)
# drawVariableName: <Group>/<name> of draw variable (string)
# drawVariableType: variable-type for draw variable (string)
# drawVariableColumn: column-number for draw variable in coefFile (integer)
#
# OUTPUTS
#
# no returned variables, but CSV file is properly written and formatted for JEDI DrawValueFromFile
#
# DEPENDENCIES
#
# csv
#
def csv_write(csvFileName, coefFileName, coordVariableName, coordVariableType, coordVariableColumn,
              drawVariableName, drawVariableType, drawVariableColumn):
    import csv
    csvFile = open(csvFileName, 'w')
    cwrite = csv.writer(csvFile)
    # write 1st-line header for coordVariableName, drawVariableName
    cwrite.writerow([coordVariableName, drawVariableName])
    # write 2nd-line header for coordVariableType, drawVariableType
    cwrite.writerow([coordVariableType, drawVariableType])
    # write first data line for missing value placeholder '_' to set draw for missing coordinates to a
    # selected missing value. For aircraft bias correction, setting these coefficient values to 0. will
    # effectively skip bias correction for these data.
    cwrite.writerow(['_', '0.'])
    # write remaining lines from coefFile columns specified by coordVariableColumn, drawVariableColumn
    # NOTE: represent a tail-number coordinate variable as a left-justified space-padded string by
    #       using the str.ljust() command
    coefFile = open(coefFileName, 'r')
    for line in coefFile:
        cwrite.writerow([line.split()[coordVariableColumn].ljust(8,' '), line.split()[drawVariableColumn]])
    return
    # close files
    csvFile.close()
    coefFileName.close()
#
# begin
#
if __name__ == "__main__":
    # define GDAS aircraft bias file name for read-only
    coefFileName = DATA_PATH + 'gdas.t18z.abias_air'
    # define CSV file names for constant, ascent, and ascent-squared predictor coefficients for write-only (will clobber)
#    constantFileName = OUTPUT_PATH + 'gdas.t18z.abias_air_constant.csv'
    ascentFileName = OUTPUT_PATH + 'gdas.t18z.abias_air_ascent.csv'
#    ascentSquaredFileName = OUTPUT_PATH + 'gdas.t18z.abias_air_ascentSquared.csv'
    # write constant predictor coefficients to c
#    csv_write(constantFileName, coefFileName,
#              'MetaData/stationIdentification',         'string', 0,
#              'BiasCoefficientValue/constantPredictor', 'float',  2)
    # write ascent predictor coefficients to a
    csv_write(ascentFileName, coefFileName,
              'MetaData/stationIdentification',         'string', 0,
              'BiasCoefficientValue/ascentPredictor',   'float',  3)
    # write ascentSquared predictor coefficients to a2
#    csv_write(ascentSquaredFileName, coefFileName,
#              'MetaData/stationIdentification',              'string', 0,
#              'BiasCoefficientValue/ascentSquaredPredictor', 'float',  4) 
#
# end
#
