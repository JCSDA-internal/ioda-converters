#!/usr/bin/python3

import csv
import argparse

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
              drawVariableName, drawVariableType, drawVariableColumn)
    with open(csvFileName, 'w') as csvFile:
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
        with open(coefFileName, 'r') as coefFile:
            for line in coefFile:
                cwrite.writerow([line.split()[coordVariableColumn].ljust(8,' '), line.split()[drawVariableColumn]])
    return
#
# begin
#
if __name__ == "__main__":
    # generate an argument parser to accept command-line inputs:
    #    coefFileName = file name containing GDAS aircraft bias correction coefficients
    #    csvFileName = file name to write CSV file to (will clobber)
    #    coordGrpVar = group/variable name of coordinate variable in CSV file
    #    coordVarTyp = variable-type of coordinate variable
    #    coordColumn = column of coef. file containing coordinate variable
    #    drawGrpVar = gropu/variable name of returned draw variable in CSV file
    #    drawVarTyp = variable-type of returned draw variable
    #    drawColumn = column of coef. file containing returned draw variable
    parser = argparse.ArgumentParser(description='define input coefficient file, output CSV file, and (group/variable, variable-type, column-number) of coordinate and draw variables')
    parser.add_argument('coefFileName', metavar='INFILE', type=str, help='name of input bias coefficient file')
    parser.add_argument('csvFileName', metavar='OUTFILE', type=str, help='name of output CSV file')
    parser.add_argument('coordGrpVar', metavar='group/var coordinate', type=str, help='group/variable name of coordinate variable')
    parser.add_argument('coordVarTyp', metavar='var-type coordinate', type=str, help='variable-type of coordinate variable')
    parser.add_argument('coordColumn', metavar='column coordinate', type=int, help='column-number of coordinate variable')
    parser.add_argument('drawGrpVar', metavar='group/var draw', type=str, help='group/variable name of draw variable')
    parser.add_argument('drawVarTyp', metavar='var-type draw', type=str, help='variable-type name of draw variable')
    parser.add_argument('drawColumn', metavar='column draw', type=int, help='column-number of draw variable')
    # parse command-line inputs
    commandInputs = parser.parse_args()
    # read from coefFileName, write predictor coefficients to csvFileName file
    csv_write(commandInputs.csvFileName, commandInputs.coefFileName,
              commandInputs.coordGrpVar, commandInputs.coordVarTyp,  commandInputs.coordColumn,
              commandInputs.drawGrpVar,  commandInputs.drawVarTyp,   commandInputs.drawColumn) 
#
# end
#
