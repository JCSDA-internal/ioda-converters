# (C) Copyright 2019-2020 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# IODA-CONVERTER fixed files
################################################################################

list( APPEND bufr_obs_samples
  bufr_obs_samples/1bamua.bufr.subset.txt
  bufr_obs_samples/adpupa.bufr.subset.txt
  bufr_obs_samples/adpupa.prepbufr.subset.txt
  bufr_obs_samples/aircraft.bufr.subset.txt
  bufr_obs_samples/aircraft.prepbufr.subset.txt
  bufr_obs_samples/aod.bufr.subset.txt
  bufr_obs_samples/gpsro.bufr.subset.txt
)

list( APPEND bufr_table_samples
  bufr_table_samples/1bamua.bufr.table.txt
  bufr_table_samples/adpupa.bufr.table.txt
  bufr_table_samples/adpupa.prepbufr.table.txt
  bufr_table_samples/aircraft.bufr.table.txt
  bufr_table_samples/aircraft.prepbufr.table.txt
  bufr_table_samples/aod.bufr.table.txt
  bufr_table_samples/gpsro.bufr.table.txt
)

list( APPEND fix
  fix/ghcn-stations.txt
)


# create doc directories and make links to the doc fix files
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bufr_obs_samples)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/bufr_table_samples)
file(MAKE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fix)
foreach(FILENAME ${bufr_obs_samples} ${bufr_table_samples} ${fix})
    execute_process( COMMAND ${CMAKE_COMMAND} -E create_symlink
           ${CMAKE_CURRENT_SOURCE_DIR}/${FILENAME}
           ${CMAKE_CURRENT_BINARY_DIR}/${FILENAME} )
endforeach(FILENAME)

list( APPEND all_files
  ${fix}
  ${bufr_obs_samples}
  ${bufr_table_samples}
)

install( FILES ${all_files} DESTINATION doc )
