#----------------------------------------------------------------
# Generated CMake target import file for configuration "RelWithDebInfo".
#----------------------------------------------------------------

# Commands may need to know the format version.
set(CMAKE_IMPORT_FILE_VERSION 1)

# Import target "gnssro_bufr2ioda" for configuration "RelWithDebInfo"
set_property(TARGET gnssro_bufr2ioda APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(gnssro_bufr2ioda PROPERTIES
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/bin/gnssro_bufr2ioda"
  )

list(APPEND _IMPORT_CHECK_TARGETS gnssro_bufr2ioda )
list(APPEND _IMPORT_CHECK_FILES_FOR_gnssro_bufr2ioda "${_IMPORT_PREFIX}/bin/gnssro_bufr2ioda" )

# Import target "gnssro_gsidiag2ioda" for configuration "RelWithDebInfo"
set_property(TARGET gnssro_gsidiag2ioda APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(gnssro_gsidiag2ioda PROPERTIES
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/bin/gnssro_gsidiag2ioda"
  )

list(APPEND _IMPORT_CHECK_TARGETS gnssro_gsidiag2ioda )
list(APPEND _IMPORT_CHECK_FILES_FOR_gnssro_gsidiag2ioda "${_IMPORT_PREFIX}/bin/gnssro_gsidiag2ioda" )

# Import target "bufr2nc_fortran.x" for configuration "RelWithDebInfo"
set_property(TARGET bufr2nc_fortran.x APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(bufr2nc_fortran.x PROPERTIES
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/bin/bufr2nc_fortran.x"
  )

list(APPEND _IMPORT_CHECK_TARGETS bufr2nc_fortran.x )
list(APPEND _IMPORT_CHECK_FILES_FOR_bufr2nc_fortran.x "${_IMPORT_PREFIX}/bin/bufr2nc_fortran.x" )

# Import target "obserror2ioda.x" for configuration "RelWithDebInfo"
set_property(TARGET obserror2ioda.x APPEND PROPERTY IMPORTED_CONFIGURATIONS RELWITHDEBINFO)
set_target_properties(obserror2ioda.x PROPERTIES
  IMPORTED_LOCATION_RELWITHDEBINFO "${_IMPORT_PREFIX}/bin/obserror2ioda.x"
  )

list(APPEND _IMPORT_CHECK_TARGETS obserror2ioda.x )
list(APPEND _IMPORT_CHECK_FILES_FOR_obserror2ioda.x "${_IMPORT_PREFIX}/bin/obserror2ioda.x" )

# Commands beyond this point should not need to know the version.
set(CMAKE_IMPORT_FILE_VERSION)
