list( APPEND PROGRAMS
  cat_nc_files.py
  select_nc_obs.py
  prep_nc_files.py
  list_sid_raob.py
  split_conv.py
  run_prep_nc_files.sh
  run_select_nc_obs.sh
  DESTINATION bin)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ${programs}
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_gsi-ncdiag_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR} )
