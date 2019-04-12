list( APPEND programs
  conv.py
  radiances.py
  composition.py 
  conv_dicts.py
  rad_dicts.py
  aod_dicts.py
  oz_dicts.py
  proc_gsi_ncdiag.py 
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ${programs}
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_gsi-ncdiag_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR} )
