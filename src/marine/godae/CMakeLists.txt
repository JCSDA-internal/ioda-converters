list( APPEND programs
  profile2ioda.py
  ship2ioda.py
  trak2ioda.py
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ${programs}
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_godae_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} )
