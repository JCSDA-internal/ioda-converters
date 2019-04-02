list( APPEND programs
  gds2_sst2ioda.py )

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install ( PROGRAMS
  ${programs}
  DESTINATION bin)
