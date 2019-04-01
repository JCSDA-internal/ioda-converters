list( APPEND programs
  emcice2ioda.py
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  emcice2ioda.py  
  DESTINATION bin)
