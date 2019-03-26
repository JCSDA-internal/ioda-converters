list(APPEND programs
  hgodas-insitu2ioda.py
  hgodas-sst2ioda.py
  hgodas-adt2ioda.py
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ${programs}
  DESTINATION bin)
