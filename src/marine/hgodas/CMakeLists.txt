list(APPEND programs
  hgodas_insitu2ioda.py
  hgodas_sst2ioda.py
  hgodas_adt2ioda.py
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ${programs}
  DESTINATION bin)
