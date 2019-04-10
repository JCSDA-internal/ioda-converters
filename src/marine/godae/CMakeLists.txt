list( APPEND programs
  godae_profile2ioda.py
  godae_ship2ioda.py
  godae_trak2ioda.py
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ${programs}
  DESTINATION bin)
