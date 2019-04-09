list( APPEND programs
  ioda_conv_ncio.py
  collect_sources.py
  run-pyflakes.py
  run-mccabe.py
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ioda_conv_ncio.py
  collect_sources.py
  run-pyflakes.py
  run-mccabe.py
  DESTINATION bin)
