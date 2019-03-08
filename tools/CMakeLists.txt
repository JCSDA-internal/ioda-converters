list( APPEND programs
    lint.sh
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  lint.sh
  DESTINATION bin)
