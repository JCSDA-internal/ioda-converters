set(IODA_SCHEMA_YAML "${CMAKE_SOURCE_DIR}/share/ObsSpace.yaml")

if(NOT EXISTS "${IODA_SCHEMA_YAML}")
    message(FATAL_ERROR "Required file not found: ${IODA_SCHEMA_YAML}")
endif()

configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/FilePathConfig.h.in
        ${CMAKE_BINARY_DIR}/generated/FilePathConfig.h
        @ONLY
)
