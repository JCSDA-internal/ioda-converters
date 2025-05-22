set(IODA_SCHEMA_YAML "${CMAKE_SOURCE_DIR}/share/ObsSpace.yaml")

if(NOT EXISTS "${IODA_SCHEMA_YAML}")
    message(FATAL_ERROR "Required file not found: ${IODA_SCHEMA_YAML}")
endif()

configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/FilePathConfig.h.in
        ${CMAKE_BINARY_DIR}/generated/FilePathConfig.h
        @ONLY
)

set(OBS2IODA_V3_EXECUTABLE_PATH "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/obs2ioda_v3")

configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/FilePathConfig.py.in
        ${CMAKE_SOURCE_DIR}/test/validation/FilePathConfig.py
        @ONLY
)

set(VALIDATION_TEST_DIRECTORY "${CMAKE_SOURCE_DIR}/test/validation")

configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/pytest.ini.in
        "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/../pytest.ini"
        @ONLY
)
