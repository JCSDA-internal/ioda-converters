set(IODA_OBS_SPACE_YAML "${IODA_YAML_ROOT}/validation/ObsSpace.yaml")

configure_file(
        ${CMAKE_CURRENT_SOURCE_DIR}/FilePathConfig.h.in
        ${CMAKE_BINARY_DIR}/generated/FilePathConfig.h
        @ONLY
)
