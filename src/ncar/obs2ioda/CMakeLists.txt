set(IODA_OBS_SPACE_YAML "${IODA_YAML_ROOT}/validation/ObsSpace.yaml")
set(BUILD_GOES_ABI_CONVERTER OFF CACHE BOOL "Build the GOES ABI converter")
add_subdirectory("${CMAKE_CURRENT_SOURCE_DIR}/src")

