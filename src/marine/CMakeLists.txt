add_subdirectory(altimeter)
add_subdirectory(godae)

ecbuild_add_test( TARGET iodaconv_altimeter_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR}/godae ${CMAKE_SOURCE_DIR} )

ecbuild_add_test( TARGET iodaconv_godae_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR}/godae ${CMAKE_SOURCE_DIR} )
