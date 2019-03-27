add_subdirectory(altimeter)
add_subdirectory(godae)
add_subdirectory(hgodas)
add_subdirectory(gmao)

ecbuild_add_test( TARGET iodaconv_altimeter_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR}/altimeter ${CMAKE_SOURCE_DIR} )

ecbuild_add_test( TARGET iodaconv_godae_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR}/godae ${CMAKE_SOURCE_DIR} )

ecbuild_add_test( TARGET iodaconv_hgodas_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR}/hgodas ${CMAKE_SOURCE_DIR} )

ecbuild_add_test( TARGET iodaconv_gmao_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR}/gmao ${CMAKE_SOURCE_DIR} )
