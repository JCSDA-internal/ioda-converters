list(APPEND libs gsi_ncdiag.py)

list( APPEND programs
  proc_gsi_ncdiag.py
  test_gsidiag.py
  combine_conv.py
  subset_files.py)

foreach(_file IN LISTS programs)
    add_custom_target(${_file} ALL COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${_file} ${CMAKE_BINARY_DIR}/bin/${_file}
                               DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_file})
endforeach()
foreach(_file IN LISTS libs)
    add_custom_target(${_file} ALL COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_CURRENT_SOURCE_DIR}/${_file} ${CMAKE_BINARY_DIR}/lib/pyiodaconv/${_file}
                               DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${_file})
endforeach()

install(PROGRAMS ${programs} DESTINATION bin)
install(FILES ${libs} DESTINATION lib/pyiodaconv)

ecbuild_add_test( TARGET iodaconv_gsi-ncdiag_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
