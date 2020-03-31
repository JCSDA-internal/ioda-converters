list(APPEND libs gsi_ncdiag.py)

list( APPEND programs
  proc_gsi_ncdiag.py
  test_gsidiag.py
  combine_conv.py
  subset_files.py)

# SET_TARGETS_DEPS( "${libs}"
#                    ${CMAKE_CURRENT_SOURCE_DIR}
#                    ${PYIODACONV_BUILD_LIBDIR}
#                    gsi_ncdiag_scripts_deps)
# add_custom_target( gsi_ncdiag_scripts ALL DEPENDS ${gsi_ncdiag_scripts_deps} )
#
# set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
# CONF_TARGETS_DEPS( "${programs}"
#                    ${CMAKE_CURRENT_SOURCE_DIR}
#                    ${CMAKE_BINARY_DIR}/bin
#                    bin_gsi_ncdiag_scripts_deps)
# add_custom_target( bin_gsi_ncdiag_scripts ALL
#     COMMAND chmod +x ${bin_gsi_ncdiag_scripts_deps}
#     DEPENDS ${bin_gsi_ncdiag_scripts_deps} )

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
