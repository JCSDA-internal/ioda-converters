list( APPEND libs
  wrfda_ncdiag.py
)

list( APPEND programs
  proc_wrfda_ncdiag.py
  test_wrfdadiag.py
)

SET_TARGETS_DEPS( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   wrfda_ncdiag_scripts_deps)
add_custom_target( wrfda_ncdiag_scripts ALL DEPENDS ${wrfda_ncdiag_scripts_deps} )

set( SCRIPT_LIB_PATH ${PYIODACONV_BUILD_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_wrfda_ncdiag_scripts_deps)
add_custom_target( bin_wrfda_ncdiag_scripts ALL
    COMMAND chmod +x ${bin_wrfda_ncdiag_scripts_deps}
    DEPENDS ${bin_wrfda_ncdiag_scripts_deps} )

# Configure files for install prefix bin
set( SCRIPT_LIB_PATH ${PYIODACONV_INSTALL_LIBDIR} )
CONF_TARGETS_DEPS( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_wrfda_ncdiag_scripts_deps)
add_custom_target( install_bin_wrfda_ncdiag_scripts ALL
    COMMAND chmod +x ${install_bin_wrfda_ncdiag_scripts_deps}
    DEPENDS ${install_bin_wrfda_ncdiag_scripts_deps} )

install( PROGRAMS
  ${install_bin_wrfda_ncdiag_scripts_deps}
  DESTINATION bin
)

install( FILES
  ${wrfda_ncdiag_scripts_deps}
  DESTINATION lib/pyiodaconv
)

ecbuild_add_test( TARGET iodaconv_wrfda-ncdiag_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/iodaconv_lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
