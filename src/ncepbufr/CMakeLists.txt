list(APPEND scripts
  bufr_ncep_prepbufr_adpupa.py
  prepbufr_adpsfc_api.py
  prepbufr_sfcshp_api.py
)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${scripts}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_ncepbufr_scripts_deps)
execute_process(COMMAND chmod +x ${bin_ncepbufr_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${scripts}"
            ${CMAKE_CURRENT_SOURCE_DIR}
            ${CMAKE_BINARY_DIR}/install-bin
            install_bin_ncepbufr_scripts_deps)

install( PROGRAMS ${install_bin_ncepbufr_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_ncepbufr_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS    ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
