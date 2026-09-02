list(APPEND programs
  altimeter_spacebased_tec_madrigal2ioda.py
  gnss_gridded_ground_tec_madrigal2ioda.py
  gnss_los_ground_tec_json2ioda.py
  gnss_los_ground_tec_madrigal2ioda.py
  gnss_tec_spacebased_cdaac2ioda.py
  gnss_tec_ground_based.py
  ionosonde_ascii2ioda.py
  gold_tdisk2ioda.py
)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_space_weather_scripts_deps)

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_space_weather_scripts_deps)

install( PROGRAMS ${install_bin_space_weather_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

ecbuild_add_test( TARGET  ${PROJECT_NAME}_space_weather_coding_norms
                  TYPE    SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                  ARGS    ${CMAKE_CURRENT_SOURCE_DIR} ${IODACONV_PYLINT_CFG_DIR} )
