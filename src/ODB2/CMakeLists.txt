list( APPEND libs
  var_convert.py)

list( APPEND programs
  odbapi2nc.py
  odbapi2json.py)

list( APPEND configs
  MO_aircraft_16026_Definition.yaml
  MO_aircraft_16029_Definition.yaml
  MO_amsua_Definition.yaml
  MO_radiosonde_Definition.yaml
  era5_16013_16015_Definition.yaml
  era5_16022_16045_Definition.yaml
  era5_aircraft_Definition.yaml )

set ( ODB2_PYCODESTYLE_CONFIG ${CMAKE_CURRENT_SOURCE_DIR} )

set_targets_deps( "${libs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${PYIODACONV_BUILD_LIBDIR}
                   odb2_scripts_deps)

file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_BINARY_DIR}/bin ${PYIODACONV_BUILD_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   bin_odb2_scripts_deps)
execute_process(COMMAND chmod +x ${bin_odb2_scripts_deps})

# Configure files for install prefix bin
file( RELATIVE_PATH SCRIPT_LIB_PATH ${CMAKE_INSTALL_FULL_BINDIR} ${PYIODACONV_INSTALL_LIBDIR} )
conf_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/install-bin
                   install_bin_odb2_scripts_deps)

# ODB "definition" files which are configurations for the odbapi2nc.py script.
copy_files( "${configs}"
            ${CMAKE_CURRENT_SOURCE_DIR}/Definitions
            ${CMAKE_BINARY_DIR}/etc/pyiodaconv )

install( PROGRAMS ${install_bin_odb2_scripts_deps} DESTINATION ${CMAKE_INSTALL_BINDIR} )

install( FILES ${odb2_scripts_deps} DESTINATION ${CMAKE_INSTALL_LIBDIR}/pyiodaconv )

# ODB "definition" files which are configurations for the odbapi2nc.py script.
foreach (FILENAME ${configs})
  list( APPEND build_area_configs ${CMAKE_BINARY_DIR}/etc/pyiodaconv/${FILENAME} )
endforeach(FILENAME)

install( FILES ${build_area_configs} DESTINATION ${CMAKE_INSTALL_DATADIR}/pyiodaconv )

add_test(NAME iodaconv_odb2_coding_norms
         COMMAND ${CMAKE_BINARY_DIR}/bin/${PROJECT_NAME}_lint.sh
                 ${CMAKE_CURRENT_SOURCE_DIR} ${ODB2_PYCODESTYLE_CONFIG})
