list( APPEND programs
  ioda_conv_ncio.py
  collect_sources.py
  run-pyflakes.py
  run-mccabe.py
)

# "build" each script for testing purposes. The idea is to walk through the
# ${programs} list and trigger a copy to the build bin directory when a script
# if modified.
foreach( script ${programs} )
  set( source_file ${CMAKE_CURRENT_SOURCE_DIR}/${script} )
  set( dest_file ${CMAKE_BINARY_DIR}/bin/${script} )
  list( APPEND lib_python_scripts_deps ${dest_file} )
  add_custom_command (
    OUTPUT ${dest_file}
    DEPENDS ${source_file}
    COMMAND cp ${source_file} ${dest_file}
    COMMAND chmod +x ${dest_file}
  )
endforeach( script )
add_custom_target( lib_python_scripts ALL DEPENDS ${lib_python_scripts_deps} )

install (PROGRAMS
  ioda_conv_ncio.py
  collect_sources.py
  run-pyflakes.py
  run-mccabe.py
  DESTINATION bin)

ecbuild_add_test( TARGET iodaconv_lib-python_coding_norms
                  TYPE SCRIPT
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR} )
