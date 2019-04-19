list( APPEND programs
    lint.sh
)

# "build" each script for testing purposes. The idea is to walk through the
# ${programs} list and trigger a copy to the build bin directory when a script
# if modified.
foreach( script ${programs} )
  set( source_file ${CMAKE_CURRENT_SOURCE_DIR}/${script} )
  set( dest_file ${CMAKE_BINARY_DIR}/bin/${script} )
  list( APPEND tools_scripts_deps ${dest_file} )
  add_custom_command (
    OUTPUT ${dest_file}
    DEPENDS ${source_file}
    COMMAND cp ${source_file} ${dest_file}
    COMMAND chmod +x ${dest_file}
  )
endforeach( script )
add_custom_target( tools_scripts ALL DEPENDS ${tools_scripts_deps} )

install (PROGRAMS
  lint.sh
  DESTINATION bin)
