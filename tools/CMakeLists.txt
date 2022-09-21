list( APPEND programs
    ${PROJECT_NAME}_lint.sh
    ${PROJECT_NAME}_comp.sh
    ${PROJECT_NAME}_cpplint.py
)

set_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   tools_scripts_deps)

add_subdirectory( fortran )
add_subdirectory( run_satwnds )
add_subdirectory( bufr )
