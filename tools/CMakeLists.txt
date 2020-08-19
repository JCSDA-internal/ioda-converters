list( APPEND programs
    ${PROJECT_NAME}_lint.sh
    ${PROJECT_NAME}_comp.sh
)

set_targets_deps( "${programs}"
                   ${CMAKE_CURRENT_SOURCE_DIR}
                   ${CMAKE_BINARY_DIR}/bin
                   tools_scripts_deps)
