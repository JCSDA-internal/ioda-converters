
if( iodaconv_bufr_ENABLED )
    list( APPEND satwnd_files
        run_satwnds.py
        satwnds_new_subset_template.yaml
        satwnds_old_subset_template.yaml
    )

    set_targets_deps( "${satwnd_files}"
                       ${CMAKE_CURRENT_SOURCE_DIR}
                       ${CMAKE_BINARY_DIR}/bin
                       run_satwnds_deps)
endif()
