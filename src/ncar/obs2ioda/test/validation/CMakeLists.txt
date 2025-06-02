set(test_write_goes_abi_ioda_v3_SOURCES
        write_goes_abi_ioda_v3.test.f90
)
set(test_write_goes_abi_ioda_v3_LIBRARY_DEPENDENCIES
        v3
)
add_executable(test_write_goes_abi_ioda_v3 ${test_write_goes_abi_ioda_v3_SOURCES})
target_link_libraries(test_write_goes_abi_ioda_v3
        PRIVATE
        ${test_write_goes_abi_ioda_v3_LIBRARY_DEPENDENCIES}
)
