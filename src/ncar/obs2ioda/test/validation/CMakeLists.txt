set(test_write_goes_abi_ioda_v3_SOURCES
        write_goes_abi_ioda_v3.test.f90
)
set(test_write_goes_abi_ioda_v3_LIBRARY_DEPENDENCIES
        v3
)
add_executable(test_write_goes_abi_ioda_v3 ${test_write_goes_abi_ioda_v3_SOURCES})
set_target_properties(test_write_goes_abi_ioda_v3 PROPERTIES LINKER_LANGUAGE Fortran)
target_link_libraries(test_write_goes_abi_ioda_v3
        PRIVATE
        ${test_write_goes_abi_ioda_v3_LIBRARY_DEPENDENCIES}
)
