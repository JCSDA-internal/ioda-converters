set(test_transpose_and_flatten_SOURCES
        transpose_and_flatten.test.f90
)
set(test_transpose_and_flatten_LIBRARY_DEPENDENCIES
        v3
)
add_fortran_ctest(test_transpose_and_flatten
        ${test_transpose_and_flatten_SOURCES}
        ${test_transpose_and_flatten_LIBRARY_DEPENDENCIES}
)

set(test_get_julian_time_SOURCES
        get_julian_time.test.f90
)
set(test_get_julian_time_LIBRARY_DEPENDENCIES
        v3
)
add_fortran_ctest(test_get_julian_time
        ${test_get_julian_time_SOURCES}
        ${test_get_julian_time_LIBRARY_DEPENDENCIES}
)
set(test_to_lower_SOURCES
        to_lower.test.f90
)
set(test_to_lower_LIBRARY_DEPENDENCIES
        v3
)
add_fortran_ctest(test_to_lower
        ${test_to_lower_SOURCES}
        ${test_to_lower_LIBRARY_DEPENDENCIES}
)

set(test_set_goes_abi_out_fname_SOURCES
        set_goes_abi_out_fname.test.f90
)
set(test_set_goes_abi_out_fname_LIBRARY_DEPENDENCIES
        v3
)
add_fortran_ctest(test_set_goes_abi_out_fname
        ${test_set_goes_abi_out_fname_SOURCES}
        ${test_set_goes_abi_out_fname_LIBRARY_DEPENDENCIES}
)

set(test_compute_bt_superob_SOURCES
        compute_bt_superob.test.f90
)
set(test_compute_bt_superob_LIBRARY_DEPENDENCIES
        v3
)
add_fortran_ctest(test_compute_bt_superob
        ${test_compute_bt_superob_SOURCES}
        ${test_compute_bt_superob_LIBRARY_DEPENDENCIES}
)

set(test_calc_solar_zenith_angle_SOURCES
        calc_solar_zenith_angle.test.f90
)
set(test_calc_solar_zenith_angle_LIBRARY_DEPENDENCIES
        v3
)
add_fortran_ctest(test_calc_solar_zenith_angle
        ${test_calc_solar_zenith_angle_SOURCES}
        ${test_calc_solar_zenith_angle_LIBRARY_DEPENDENCIES}
)

set(f_c_string_cpp_test_SOURCES
    f_c_string.test.cpp
)
add_library(f_c_string_cpp_test "${f_c_string_cpp_test_SOURCES}")
set_target_properties(f_c_string_cpp_test PROPERTIES
 	ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
	LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
)
target_include_directories(f_c_string_cpp_test PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
set(test_f_c_string_t_SOURCES
        f_c_string_t.test.f90
)
set(test_f_c_string_t_LIBRARY_DEPENDENCIES
        v3 f_c_string_cpp_test
)
add_fortran_ctest(test_f_c_string_t
        ${test_f_c_string_t_SOURCES}
        "${test_f_c_string_t_LIBRARY_DEPENDENCIES}"
)

set(test_f_c_string_array_t_SOURCES
        f_c_string_array_t.test.f90
)
set(test_f_c_string_array_t_LIBRARY_DEPENDENCIES
        v3 f_c_string_cpp_test
)
add_fortran_ctest(test_f_c_string_array_t
        ${test_f_c_string_array_t_SOURCES}
        "${test_f_c_string_array_t_LIBRARY_DEPENDENCIES}"
)
