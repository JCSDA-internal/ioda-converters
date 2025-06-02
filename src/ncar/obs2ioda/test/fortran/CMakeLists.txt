set(test_transpose_and_flatten_SOURCES
        transpose_and_flatten.test.f90
)
set(test_transpose_and_flatten_LIBRARY_DEPENDENCIES
        v3
)
add_executable(test_transpose_and_flatten ${test_transpose_and_flatten_SOURCES})
target_link_libraries(test_transpose_and_flatten
        PRIVATE
        ${test_transpose_and_flatten_LIBRARY_DEPENDENCIES}
)
add_test(NAME test_transpose_and_flatten
        COMMAND test_transpose_and_flatten
)

set(test_get_julian_time_SOURCES
        get_julian_time.test.f90
)
set(test_get_julian_time_LIBRARY_DEPENDENCIES
        v3
)
add_executable(test_get_julian_time ${test_get_julian_time_SOURCES})
target_link_libraries(test_get_julian_time
        PRIVATE
        ${test_get_julian_time_LIBRARY_DEPENDENCIES}
)
add_test(NAME test_get_julian_time
        COMMAND test_get_julian_time
)
