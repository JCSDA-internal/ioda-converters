
FETCHCONTENT_DECLARE(
        googletest
        URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
FETCHCONTENT_MAKEAVAILABLE(googletest)

set(test_ioda_obs_schema_SOURCES ioda_obs_schema.test.cc)
set(test_ioda_obs_schema_LIBRARIES yaml-cpp::yaml-cpp GTest::gtest_main obs2ioda_cxx)
set(test_ioda_obs_schema_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/cxx)
add_cxx_ctest(test_ioda_obs_schema "${test_ioda_obs_schema_SOURCES}" "${test_ioda_obs_schema_INCLUDE_DIRS}" "${test_ioda_obs_schema_LIBRARIES}")

set(test_netcdf_file_SOURCES netcdf_file.test.cc)
set(test_netcdf_file_LIBRARIES GTest::gtest_main obs2ioda_cxx)
set(test_netcdf_file_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/cxx)
add_cxx_ctest(test_netcdf_file "${test_netcdf_file_SOURCES}" "${test_netcdf_file_INCLUDE_DIRS}" "${test_netcdf_file_LIBRARIES}")

set(test_netcdf_error_SOURCES netcdf_error.test.cc)
set(test_netcdf_error_LIBRARIES GTest::gtest_main obs2ioda_cxx)
set(test_netcdf_error_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/cxx)
add_cxx_ctest(test_netcdf_error "${test_netcdf_error_SOURCES}" "${test_netcdf_error_INCLUDE_DIRS}" "${test_netcdf_error_LIBRARIES}")

set(test_netcdf_group_SOURCES netcdf_group.test.cc)
set(test_netcdf_group_LIBRARIES GTest::gtest_main obs2ioda_cxx)
set(test_netcdf_group_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/cxx)
add_cxx_ctest(test_netcdf_group "${test_netcdf_group_SOURCES}" "${test_netcdf_group_INCLUDE_DIRS}" "${test_netcdf_group_LIBRARIES}")

set(test_netcdf_dimension_SOURCES netcdf_dimension.test.cc)
set(test_netcdf_dimension_LIBRARIES GTest::gtest_main obs2ioda_cxx)
set(test_netcdf_dimension_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/cxx)
add_cxx_ctest(test_netcdf_dimension "${test_netcdf_dimension_SOURCES}" "${test_netcdf_dimension_INCLUDE_DIRS}" "${test_netcdf_dimension_LIBRARIES}")

set(test_netcdf_attribute_SOURCES netcdf_attribute.test.cc)
set(test_netcdf_attribute_LIBRARIES GTest::gtest_main obs2ioda_cxx)
set(test_netcdf_attribute_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/cxx)
add_cxx_ctest(test_netcdf_attribute "${test_netcdf_attribute_SOURCES}" "${test_netcdf_attribute_INCLUDE_DIRS}" "${test_netcdf_attribute_LIBRARIES}")

set(test_netcdf_variable_SOURCES netcdf_variable.test.cc)
set(test_netcdf_variable_LIBRARIES GTest::gtest_main obs2ioda_cxx)
set(test_netcdf_variable_INCLUDE_DIRS ${CMAKE_SOURCE_DIR}/src/cxx)
add_cxx_ctest(test_netcdf_variable "${test_netcdf_variable_SOURCES}" "${test_netcdf_variable_INCLUDE_DIRS}" "${test_netcdf_variable_LIBRARIES}")
