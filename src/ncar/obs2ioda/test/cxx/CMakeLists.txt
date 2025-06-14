FETCHCONTENT_DECLARE(
        googletest
        URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
FETCHCONTENT_MAKEAVAILABLE(googletest)

set(test_ioda_obs_schema_SOURCES ioda_obs_schema.test.cc)
list(TRANSFORM test_ioda_obs_schema_SOURCES PREPEND ${CMAKE_CURRENT_SOURCE_DIR}/)
set(test_ioda_obs_schema_LIBRARIES yaml-cpp::yaml-cpp GTest::gtest_main obs2ioda_cxx)
set(test_ioda_obs_schema_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/obs2ioda-v3/src/cxx)
add_cxx_ctest(test_ioda_obs_schema "${test_ioda_obs_schema_SOURCES}" "${test_ioda_obs_schema_INCLUDE_DIRS}" "${test_ioda_obs_schema_LIBRARIES}")

