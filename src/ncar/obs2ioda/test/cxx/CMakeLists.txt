FETCHCONTENT_DECLARE(
        googletest
        URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
FETCHCONTENT_MAKEAVAILABLE(googletest)

set(test_IodaObsSchema_SOURCES test_IodaObsSchema.cc fixture/IodaObsSchemaFixture.cc)
list(TRANSFORM test_IodaObsSchema_SOURCES PREPEND ${CMAKE_CURRENT_SOURCE_DIR}/)
set(test_IodaObsSchema_LIBRARIES yaml-cpp::yaml-cpp GTest::gtest_main obs2ioda_cxx)
set(test_IodaObsSchema_INCLUDE_DIRS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/obs2ioda-v2/src/cxx)
add_cxx_ctest(test_IodaObsSchema "${test_IodaObsSchema_SOURCES}" "${test_IodaObsSchema_INCLUDE_DIRS}" "${test_IodaObsSchema_LIBRARIES}")

