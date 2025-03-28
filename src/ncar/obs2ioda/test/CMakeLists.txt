include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/Obs2Ioda_Test_Functions.cmake")
INCLUDE(FetchContent)
FETCHCONTENT_DECLARE(
        googletest
        URL https://github.com/google/googletest/archive/03597a01ee50ed33e9dfd640b249b4be3799d395.zip
)
FETCHCONTENT_MAKEAVAILABLE(googletest)
add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/cxx)
