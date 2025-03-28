# Adds a C++ test executable and registers it as a CTest.
#
# This function creates an executable target for a C++ test, sets up include directories
# and library dependencies, and registers the test with CTest.
#
# Arguments:
#   name                 - Name of the test executable.
#   sources              - List of source files for the test.
#   include_directories  - Directories to be included during compilation.
#   library_dependencies - Libraries that the test executable should link against.
#
# The function does the following:
#   1. Creates an executable target with the given name and sources.
#   2. Sets the include directories for the target.
#   3. Links the target against the specified libraries.
#   4. Registers the executable as a CTest with Google Test support.
#
# Example usage:
#   add_cxx_ctest(my_test "test.cpp" "${PROJECT_SOURCE_DIR}/include" "gtest_main")
#
function(add_cxx_ctest name sources include_directories library_dependencies)
    add_executable(${name} ${sources})
    target_include_directories(${name} PUBLIC ${include_directories})
    target_link_libraries(${name} PUBLIC ${library_dependencies})
    add_test(
            NAME ${name}
            COMMAND ${name} --gtest_filter=*
    )
endfunction()
