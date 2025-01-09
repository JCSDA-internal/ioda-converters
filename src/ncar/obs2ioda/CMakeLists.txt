# Check CMake version
cmake_minimum_required(VERSION 3.10)

include("${CMAKE_SOURCE_DIR}/cmake/Obs2Ioda_Functions.cmake")
# Define the project
project(obs2ioda LANGUAGES Fortran)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CMAKE_SOURCE_DIR}/cmake/")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)
# Fortran module output directory for build interface
set(OBS2IODA_MODULE_DIR ${PROJECT_NAME}/module/${CMAKE_Fortran_COMPILER_ID}/${CMAKE_Fortran_COMPILER_VERSION})
# Install Fortran module directory
install(DIRECTORY ${CMAKE_BINARY_DIR}/${OBS2IODA_MODULE_DIR}/ DESTINATION ${CMAKE_INSTALL_LIBDIR}/${OBS2IODA_MODULE_DIR}/)


# Set the Fortran compiler and flags
set(NCEP_BUFR_LIB CACHE STRING "" )

# Find required packages
find_package(NetCDF REQUIRED COMPONENTS Fortran C)

add_subdirectory("${CMAKE_SOURCE_DIR}/obs2ioda-v2")
