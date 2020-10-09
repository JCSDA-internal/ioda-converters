# (C) Copyright 2018-2020 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# IODA-CONVERTERS
################################################################################

cmake_minimum_required( VERSION 3.12 )

project( iodaconv VERSION 0.0.1 LANGUAGES CXX Fortran )

## Configuration
include(GNUInstallDirs)
include(CTest)

list( INSERT CMAKE_MODULE_PATH 0 ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
include( iodaconv_extra_macros )

# Location of .pycodestyle for norm checking within IODA-converters
set( IODACONV_PYLINT_CFG_DIR ${CMAKE_CURRENT_SOURCE_DIR} )

# Location of installed python iodaconv libraries
set( PYIODACONV_BUILD_LIBDIR   ${CMAKE_BINARY_DIR}/lib/pyiodaconv )
set( PYIODACONV_INSTALL_LIBDIR ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/pyiodaconv )


## Dependencies

# Optional: gnssro converter dependencies
# Either jedicmake or ecbuild are required for FindNetCDF.cmake
find_package(jedicmake QUIET)
if(jedicmake_FOUND)
    message(STATUS "Found: jedicmake")
else()
    message(STATUS "NOT Found: jedicmake")
    find_package(ecbuild QUIET)
    if( NOT ecbuild_FOUND OR NOT EXISTS ${ecbuild_MACROS_DIR})
        message(STATUS "NOT Found: ecbuild")
        message(WARNING "Neither jedicmake nor ecbuild was found.")
    else()
        message(STATUS "Found: ecbuild")
    endif()
    list(APPEND CMAKE_MODULE_PATH ${ecbuild_MACROS_DIR})
    include(ecbuild_log) #Required for ecbuild/FindNetCDF.cmake
endif()
# Optional: pygrib for python for GRIB2 input files
if ( USE_PYGRIB )
    message(STATUS "Adding python tests that import pygrib")
    set( iodaconv_pygrib_ENABLED True)
else()
    message(STATUS "Skipping python tests that import pygrib")
    set( iodaconv_pygrib_ENABLED False)
endif()

find_package(eckit QUIET)
find_package(oops QUIET)
find_package(ioda_engines QUIET)
find_package(bufr QUIET)
find_package(NetCDF QUIET COMPONENTS Fortran)

if( bufr_FOUND AND NetCDF_Fortran_FOUND )
    set(iodaconv_gnssro_ENABLED True)
    message(STATUS "Found: bufr")
    message(STATUS "Found: NetCDF::NetCDF_Fortran")
    message(STATUS "Enabled Component: gnssro")
else()
    set(iodaconv_gnssro_ENABLED False)
    if(NOT bufr_FOUND)
        message(STATUS "NOT-Found: bufr - GNSSRO converter disabled")
    endif()
    if(NOT NetCDF_Fortran_FOUND)
        message(STATUS "NOT-Found: NetCDF - GNSSRO converter disabled")
    endif()
    message(STATUS "Disabled Component: gnssro")
endif()

if( eckit_FOUND AND oops_FOUND AND ioda_engines_FOUND AND bufr_FOUND )
    set(iodaconv_bufr_ENABLED True)
    message(STATUS "Found: eckit")
    message(STATUS "Found: oops")
    message(STATUS "Found: ioda_engines")
    message(STATUS "Found: bufr")
    message(STATUS "Enabled Component: bufr")
else()
    set(iodaconv_bufr_ENABLED False)
    if(NOT eckit_FOUND)
        message(STATUS "NOT-Found: eckit - BUFR converter disabled")
    endif()
    if(NOT oops_FOUND)
        message(STATUS "NOT-Found: oops - BUFR converter disabled")
    endif()
    if(NOT ioda_engines_FOUND)
        message(STATUS "NOT-Found: ioda_engines - BUFR converter disabled")
    endif()
    if(NOT bufr_FOUND)
        message(STATUS "NOT-Found: bufr - BUFR converter disabled")
    endif()
    message(STATUS "Disabled Component: bufr")
endif()


include( iodaconv_compiler_flags )


## Sources

add_subdirectory( src )
add_subdirectory( tools )
add_subdirectory( test )


## Package config
include(CMakePackageConfigHelpers)

#Install location for CMake config files
set(CONFIG_INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME})

# <pkgname>-config.cmake: build-tree
set( CONFIG_BINDIR ${CMAKE_BINARY_DIR}/bin )
set( CONFIG_PYTHON_PATH ${PYIODACONV_BUILD_LIBDIR} )
configure_package_config_file(PackageConfig.cmake.in ${PROJECT_NAME}-config.cmake
                              INSTALL_DESTINATION .
                              INSTALL_PREFIX ${CMAKE_CURRENT_BINARY_DIR}
                              PATH_VARS CONFIG_BINDIR CONFIG_PYTHON_PATH)

# <pkgname>-config.cmake: install-tree
set( CONFIG_BINDIR ${CMAKE_INSTALL_FULL_BINDIR} )
set( CONFIG_PYTHON_PATH ${PYIODACONV_INSTALL_LIBDIR} )
configure_package_config_file(PackageConfig.cmake.in install/${PROJECT_NAME}-config.cmake
                              INSTALL_DESTINATION ${CONFIG_INSTALL_DESTINATION}
                              PATH_VARS CONFIG_BINDIR CONFIG_PYTHON_PATH)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/install/${PROJECT_NAME}-config.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})

# <pkgname>-config-version.cmake
write_basic_package_version_file(${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
                                 VERSION ${PROJECT_VERSION} COMPATIBILITY AnyNewerVersion)
install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake
        DESTINATION ${CONFIG_INSTALL_DESTINATION})


# CPack stuff. Used for jedi-build to make a relocatable package.
set(CPACK_PACKAGE_NAME "ioda-converters")
set(CPACK_PACKAGE_VENDOR "Joint Center for Satellite Data Assimilation")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "ioda-converters")
set(CPACK_PACKAGE_VERSION "${PROJECT_VERSION}")
set(CPACK_PACKAGE_VERSION_MAJOR "${PROJECT_VERSION_MAJOR}")
set(CPACK_PACKAGE_VERSION_MINOR "${PROJECT_VERSION_MINOR}")
set(CPACK_PACKAGE_VERSION_PATCH "${PROJECT_VERSION_PATCH}")

set(CPACK_PACKAGE_DESCRIPTION_FILE "${CMAKE_CURRENT_SOURCE_DIR}/README.md")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "ioda-converters")
set(CPACK_PACKAGE_CONTACT "Stephen Herbener (stephenh@ucar.edu)")
set(CPACK_STRIP_FILES FALSE)
set(CPACK_SOURCE_STRIP_FILES FALSE)

# This must always be last!
include(CPack)
