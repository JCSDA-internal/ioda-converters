# (C) Copyright 2018 UCAR.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

################################################################################
# IODA-CONVERTERS
################################################################################

cmake_minimum_required( VERSION 3.12 )

project( iodaconv VERSION 0.0.1 LANGUAGES Fortran )

## Configuration
include(GNUInstallDirs)
include(CTest)

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake;${CMAKE_MODULE_PATH} )
include( iodaconv_extra_macros )

# Location of .pycodestyle for norm checking within IODA-converters
set( IODACONV_PYLINT_CFG_DIR ${CMAKE_CURRENT_SOURCE_DIR} )

# Location of installed python iodaconv libraries
set( PYIODACONV_BUILD_LIBDIR   ${CMAKE_BINARY_DIR}/lib/pyiodaconv )
set( PYIODACONV_INSTALL_LIBDIR ${CMAKE_INSTALL_PREFIX}/${CMAKE_INSTALL_LIBDIR}/pyiodaconv )


## Dependencies

# Optional: odb2 converter dependencies
find_package(odb_api QUIET)
if(odb_api_FOUND)
    set(iodaconv_odb2_ENABLED True)
    message(STATUS "Found: odb_api")
    message(STATUS "Enabled Component: odb2")
else()
    set(iodaconv_odb2_ENABLED False)
    message(STATUS "NOT-Found: odb_api")
    message(STATUS "Disabled Component: odb_api")
endif()

# Optional: odc converter dependencies
find_package(odc QUIET)
if(odc_FOUND)
    set(iodaconv_odc_ENABLED True)
    message(STATUS "Found: odc")
    message(STATUS "Enabled Component: odc")
else()
    set(iodaconv_odc_ENABLED False)
    message(STATUS "NOT-Found: odc")
    message(STATUS "Disabled Component: odc")
endif()

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

find_package(bufrlib QUIET)
find_package(NetCDF QUIET COMPONENTS Fortran)
if( bufrlib_FOUND AND NetCDF_Fortran_FOUND )
    set(iodaconv_gnssro_ENABLED True)
    message(STATUS "Found: bufrlib")
    message(STATUS "Found: NetCDF::NetCDF_Fortran")
    message(STATUS "Enabled Component: gnssro")
else()
    set(iodaconv_gnssro_ENABLED False)
    if(NOT bufrlib_FOUND)
        message(STATUS "NOT-Found: bufrlib - GNSSRO converter disabled")
    endif()
    if(NOT NetCDF_Fortran_FOUND)
        message(STATUS "NOT-Found: NetCDF - GNSSRO converter disabled")
    endif()
    message(STATUS "Disabled Component: gnssro")
endif()


## Sources
# Core components
add_subdirectory(src/chem)
add_subdirectory(src/gsi-ncdiag)
add_subdirectory(src/lib-python)
add_subdirectory(src/marine)
add_subdirectory(src/ncep)
add_subdirectory(src/nsmc)
add_subdirectory(src/ssec)
add_subdirectory(src/wrfda-ncdiag)

# Optional components
if(iodaconv_gnssro_ENABLED)
    add_subdirectory(src/gnssro)
endif()
if(iodaconv_odb2_ENABLED)
    set(odb2_ENABLED True)
endif()
if(iodaconv_odc_ENABLED)
    add_subdirectory(src/odc)
endif()

# Auxiliaries
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
