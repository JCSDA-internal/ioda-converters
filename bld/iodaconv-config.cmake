# Config file for the iodaconv package
# Defines the following variables:
#
#  iodaconv_FEATURES       - list of enabled features
#  iodaconv_VERSION        - version of the package
#  iodaconv_GIT_SHA1       - Git revision of the package
#  iodaconv_GIT_SHA1_SHORT - short Git revision of the package
#


####### Expanded from @PACKAGE_INIT@ by configure_package_config_file() #######
####### Any changes to this file will be overwritten by the next CMake run ####
####### The input file was project-config.cmake.in                            ########

get_filename_component(PACKAGE_PREFIX_DIR "${CMAKE_CURRENT_LIST_DIR}/" ABSOLUTE)

macro(set_and_check _var _file)
  set(${_var} "${_file}")
  if(NOT EXISTS "${_file}")
    message(FATAL_ERROR "File or directory ${_file} referenced by variable ${_var} does not exist !")
  endif()
endmacro()

macro(check_required_components _NAME)
  foreach(comp ${${_NAME}_FIND_COMPONENTS})
    if(NOT ${_NAME}_${comp}_FOUND)
      if(${_NAME}_FIND_REQUIRED_${comp})
        set(${_NAME}_FOUND FALSE)
      endif()
    endif()
  endforeach()
endmacro()

####################################################################################

### computed paths
set_and_check(iodaconv_CMAKE_DIR "${PACKAGE_PREFIX_DIR}/.")
set_and_check(iodaconv_BASE_DIR "${PACKAGE_PREFIX_DIR}/")
if(DEFINED ECBUILD_2_COMPAT AND ECBUILD_2_COMPAT)
  set(IODACONV_CMAKE_DIR ${iodaconv_CMAKE_DIR})
  set(IODACONV_BASE_DIR ${iodaconv_BASE_DIR})
endif()

### export version info
set(iodaconv_VERSION           "0.0.1")
set(iodaconv_GIT_SHA1          "1d9d29fac3328a7884a66f3ad166ba5dc242b651")
set(iodaconv_GIT_SHA1_SHORT    "1d9d29f")

if(DEFINED ECBUILD_2_COMPAT AND ECBUILD_2_COMPAT)
  set(IODACONV_VERSION           "0.0.1" )
  set(IODACONV_GIT_SHA1          "1d9d29fac3328a7884a66f3ad166ba5dc242b651" )
  set(IODACONV_GIT_SHA1_SHORT    "1d9d29f" )
endif()

### has this configuration been exported from a build tree?
set(iodaconv_IS_BUILD_DIR_EXPORT ON)
if(DEFINED ECBUILD_2_COMPAT AND ECBUILD_2_COMPAT)
  set(IODACONV_IS_BUILD_DIR_EXPORT ${iodaconv_IS_BUILD_DIR_EXPORT})
endif()

### insert definitions for IMPORTED targets
if(NOT iodaconv_BINARY_DIR)
  find_file(iodaconv_TARGETS_FILE
    NAMES iodaconv-targets.cmake
    HINTS ${iodaconv_CMAKE_DIR};/home/ubuntu/jedi/ioda-bundle/iodaconv/bld
    NO_DEFAULT_PATH)
  if(iodaconv_TARGETS_FILE)
    include(${iodaconv_TARGETS_FILE})
  endif()
endif()

### include the <project>-import.cmake file if there is one
if(EXISTS ${iodaconv_CMAKE_DIR}/iodaconv-import.cmake)
  set(iodaconv_IMPORT_FILE "${iodaconv_CMAKE_DIR}/iodaconv-import.cmake")
  include(${iodaconv_IMPORT_FILE})
endif()

### handle third-party dependencies
if(DEFINED ECBUILD_2_COMPAT AND ECBUILD_2_COMPAT)
  set(IODACONV_LIBRARIES         "")
  set(IODACONV_TPLS              "" )

  include(${CMAKE_CURRENT_LIST_FILE}.tpls OPTIONAL)
endif()

### publish this file as imported
if( DEFINED ECBUILD_2_COMPAT AND ECBUILD_2_COMPAT )
  set(iodaconv_IMPORT_FILE ${CMAKE_CURRENT_LIST_FILE})
  mark_as_advanced(iodaconv_IMPORT_FILE)
  set(IODACONV_IMPORT_FILE ${CMAKE_CURRENT_LIST_FILE})
  mark_as_advanced(IODACONV_IMPORT_FILE)
endif()

### export features and check requirements
set(iodaconv_FEATURES "TESTS")
if(DEFINED ECBUILD_2_COMPAT AND ECBUILD_2_COMPAT)
  set(IODACONV_FEATURES ${iodaconv_FEATURES})
endif()
foreach(_f ${iodaconv_FEATURES})
  set(iodaconv_${_f}_FOUND 1)
  set(iodaconv_HAVE_${_f} 1)
  if(DEFINED ECBUILD_2_COMPAT AND ECBUILD_2_COMPAT)
    set(IODACONV_HAVE_${_f} 1)
  endif()
endforeach()
check_required_components(iodaconv)
