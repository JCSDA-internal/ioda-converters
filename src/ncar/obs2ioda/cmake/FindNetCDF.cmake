set(NetCDF_FOUND FALSE)

# Include file names for each component
set(NetCDF_C_INCLUDE_NAME netcdf.h)
set(NetCDF_CXX_INCLUDE_NAME netcdf)
set(NetCDF_Fortran_INCLUDE_NAME netcdf.mod)

# Library names for each component
set(NetCDF_C_LIBRARY_NAME netcdf)
set(NetCDF_CXX_LIBRARY_NAME netcdf_c++4)
set(NetCDF_Fortran_LIBRARY_NAME netcdff)

# List of components to search for
list(APPEND _possible_components C CXX Fortran)

foreach(_comp IN LISTS _possible_components)
  if(_comp STREQUAL "C")
    set(_config_executable nc-config)
  elseif(_comp STREQUAL "CXX")
    set(_config_executable ncxx4-config)
  elseif(_comp STREQUAL "Fortran")
    set(_config_executable nf-config)
  endif()

  # Find the config executable for the component
  find_program(NetCDF_${_comp}_CONFIG_EXECUTABLE
          ${_config_executable}
          HINTS ${NetCDF_INCLUDE_DIRS}
          PATH_SUFFIXES bin
          DOC "NetCDF ${_config_executable} helper"
  )

  if(NetCDF_${_comp}_CONFIG_EXECUTABLE)
    set(NetCDF_${_comp}_FOUND TRUE)

    # Get libraries and library directory
    if(_comp STREQUAL "Fortran")
      execute_process(
              COMMAND ${NetCDF_${_comp}_CONFIG_EXECUTABLE} --flibs
              OUTPUT_VARIABLE NetCDF_${_comp}_LIBRARIES
              OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      # Extract the -L path from the library flags
      string(REGEX MATCH "-L([^ ]+)" NetCDF_${_comp}_LIBRARY_DIR "${NetCDF_${_comp}_LIBRARIES}")
      string(REPLACE "-L" "" NetCDF_${_comp}_LIBRARY_DIR "${NetCDF_${_comp}_LIBRARY_DIR}")
    else()
      execute_process(
              COMMAND ${NetCDF_${_comp}_CONFIG_EXECUTABLE} --libs
              OUTPUT_VARIABLE NetCDF_${_comp}_LIBRARIES
              OUTPUT_STRIP_TRAILING_WHITESPACE
      )
      execute_process(
              COMMAND ${NetCDF_${_comp}_CONFIG_EXECUTABLE} --libdir
              OUTPUT_VARIABLE NetCDF_${_comp}_LIBRARY_DIR
              OUTPUT_STRIP_TRAILING_WHITESPACE
      )
    endif()

    # Get include directory
    execute_process(
            COMMAND ${NetCDF_${_comp}_CONFIG_EXECUTABLE} --includedir
            OUTPUT_VARIABLE NetCDF_${_comp}_INCLUDE_DIR
            OUTPUT_STRIP_TRAILING_WHITESPACE
    )

    # Define an imported interface target for each component
    add_library(NetCDF::NetCDF_${_comp} INTERFACE IMPORTED)
    set_target_properties(NetCDF::NetCDF_${_comp} PROPERTIES
            INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_${_comp}_INCLUDE_DIR}"
            INTERFACE_LINK_LIBRARIES "${NetCDF_${_comp}_LIBRARIES}"
            INTERFACE_LINK_DIRECTORIES "${NetCDF_${_comp}_LIBRARY_DIR}"
    )
  endif()
endforeach()

if (NetCDF_C_FOUND AND NetCDF_CXX_FOUND AND NetCDF_Fortran_FOUND)
  set(NetCDF_FOUND TRUE)
else ()
  set(MISSING_NETCDF_COMPONENTS "")
  foreach (_comp IN LISTS _possible_components)
    if (NOT NetCDF_${_comp}_FOUND)
      list(APPEND MISSING_NETCDF_COMPONENTS "${_comp}")
    endif ()
  endforeach ()
  message(FATAL_ERROR "Could not find NetCDF components: ${MISSING_NETCDF_COMPONENTS}")
endif ()
