
add_subdirectory(cxx)

# Define sources
set(v3_SOURCES
        define_mod.f90
        gnssro_mod.f90
        hsd.f90
        satwnd_mod.f90
        kinds.f90
        ncio_mod.f90
        prepbufr_mod.f90
        radiance_mod.f90
        ufo_variables_mod.F90
        utils_mod.f90
        f_c_string_t_mod.f90
        f_c_string_array_t_mod.f90
        netcdf_cxx_i_mod.f90
        netcdf_cxx_mod.f90
        goes_abi_converter_mod.f90
)
set(obs2ioda_v3_SOURCES
        main.f90
)
set(goes_abi_converter_SOURCES
        goes_abi_converter.f90
)
set(v3_PUBLIC_LINK_LIBRARIES NetCDF::NetCDF_Fortran bufr::bufr_4 obs2ioda_cxx)
add_library(v3 SHARED ${v3_SOURCES})

function(obs2ioda_fortran_library target public_link_libraries)
    # Global Fortran configuration
    set_target_properties(${target} PROPERTIES Fortran_FORMAT FREE
              ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
              LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
              INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
    # Compiler-specific options and flags
    set(OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE "")
    if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
             ${FORTRAN_COMPILER_GNU_FLAGS}
        )
        if (CMAKE_SYSTEM_NAME MATCHES Linux)
            list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                 ${FORTRAN_COMPILER_GNU_LINUX_FLAGS}
            )
        endif ()
        if (CMAKE_BUILD_TYPE MATCHES Debug)
            list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                 ${FORTRAN_COMPILER_GNU_DEBUG_FLAGS}
            )
        endif ()
    elseif (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
             ${FORTRAN_COMPILER_INTEL_FLAGS}
        )
        if (CMAKE_BUILD_TYPE MATCHES Debug)
            list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                 ${FORTRAN_COMPILER_INTEL_DEBUG_FLAGS}
            )
        endif ()
    endif ()
    target_compile_options(${target} PRIVATE ${OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE})
    target_link_libraries(${target} PUBLIC ${public_link_libraries})
endfunction()

obs2ioda_fortran_library(v3 "${v3_PUBLIC_LINK_LIBRARIES}")

set(obs2ioda_v3_PUBLIC_LINK_LIBRARIES v3)
#add_executable(obs2ioda_v3 ${obs2ioda_v3_SOURCES})
#obs2ioda_fortran_executable(obs2ioda_v3 "${obs2ioda_v3_PUBLIC_LINK_LIBRARIES}")
ecbuild_add_executable( TARGET  obs2ioda_v3
                        SOURCES ${obs2ioda_v3_SOURCES})
set_target_properties(obs2ioda_v3 PROPERTIES LINKER_LANGUAGE Fortran
              RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
              INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
target_link_libraries(obs2ioda_v3 PUBLIC "${obs2ioda_v3_PUBLIC_LINK_LIBRARIES}")

if (BUILD_GOES_ABI_CONVERTER)
    set(goes_abi_converter_PUBLIC_LINK_LIBRARIES v3)
    #add_executable(goes_abi_converter ${goes_abi_converter_SOURCES})
    ecbuild_add_executable( TARGET  goes_abi_converter
                            SOURCES ${goes_abi_converter_SOURCES})
    set_target_properties(goes_abi_converter PROPERTIES LINKER_LANGUAGE Fortran
              RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
              INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
    target_link_libraries(goes_abi_converter PUBLIC "${goes_abi_converter_PUBLIC_LINK_LIBRARIES}")    
    #obs2ioda_fortran_executable(goes_abi_converter "${goes_abi_converter_PUBLIC_LINK_LIBRARIES}")
endif ()
