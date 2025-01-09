include("${CMAKE_SOURCE_DIR}/cmake/Obs2Ioda_CompilerFlags.cmake")

# This CMake function, `obs2ioda_fortran_target`, configures Fortran targets for obs2ioda.
#
# Its arguments are:
# - target: the name of the target to configure
# - target_main: the main source file for the executable
#
# The function sets the following properties for the target:
# - The directory for Fortran module files
# - The include directories for the target (both build and install interfaces)
# - The install RPATH to enable finding shared libraries at runtime
# - Fortran format as FREE
# - Compiler-specific options and flags, depending on whether the GNU Fortran or Intel Fortran compiler is used,
#   and whether the build type is Debug or not
#
# The function also links the public libraries to the target and creates an executable `obs2ioda_${target}` linked
# to the original target.
function(obs2ioda_fortran_target target target_main)
    set_target_properties(${target} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/${OBS2IODA_MODULE_DIR})
    target_include_directories(${target} INTERFACE $<BUILD_INTERFACE:${CMAKE_BINARY_DIR}/${OBS2IODA_MODULE_DIR}>
                               $<INSTALL_INTERFACE:${OBS2IODA_MODULE_DIR}>)
    #Relocatable, portable, runtime dynamic linking
    set_target_properties(${target} PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
    # Global Fortran configuration
    set(public_link_libraries_name ${target}_PUBLIC_LINK_LIBRARIES)
    set(public_link_libraries ${${public_link_libraries_name}})
    set_target_properties(${target} PROPERTIES Fortran_FORMAT FREE)

    # Compiler-specific options and flags
    set(OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE "")
    if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
             ${FORTRAN_COMPILER_GNU_FLAGS}
        )
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
    add_executable(obs2ioda_${target} ${target_main})
    target_link_libraries(obs2ioda_${target} PUBLIC ${target})

endfunction()

# This CMake function, `obs2ioda_cxx_library`, configures C++ targets for obs2ioda.
#
# Its arguments are:
# - target: the name of the C++ target to configure
# - public_link_libraries: the public link libraries associated with the target
#
# The function performs the following:
# * Sets the `INSTALL_RPATH` property for the target, ensuring that shared libraries can be found
#    relative to the target's installation directory.
# * Links the provided public libraries to the target using `target_link_libraries`.
#
# This setup ensures that the target is correctly linked with its public dependencies and that
# runtime shared library paths are properly configured for relocatable installations.
function(obs2ioda_cxx_library target public_link_libraries)
    set_target_properties(${target} PROPERTIES INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
    target_link_libraries(${target} PUBLIC ${public_link_libraries})
endfunction()
