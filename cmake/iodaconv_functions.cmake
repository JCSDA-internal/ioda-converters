# This CMake function, `obs2ioda_fortran_library_target`, configures Fortran library targets for obs2ioda.
#
# Its arguments are:
# - target: the name of the target to configure
# - public_link_libraries: the public link libraries associated with the target
#
# The function sets the following properties for the target:
# - The directory for Fortran module files
# - The include directories for the target (both build and install interfaces)
# - The install RPATH to enable finding shared libraries at runtime
# - Fortran format as FREE
# - Compiler-specific options and flags, depending on whether the GNU Fortran or Intel Fortran compiler is used,
#   and whether the build type is Debug or not
#
# The function also links the provided public libraries to the target.
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
             ${compiler_flags_GNU_Fortran}
        )
    elseif (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
             ${compiler_flags_Intel_Fortran}
        )
    endif ()
    target_compile_options(${target} PRIVATE ${OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE})
    target_link_libraries(${target} PUBLIC ${public_link_libraries})
endfunction()

# This CMake function, `obs2ioda_fortran_executable`, configures the installation and linking of Fortran executables for obs2ioda.
#
# Its arguments are:
# - target: the name of the executable target to configure
# - public_link_libraries: the public link libraries associated with the target
#
# The function performs the following:
# - Sets the install RPATH for the target to enable finding shared libraries relative to the executable's location.
# - Links the provided public libraries to the target using `target_link_libraries`.
#
# This ensures that the target has the correct runtime library paths and is properly linked with its public dependencies.
function(obs2ioda_fortran_executable target public_link_libraries)
    set_target_properties(${target} PROPERTIES LINKER_LANGUAGE Fortran
                      RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin
	             INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")
    target_link_libraries(${target} PUBLIC ${public_link_libraries})
endfunction()

# This CMake function, `obs2ioda_cxx_library`, configures C++ targets for obs2ioda.
#
# Its arguments are:
# - target: the name of the C++ target to configure
# - include_dirs: the include directories associated with the target
# - public_link_libraries: the public link libraries associated with the target
#
# The function performs the following:
# * Sets the `INSTALL_RPATH` property for the target, ensuring that shared libraries can be found
#    relative to the target's installation directory.
# * Links the provided public libraries to the target using `target_link_libraries`.
# * Sets the include directories for the target using `target_include_directories`.
#
# This setup ensures that the target is correctly linked with its public dependencies and that
# runtime shared library paths are properly configured for relocatable installations, and that the target
# can find its include directories.
function(obs2ioda_cxx_library target include_dirs public_link_libraries)
	set_target_properties(${target} PROPERTIES
		      ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
		      LIBRARY_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/lib
		      INSTALL_RPATH "\$ORIGIN/../${CMAKE_INSTALL_LIBDIR}")

    target_link_libraries(${target} PUBLIC ${public_link_libraries})
    target_include_directories(${target} PUBLIC ${include_dirs})
endfunction()
