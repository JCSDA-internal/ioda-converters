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
    set(OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
        $<$<COMPILE_LANGUAGE:Fortran>:-mcmodel=medium>
    )
    if (CMAKE_Fortran_COMPILER_ID MATCHES GNU)
        list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
             $<$<COMPILE_LANGUAGE:Fortran>:-cpp -ffree-line-length-none>
        )
        if (CMAKE_BUILD_TYPE MATCHES Debug)
            list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
                 $<$<COMPILE_LANGUAGE:Fortran>:-fbacktrace -ffpe-trap=invalid,zero,overflow -fcheck=all>
            )
        endif ()
    elseif (CMAKE_Fortran_COMPILER_ID MATCHES Intel)
        list(APPEND OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE
             $<$<COMPILE_LANGUAGE:Fortran>:-fpp>
        )
    endif ()
    target_compile_options(${target} PRIVATE ${OBS2IODA_FORTRAN_TARGET_COMPILE_OPTIONS_PRIVATE})
    target_link_libraries(${target} PUBLIC ${public_link_libraries})
    add_executable(obs2ioda_${target} ${target_main})
    target_link_libraries(obs2ioda_${target} PUBLIC ${target})

endfunction()
