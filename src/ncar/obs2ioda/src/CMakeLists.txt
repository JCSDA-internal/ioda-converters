
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
set(v3_PUBLIC_LINK_LIBRARIES NetCDF::NetCDF_Fortran "${NCEP_BUFR_LIB}" obs2ioda_cxx)
add_library(v3 SHARED ${v3_SOURCES})
obs2ioda_fortran_library(v3 "${v3_PUBLIC_LINK_LIBRARIES}")
set(obs2ioda_v3_PUBLIC_LINK_LIBRARIES v3)
add_executable(obs2ioda_v3 ${obs2ioda_v3_SOURCES})
obs2ioda_fortran_executable(obs2ioda_v3 "${obs2ioda_v3_PUBLIC_LINK_LIBRARIES}")

if (BUILD_GOES_ABI_CONVERTER)
    set(goes_abi_converter_PUBLIC_LINK_LIBRARIES v3)
    add_executable(goes_abi_converter ${goes_abi_converter_SOURCES})
    obs2ioda_fortran_executable(goes_abi_converter "${goes_abi_converter_PUBLIC_LINK_LIBRARIES}")
endif ()
