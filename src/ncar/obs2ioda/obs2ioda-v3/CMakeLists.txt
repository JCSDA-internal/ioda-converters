
add_subdirectory(src/cxx)

# Define sources
set(v3_SOURCES
        define_mod.f90
        gnssro_mod.f90
        hsd.f90
        satwnd_mod.f90
        kinds.f90
        ncio_mod.f90
        netcdf_mod.f90
        prepbufr_mod.f90
        radiance_mod.f90
        ufo_variables_mod.F90
        utils_mod.f90
        f_c_string_t_mod.f90
        f_c_string_1D_t_mod.f90
        netcdf_cxx_i_mod.f90
        netcdf_cxx_mod.f90
)
list(TRANSFORM v3_SOURCES PREPEND "src/")
set(obs2ioda_v3_SOURCES
        main.f90
)
list(TRANSFORM obs2ioda_v3_SOURCES PREPEND "src/")
set(v3_PUBLIC_LINK_LIBRARIES NetCDF::NetCDF_Fortran "${NCEP_BUFR_LIB}" obs2ioda_cxx)
add_library(v3 SHARED ${v3_SOURCES})
obs2ioda_fortran_library(v3 "${v3_PUBLIC_LINK_LIBRARIES}")
set(obs2ioda_v3_PUBLIC_LINK_LIBRARIES v3)
add_executable(obs2ioda_v3 ${obs2ioda_v3_SOURCES})
obs2ioda_fortran_executable(obs2ioda_v3 "${obs2ioda_v3_PUBLIC_LINK_LIBRARIES}")

