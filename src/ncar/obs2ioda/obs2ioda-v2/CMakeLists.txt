
# Set include directories
include_directories(${NetCDF_INCLUDE_DIRS} )
add_subdirectory(src/cxx)

# Define sources
set(v2_SOURCES
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
)
list(TRANSFORM v2_SOURCES PREPEND "src/")
set(obs2ioda_v2_SOURCES
    main.f90
)
list(TRANSFORM obs2ioda_v2_SOURCES PREPEND "src/")
set(v2_PUBLIC_LINK_LIBRARIES "${NetCDF_LIBRARIES}" "${NCEP_BUFR_LIB}" obs2ioda_cxx)
add_library(v2 SHARED ${v2_SOURCES})
obs2ioda_fortran_library(v2 "${v2_PUBLIC_LINK_LIBRARIES}")
set(obs2ioda_v2_PUBLIC_LINK_LIBRARIES v2 )
add_executable(obs2ioda_v2 ${obs2ioda_v2_SOURCES})
obs2ioda_fortran_executable(obs2ioda_v2 "${obs2ioda_v2_PUBLIC_LINK_LIBRARIES}")

