
# Set include directories
include_directories(${NetCDF_INCLUDE_DIRS} )

# Define sources
set(v2_SOURCES
    define_mod.f90
    gnssro_mod.f90
    hsd.f90
    satwnd_mod.f90
    kinds.f90
    main.f90
    ncio_mod.f90
    netcdf_mod.f90
    prepbufr_mod.f90
    radiance_mod.f90
    ufo_variables_mod.F90
    utils_mod.f90
)
list(TRANSFORM v2_SOURCES PREPEND "src/")
set(v2_MAIN_SOURCE
    main.f90
)
list(TRANSFORM v2_MAIN_SOURCE PREPEND "src/")
set(v2_PUBLIC_LINK_LIBRARIES "${NetCDF_LIBRARIES}" "${NCEP_BUFR_LIB}")
add_library(v2 SHARED ${v2_SOURCES})
obs2ioda_fortran_target(v2 ${v2_MAIN_SOURCE})

