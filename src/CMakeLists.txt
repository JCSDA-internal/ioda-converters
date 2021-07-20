
add_subdirectory(chem)
add_subdirectory(gsi-ncdiag)
add_subdirectory(goes)
add_subdirectory(land)
add_subdirectory(lib-python)
add_subdirectory(marine)
add_subdirectory(conventional)
add_subdirectory(ncep)
add_subdirectory(ssec)
add_subdirectory(wrfda-ncdiag)
add_subdirectory(single_observation)

# Optional components
if(iodaconv_gnssro_ENABLED)
    add_subdirectory(gnssro)
endif()

if(iodaconv_bufr_ENABLED)
  add_subdirectory(bufr)
endif()

if(iodaconv_pbfortran_ENABLED)
    add_subdirectory(ncar-bufr2nc-fortran)
endif()

if(iodaconv_satbias_ENABLED)
  add_subdirectory(satbias)
endif()
