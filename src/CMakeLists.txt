
add_subdirectory(utils)
add_subdirectory(compo)
add_subdirectory(gsi_ncdiag)
add_subdirectory(goes)
add_subdirectory(hdf5)
add_subdirectory(land)
add_subdirectory(pyiodaconv)
add_subdirectory(marine)
add_subdirectory(conventional)
add_subdirectory(ncep)
add_subdirectory(ssec)
add_subdirectory(wrfda_ncdiag)
add_subdirectory(single_observation)
add_subdirectory(mrms)

# Optional components
if(iodaconv_gnssro_ENABLED)
    add_subdirectory(gnssro)
endif()

if(iodaconv_bufr_query_ENABLED)
  add_subdirectory(bufr)
endif()

if(iodaconv_pbfortran_ENABLED)
    add_subdirectory(ncar-bufr2nc-fortran)
endif()

if(iodaconv_satbias_ENABLED)
  add_subdirectory(satbias)
endif()

if(iodaconv_obserror_ENABLED)
  add_subdirectory(obserror)
endif()
