# The Fortran code links with BUFR library
if( iodaconv_bufr_ENABLED )
  ecbuild_add_executable( TARGET  pb_decode
                          SOURCES pb_decode.f90
                          LIBS    bufr::bufr_4)

  ecbuild_add_executable( TARGET  pb_decode_events
                          SOURCES pb_decode_events.f90
                          LIBS    bufr::bufr_4)
endif()
