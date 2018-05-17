# The Fortran code links with libbufr.a, which at this point is
# expected to live in /usr/local/lib

add_executable (pb_decode pb_decode.f90)
add_executable (pb_decode_events pb_decode_events.f90)

target_link_libraries (pb_decode bufr)
target_link_libraries (pb_decode_events bufr)

