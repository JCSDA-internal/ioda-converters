# link with libbufr.a in the py-ncepbufr project
# the link_directories command needs to go before all targets that require libbufr.a
link_directories (/Users/stephenh/projects/py-ncepbufr/src)

add_executable (pb_decode pb_decode.f90)
add_executable (pb_decode_events pb_decode_events.f90)

target_link_libraries (pb_decode bufr)
target_link_libraries (pb_decode_events bufr)

install (TARGETS pb_decode pb_decode_events DESTINATION bin)
install (PROGRAMS bufrtest.py DESTINATION bin)
