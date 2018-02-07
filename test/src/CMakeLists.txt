# link with libbufr.a in the py-ncepbufr project
# the link_directories command needs to go before all targets that require libbufr.a
link_directories (/Users/stephenh/projects/py-ncepbufr/src)

add_executable (prepbufr_decode_all prepbufr_decode_all.f90)
add_executable (prepbufr_decode_all_evn prepbufr_decode_all_evn.f90)

target_link_libraries (prepbufr_decode_all bufr)
target_link_libraries (prepbufr_decode_all_evn bufr)

install (TARGETS prepbufr_decode_all prepbufr_decode_all_evn DESTINATION bin)
