# CMake generated Testfile for 
# Source directory: /home/ubuntu/jedi/ioda-bundle/iodaconv/src/compo
# Build directory: /home/ubuntu/jedi/ioda-bundle/iodaconv/bld/src/compo
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(iodaconv_compo_coding_norms "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/bin/iodaconv_lint.sh" "/home/ubuntu/jedi/ioda-bundle/iodaconv/src/compo" "/home/ubuntu/jedi/ioda-bundle/iodaconv")
set_tests_properties(iodaconv_compo_coding_norms PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "iodaconv;script" _BACKTRACE_TRIPLES "/usr/local/share/ecbuild/cmake/ecbuild_add_test.cmake;438;add_test;/home/ubuntu/jedi/ioda-bundle/iodaconv/src/compo/CMakeLists.txt;28;ecbuild_add_test;/home/ubuntu/jedi/ioda-bundle/iodaconv/src/compo/CMakeLists.txt;0;")
