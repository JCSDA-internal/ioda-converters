# CMake generated Testfile for 
# Source directory: /home/ubuntu/jedi/ioda-bundle/iodaconv/src/conventional
# Build directory: /home/ubuntu/jedi/ioda-bundle/iodaconv/bld/src/conventional
# 
# This file includes the relevant testing commands required for 
# testing this directory and lists subdirectories to be tested as well.
add_test(iodaconv_conventional_coding_norms "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/bin/iodaconv_lint.sh" "/home/ubuntu/jedi/ioda-bundle/iodaconv/src/conventional" "/home/ubuntu/jedi/ioda-bundle/iodaconv")
set_tests_properties(iodaconv_conventional_coding_norms PROPERTIES  ENVIRONMENT "OMP_NUM_THREADS=1" LABELS "iodaconv;script" _BACKTRACE_TRIPLES "/usr/local/share/ecbuild/cmake/ecbuild_add_test.cmake;438;add_test;/home/ubuntu/jedi/ioda-bundle/iodaconv/src/conventional/CMakeLists.txt;26;ecbuild_add_test;/home/ubuntu/jedi/ioda-bundle/iodaconv/src/conventional/CMakeLists.txt;0;")
