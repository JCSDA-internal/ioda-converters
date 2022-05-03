# This file will be configured to contain variables for CPack. These variables
# should be set in the CMake list file of the project before CPack module is
# included. The list of available CPACK_xxx variables and their associated
# documentation may be obtained using
#  cpack --help-variable-list
#
# Some variables are common to all generators (e.g. CPACK_PACKAGE_NAME)
# and some are specific to a generator
# (e.g. CPACK_NSIS_EXTRA_INSTALL_COMMANDS). The generator specific variables
# usually begin with CPACK_<GENNAME>_xxxx.


set(CPACK_BUILD_SOURCE_DIRS "/home/ubuntu/jedi/ioda-bundle/iodaconv;/home/ubuntu/jedi/ioda-bundle/iodaconv/bld")
set(CPACK_CMAKE_GENERATOR "Unix Makefiles")
set(CPACK_COMPONENT_UNSPECIFIED_HIDDEN "TRUE")
set(CPACK_COMPONENT_UNSPECIFIED_REQUIRED "TRUE")
set(CPACK_DEFAULT_PACKAGE_DESCRIPTION_FILE "/usr/local/share/cmake-3.20/Templates/CPack.GenericDescription.txt")
set(CPACK_DEFAULT_PACKAGE_DESCRIPTION_SUMMARY "iodaconv built using CMake")
set(CPACK_GENERATOR "TGZ")
set(CPACK_INSTALL_CMAKE_PROJECTS "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld;iodaconv;ALL;/")
set(CPACK_INSTALL_PREFIX "/usr/local")
set(CPACK_MODULE_PATH "/home/ubuntu/jedi/ioda-bundle/iodaconv/cmake;/usr/local/./share/ecbuild/cmake;/usr/local/share/ecbuild/cmake;/usr/local/share/ecbuild/cmake/contrib;/home/ubuntu/jedi/ioda-bundle/iodaconv/cmake")
set(CPACK_NSIS_DISPLAY_NAME "iodaconv 0.0.1")
set(CPACK_NSIS_INSTALLER_ICON_CODE "")
set(CPACK_NSIS_INSTALLER_MUI_ICON_CODE "")
set(CPACK_NSIS_INSTALL_ROOT "$PROGRAMFILES")
set(CPACK_NSIS_PACKAGE_NAME "iodaconv 0.0.1")
set(CPACK_NSIS_UNINSTALL_NAME "Uninstall")
set(CPACK_OUTPUT_CONFIG_FILE "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/CPackConfig.cmake")
set(CPACK_PACKAGE_DEFAULT_LOCATION "/")
set(CPACK_PACKAGE_DESCRIPTION_FILE "/usr/local/share/cmake-3.20/Templates/CPack.GenericDescription.txt")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "iodaconv misses a description")
set(CPACK_PACKAGE_FILE_NAME "iodaconv-0.0.1-Linux-x86_64")
set(CPACK_PACKAGE_INSTALL_DIRECTORY "iodaconv 0.0.1")
set(CPACK_PACKAGE_INSTALL_REGISTRY_KEY "iodaconv 0.0.1")
set(CPACK_PACKAGE_NAME "iodaconv")
set(CPACK_PACKAGE_RELOCATABLE "true")
set(CPACK_PACKAGE_VENDOR "ECMWF")
set(CPACK_PACKAGE_VERSION "0.0.1")
set(CPACK_PACKAGE_VERSION_MAJOR "0")
set(CPACK_PACKAGE_VERSION_MINOR "0")
set(CPACK_PACKAGE_VERSION_PATCH "1")
set(CPACK_RESOURCE_FILE_LICENSE "/usr/local/share/cmake-3.20/Templates/CPack.GenericLicense.txt")
set(CPACK_RESOURCE_FILE_README "/usr/local/share/cmake-3.20/Templates/CPack.GenericDescription.txt")
set(CPACK_RESOURCE_FILE_WELCOME "/usr/local/share/cmake-3.20/Templates/CPack.GenericWelcome.txt")
set(CPACK_SET_DESTDIR "OFF")
set(CPACK_SOURCE_GENERATOR "TGZ")
set(CPACK_SOURCE_IGNORE_FILES "/build/;/\\.git/;/\\.svn/;CMakeLists.txt.user;\\.swp$;p4config")
set(CPACK_SOURCE_INSTALLED_DIRECTORIES "/home/ubuntu/jedi/ioda-bundle/iodaconv;.;/usr/local/share/ecbuild/cmake;cmake/;/usr/local/bin;bin/;/usr/local/share/ecbuild/check_linker;share/ecbuild/check_linker/")
set(CPACK_SOURCE_OUTPUT_CONFIG_FILE "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/CPackSourceConfig.cmake")
set(CPACK_SYSTEM_NAME "Linux")
set(CPACK_THREADS "1")
set(CPACK_TOPLEVEL_TAG "Linux")
set(CPACK_WIX_SIZEOF_VOID_P "8")

if(NOT CPACK_PROPERTIES_FILE)
  set(CPACK_PROPERTIES_FILE "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/CPackProperties.cmake")
endif()

if(EXISTS ${CPACK_PROPERTIES_FILE})
  include(${CPACK_PROPERTIES_FILE})
endif()
