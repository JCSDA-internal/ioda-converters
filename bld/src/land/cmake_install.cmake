# Install script for directory: /home/ubuntu/jedi/ioda-bundle/iodaconv/src/land

# Set the install prefix
if(NOT DEFINED CMAKE_INSTALL_PREFIX)
  set(CMAKE_INSTALL_PREFIX "/usr/local")
endif()
string(REGEX REPLACE "/$" "" CMAKE_INSTALL_PREFIX "${CMAKE_INSTALL_PREFIX}")

# Set the install configuration name.
if(NOT DEFINED CMAKE_INSTALL_CONFIG_NAME)
  if(BUILD_TYPE)
    string(REGEX REPLACE "^[^A-Za-z0-9_]+" ""
           CMAKE_INSTALL_CONFIG_NAME "${BUILD_TYPE}")
  else()
    set(CMAKE_INSTALL_CONFIG_NAME "RelWithDebInfo")
  endif()
  message(STATUS "Install configuration: \"${CMAKE_INSTALL_CONFIG_NAME}\"")
endif()

# Set the component getting installed.
if(NOT CMAKE_INSTALL_COMPONENT)
  if(COMPONENT)
    message(STATUS "Install component: \"${COMPONENT}\"")
    set(CMAKE_INSTALL_COMPONENT "${COMPONENT}")
  else()
    set(CMAKE_INSTALL_COMPONENT)
  endif()
endif()

# Install shared libraries without execute permission?
if(NOT DEFINED CMAKE_INSTALL_SO_NO_EXE)
  set(CMAKE_INSTALL_SO_NO_EXE "1")
endif()

# Is this installation the result of a crosscompile?
if(NOT DEFINED CMAKE_CROSSCOMPILING)
  set(CMAKE_CROSSCOMPILING "FALSE")
endif()

# Set default install directory permissions.
if(NOT DEFINED CMAKE_OBJDUMP)
  set(CMAKE_OBJDUMP "/usr/bin/objdump")
endif()

if("x${CMAKE_INSTALL_COMPONENT}x" STREQUAL "xUnspecifiedx" OR NOT CMAKE_INSTALL_COMPONENT)
  file(INSTALL DESTINATION "${CMAKE_INSTALL_PREFIX}/bin" TYPE PROGRAM FILES
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/ims_scf2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/afwa_snod2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/ghcn_snod2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/smap_ssm2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/smos_ssm2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/ascat_ssm2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/imsfv3_scf2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/smap9km_ssm2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/owp_snow_obs.py"
    )
endif()

