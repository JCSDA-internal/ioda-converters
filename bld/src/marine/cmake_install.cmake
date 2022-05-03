# Install script for directory: /home/ubuntu/jedi/ioda-bundle/iodaconv/src/marine

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
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/copernicus_l4adt2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/copernicus_l3swh2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/cryosat_ice2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/emc_ice2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/gds2_sst2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/gmao_obs2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/godae_profile2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/godae_ship2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/godae_trak2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/hgodas_adt2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/hgodas_insitu2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/hgodas_sst2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/glider2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/ndbc_hfradar2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/rads_adt2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/smap_sss2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/smos_sss2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/argoClim2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/viirs_modis_l2_oc2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/viirs_modis_l3_oc2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/godae_bgc_argo2ioda.py"
    "/home/ubuntu/jedi/ioda-bundle/iodaconv/bld/install-bin/avhrr_radiance2ioda.py"
    )
endif()

