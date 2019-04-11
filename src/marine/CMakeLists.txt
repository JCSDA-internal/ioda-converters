list(APPEND programs
  emc_ice2ioda.py
  gds2_sst2ioda.py
  gmao_obs2ioda.py
  godae_profile2ioda.py
  godae_ship2ioda.py
  godae_trak2ioda.py
  hgodas_adt2ioda.py
  hgodas_insitu2ioda.py
  hgodas_sst2ioda.py
  rads_adt2ioda.py
  smap_sss2ioda.py
)

COPY_FILES( "${programs}" ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_BINARY_DIR}/bin )

install (PROGRAMS
  ${programs}
  DESTINATION bin)


ecbuild_add_test( TARGET iodaconv_marine_coding_norms
                  TYPE
                  COMMAND ${CMAKE_BINARY_DIR}/bin/lint.sh
                  ARGS ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_SOURCE_DIR} )
