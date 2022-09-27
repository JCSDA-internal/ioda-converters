

list(APPEND _bufr_deps
            eckit
            bufr::bufr_d)

list(APPEND _srcs
            print_queries.cpp
            ../../src/bufr/BufrParser/Query/DataProvider.h
            ../../src/bufr/BufrParser/Query/DataProvider.cpp
            ../../src/bufr/BufrParser/Query/SubsetTable.h
            ../../src/bufr/BufrParser/Query/SubsetTable.cpp)

ecbuild_add_executable( TARGET  print_queries.x
                        SOURCES ${_srcs}
                        LIBS ${_bufr_deps})
