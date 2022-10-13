

list(APPEND _bufr_deps
            eckit
            bufr::bufr_d)

list(APPEND _srcs
            print_queries.cpp
            ../../src/bufr/BufrParser/Query/QuerySet.h
            ../../src/bufr/BufrParser/Query/QuerySet.cpp
            ../../src/bufr/BufrParser/Query/QueryParser.h
            ../../src/bufr/BufrParser/Query/QueryParser.cpp
            ../../src/bufr/BufrParser/Query/DataProvider/DataProvider.h
            ../../src/bufr/BufrParser/Query/DataProvider/DataProvider.cpp
            ../../src/bufr/BufrParser/Query/DataProvider/NcepDataProvider.h
            ../../src/bufr/BufrParser/Query/DataProvider/NcepDataProvider.cpp
            ../../src/bufr/BufrParser/Query/DataProvider/WmoDataProvider.h
            ../../src/bufr/BufrParser/Query/DataProvider/WmoDataProvider.cpp
            ../../src/bufr/BufrParser/Query/SubsetTable.h
            ../../src/bufr/BufrParser/Query/SubsetTable.cpp)

ecbuild_add_executable( TARGET  print_queries.x
                        SOURCES ${_srcs}
                        LIBS ${_bufr_deps})
