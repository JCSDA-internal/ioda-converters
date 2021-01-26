# (C) Copyright 2021 UCAR
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

ecbuild_add_executable( TARGET  satbias2ioda.x
                        SOURCES satbias2ioda.cpp
                        LIBS    Eigen3::Eigen ${oops_LIBRARIES} ioda_engines )
