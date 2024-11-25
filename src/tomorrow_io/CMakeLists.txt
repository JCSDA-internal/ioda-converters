# (C) Copyright 2024 The Tomorrow Companies, Inc.
#
# This software is licensed under the terms of the Apache Licence Version 2.0
# which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.

add_executable(convert_tio.x
	creation.cc
	creation_tio_sat_inst_specs.cc
	copy.h
	copy_function.cc
	copy_helpers.cc
	copy_tms.cc
	copy_tms_datetime.cc
	detect_tms_type.cc
	product.cc
	product.h
	main_tio.cc
	)
target_link_libraries(convert_tio.x PUBLIC ioda)
set_target_properties(convert_tio.x PROPERTIES CXX_STANDARD 17)

