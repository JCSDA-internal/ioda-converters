/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <algorithm>
#include <cstring>
#include <exception>
#include <string>
#include <utility>
#include <vector>

#include "copy.h"
#include "hdf5.h"

namespace tio_converter {

void copy(const VariableInfo &from, const VariableInfo &to) {
  using namespace ioda;
  using std::byte;
  using std::max;
  using std::memcmp;
  using std::memcpy;
  using std::vector;

  VariableDerivedInfo from_info(from);
  VariableDerivedInfo to_info(to);

  vector<char> buffer(to_info.size_bytes);
  from.var.read(
    gsl::make_span(buffer.data(), buffer.size()),  // Read into the buffer
    to_info.type,  // Convert data into the destination data type (e.g. int, float, ...)
    from_info.selection_membuf,  // Needed to tell ioda how the data should be mapped into memory
    from_info.selection_ioda     // The hyperslab being read
  );

  vector<char> from_fill                    = get_fill_value(from.var, to_info.type);
  vector<char> to_fill                      = get_fill_value(to.var);
  const size_t buffer_size_of_element_bytes = to_info.type.getSize();
  // Iterate over buffer in buffer_size_of_element_bytes increments.
  // If we match from_fill_as_bytes_in_dest_representation, replace with the
  // contents of to_fill_as_bytes_in_dest_representation.
  for (size_t i = 0; i < buffer.size(); i += buffer_size_of_element_bytes) {
    if (!memcmp(buffer.data() + i, from_fill.data(), buffer_size_of_element_bytes))
      memcpy(buffer.data() + i, to_fill.data(), buffer_size_of_element_bytes);
  }

  to.var.write(gsl::make_span(buffer.data(), buffer.size()),  // Write from this buffer
               to_info.type,                                  // Output variable type
               to_info.selection_membuf,                      // Data mapping in memory
               to_info.selection_ioda                         // The hyperslab being written
  );
}

}  // namespace tio_converter
