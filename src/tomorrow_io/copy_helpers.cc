/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <algorithm>

#include "copy.h"
#include "hdf5.h"

namespace tio_converter {

namespace {
// This only returns predefined HDF5 types, and those hid_ts are static objects.
// NOTE (RH): ioda really should be extended to return the endianness of data.
//            We assume little endian until this is fixed.
hid_t get_hdf5_type(const ioda::Type &typ) {
  using namespace ioda;
  using std::logic_error;
  const size_t len    = typ.getSize();
  const TypeClass cls = typ.getClass();
  if (cls == TypeClass::Integer) {
    bool sgn = typ.isTypeSigned();
    if (sgn && (len == 1)) return H5T_STD_I8LE;
    if (!sgn && (len == 1)) return H5T_STD_U8LE;
    if (sgn && (len == 2)) return H5T_STD_I16LE;
    if (!sgn && (len == 2)) return H5T_STD_U16LE;
    if (sgn && (len == 4)) return H5T_STD_I32LE;
    if (!sgn && (len == 4)) return H5T_STD_U32LE;
    if (sgn && (len == 8)) return H5T_STD_I64LE;
    if (!sgn && (len == 8)) return H5T_STD_U64LE;
  } else if (cls == TypeClass::Float) {
#ifdef H5T_NATIVE_FLOAT16  // Introduced in recent HDF5 versions
    if (len == 2) return H5T_IEEE_F16LE;
#endif
    if (len == 4) return H5T_IEEE_F32LE;
    if (len == 8) return H5T_IEEE_F64LE;
  }
  throw logic_error("Unsupported object type.");
}
}  // namespace

VariableDerivedInfo::VariableDerivedInfo() = default;
VariableDerivedInfo::VariableDerivedInfo(const VariableInfo &vi)
    : VariableDerivedInfo(vi, vi.range) {}
VariableDerivedInfo::VariableDerivedInfo(const VariableInfo &vi, const DimensionRanges_t &di) {
  using ioda::Dimensions_t;
  using ioda::SelectionOperator;
  using std::accumulate;
  using std::multiplies;
  using std::vector;

  range = di;
  dims  = vi.var.getDimensions();
  type  = vi.var.getType();

  vector<Dimensions_t> zero_starts(dims.dimensionality);
  selection_start.resize(dims.dimensionality);
  selection_count.resize(dims.dimensionality);

  for (size_t i = 0; i < dims.dimensionality; ++i) {
    if (range.size() > i && range[i]) {
      selection_start[i] = range[i]->first;
      selection_count[i] = range[i]->second - range[i]->first + 1;
    } else {
      selection_start[i] = 0;
      selection_count[i] = dims.dimsCur[i];
    }
  }
  selection_ioda.extent(dims.dimsCur)
    .select({SelectionOperator::SET, selection_start, selection_count});
  selection_membuf.extent(selection_count)
    .select({SelectionOperator::SET, zero_starts, selection_count});

  // Determine the size of a buffer needed to read this data in its entirety.
  // This is just selection_count.
  selection_num_elements
    = accumulate(selection_count.begin(), selection_count.end(), 1, multiplies<size_t>());
  const size_t size_of_element_bytes = type.getSize();
  size_bytes                         = selection_num_elements * size_of_element_bytes;
}

std::vector<char> get_fill_value(const ioda::Variable &var, std::optional<ioda::Type> as_type) {
  using std::logic_error;
  using std::max;
  using std::memcpy;
  using std::vector;
  const size_t len_bytes_src = var.getType().getSize();
  vector<char> fill_bytes(len_bytes_src);
  // BUG (RH): ioda's getFillValue is very slightly buggy in that it reports a
  // spurious warning when reading the fill value of the TMS L1B MultiMask variable,
  // which has an unsigned char data type.
  // "ioda::Variable: hdf and netcdf fill value specifications do not match"
  // In this case, we can just read the _FillValue attribute directly.
  if (var.atts.exists("_FillValue")) {
    ioda::Attribute fvAttr = var.atts.open("_FillValue");
    fvAttr.read(gsl::make_span(fill_bytes.data(), fill_bytes.size()), fvAttr.getType());
  } else {
    const auto src_fill = var.getFillValue();
    memcpy(fill_bytes.data(), &(src_fill.fillValue_.ui64), len_bytes_src);
  }

  if (!as_type) return fill_bytes;

  const size_t len_bytes_to = as_type->getSize();
  const hid_t h5type_from   = get_hdf5_type(var.getType());
  const hid_t h5type_to     = get_hdf5_type(*as_type);
  fill_bytes.resize(max(len_bytes_src, len_bytes_to));

  herr_t cvt_res = H5Tconvert(h5type_from, h5type_to,
                              1,  // Only one 'element' to be converted
                              fill_bytes.data(), nullptr, H5P_DEFAULT);
  if (cvt_res < 0) throw logic_error("Fill value type conversion failed.");
  fill_bytes.resize(len_bytes_to);
  return fill_bytes;
}

}  // namespace tio_converter
