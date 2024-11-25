#pragma once
/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "ioda/ObsGroup.h"

namespace tio_converter {

using DimensionRanges_t = std::vector<std::optional<std::pair<size_t, size_t>>>;

struct VariableInfo {
  mutable ioda::Variable var;  ///< The variable.
  /// Optionally defines a subset of indices along each axis. Bounds are inclusive on both sides.
  DimensionRanges_t range;
};

struct VariableDerivedInfo {
  /// Supplementary information about the dimensions of the variable.
  DimensionRanges_t range;
  /// The dimensions of the variable.
  ioda::Dimensions dims;
  /// The type of the variable's data. Ex: unsigned little-endian 32-bit integer.
  ioda::Type type;
  /// The starting indices for a hyperslab selection.
  std::vector<ioda::Dimensions_t> selection_start;
  /// The span along each axis for a hyperslab selection.
  std::vector<ioda::Dimensions_t> selection_count;
  /// The number of data elements in this selection.
  size_t selection_num_elements;
  /// The size, in bytes, needed to store the variable's data, accounting for the selection.
  size_t size_bytes;
  /// Selection from the file
  ioda::Selection selection_ioda;
  /// Selection within memory (starts at 0,0,0,...)
  ioda::Selection selection_membuf;

  VariableDerivedInfo();
  VariableDerivedInfo(const VariableInfo &);
  VariableDerivedInfo(const VariableInfo &, const DimensionRanges_t &);
};

/// @brief Get the fill value assigned to a variable as a vector of bytes, and optionally
///   convert to a different type representation.
/// @param var is the variable to be queried.
/// @param as_type is the desired return value's data type. Normally this is the source
///   variable's data type, but optionally you can convert to a different representation.
///   Useful when converting between differing source and destination types.
std::vector<char> get_fill_value(const ioda::Variable &var, std::optional<ioda::Type> as_type = {});

/// @brief Generic function to copy a hyperslab of data from one variable to another.
/// @param from is the specification of the source data. This includes the variable and the hyperslab.
/// @param to is the specification of the destination location.
void copy(const VariableInfo &from, const VariableInfo &to);

}  // namespace tio_converter
