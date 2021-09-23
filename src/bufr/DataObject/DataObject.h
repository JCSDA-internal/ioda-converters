/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <set>
#include <string>

#include "ioda/ObsGroup.h"
#include "ioda/defs.h"


namespace Ingester
{
    /// \brief Abstract base class for intermediate data object that bridges the Parsers with the
    /// IodaEncoder.
    class DataObject
    {
     public:
        /// \brief Makes an ioda::Variable and ads it to the given ioda::ObsGroup
        /// \param obsGroup Obsgroup were to add the variable
        /// \param name The name to associate with the variable (ex "latitude@MetaData")
        /// \param dimensions List of Variables to use as the dimensions for this new variable
        /// \param chunks List of integers specifying the chunking dimensions
        /// \param compressionLevel The GZip compression level to use, must be 0-9
        virtual ioda::Variable createVariable(
                                      ioda::ObsGroup& obsGroup,
                                      const std::string& name,
                                      const std::vector<ioda::Variable>& dimensions,
                                      const std::vector<ioda::Dimensions_t>& chunks,
                                      int compressionLevel) = 0;

        /// \brief Print data to stdout for debug purposes.
        virtual void print() const = 0;

        /// \brief Get number of columns represented in the data.
        virtual size_t ncols() const = 0;

        /// \brief Get number of rows represented in the data.
        virtual size_t nrows() const = 0;

        /// \brief Get float value for a given row and column.
        /// \param row Row number to check
        /// \param col Column number to check
        /// \return Float value at the given row and column.
        virtual float getFloat(size_t row, size_t col) const = 0;

        /// \brief Get string value for a given row and column.
        /// \param row Row number to check
        /// \param col Column number to check
        /// \return String value at the given row and column.
        virtual std::string getString(size_t row, size_t col) const = 0;

        /// \brief Get int value for a given row and column.
        /// \param row Row number to check
        /// \param col Column number to check
        /// \return Int value at the given row and column.
        virtual int getInt(size_t row, size_t col) const = 0;

        /// \brief Get vector of float values for a given column.
        /// \param col Column number
        /// \return Vector of float values at the given column.
        virtual std::vector<float> getFloats(size_t col=0) const = 0;

        /// \brief Get vector of string values for a given column.
        /// \param col Column number
        /// \return Vector of string values at the given column.
        virtual std::vector<std::string> getStrings(size_t col=0) const = 0;

        /// \brief Get vector of int values for a given column.
        /// \param col Column number
        /// \return Vector of int values at the given column.
        virtual std::vector<int> getInts(size_t col=0) const = 0;  

        /// \brief Slice the data object given a vector of row indices.
        /// \param rowIndices Vector of row indices to slice on.
        /// \return Slice of the data object.
        virtual std::shared_ptr<DataObject> slice(const std::vector<size_t>& rowIndices) const = 0;      
    };
}  // namespace Ingester


