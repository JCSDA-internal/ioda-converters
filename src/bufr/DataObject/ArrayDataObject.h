/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "DataObject.h"
#include "IngesterTypes.h"


namespace Ingester
{
    /// \brief Container for Parser data that is expressed as a Eigen Array of doubles.
    class ArrayDataObject : public DataObject
    {
     public:
        explicit ArrayDataObject(const IngesterArray& eigArray);
        ~ArrayDataObject() = default;

        /// \brief Makes an ioda::Variable and ads it to the given ioda::ObsGroup
        /// \param obsGroup Obsgroup were to add the variable
        /// \param name The name to associate with the variable (ex "latitude@MetaData")
        /// \param dimensions List of Variables to use as the dimensions for this new variable
        /// \param chunks List of integers specifying the chunking dimensions
        /// \param compressionLevel The GZip compression level to use, must be 0-9
        ioda::Variable createVariable(ioda::ObsGroup& obsGroup,
                                      const std::string& name,
                                      const std::vector<ioda::Variable>& dimensions,
                                      const std::vector<ioda::Dimensions_t>& chunks,
                                      int compressionLevel) final;

        /// \brief Print data to stdout for debug purposes.
        void print() const final;

        /// \brief Get number of rows represented in the data
        size_t nrows() const final;

        /// \brief Get number of columns represented in the data
        size_t ncols() const final;

        // Getters
        inline IngesterArray get() const { return eigArray_; }

        /// \brief Get float value for a given row and column.
        float getFloat(size_t row, size_t col) const final;

        /// \brief Get string value for a given row and column.
        std::string getString(size_t row, size_t col) const final;

        /// \brief Get eigen array
        IngesterArray getEigenArray() const { return eigArray_; }

        /// \brief Get int value for a given row and column.
        /// \param row Row number 
        /// \param col Column number 
        /// \return Int value at the given row and column.
        int getInt(size_t row, size_t col) const final;

        /// \brief Get vector of float values for a given column.
        /// \param col Column number 
        /// \return Vector of float values at the given column.
        std::vector<float> getFloats(size_t col=0) const final;

        /// \brief Get vector of string values for a given column.
        /// \param col Column number 
        /// \return Vector of string values at the given column.
        std::vector<std::string> getStrings(size_t col=0) const final;

        /// \brief Get vector of int values for a given column.
        /// \param col Column number 
        /// \return Vector of int values at the given column.
        std::vector<int> getInts(size_t col=0) const final;

        /// \brief Slice the data object given a vector of row indices.
        /// \param rowIndices Vector of row indices to slice on.
        /// \return Slice of the data object.
        std::shared_ptr<DataObject> slice(const std::vector<size_t>& rowIndices) const final;      

     private:
        /// \brief Eigen Array that holds the data
        const IngesterArray eigArray_;

        /// \brief Create an ioda::VariableCreationParameters for the data.
        /// \param chunks List of integers specifying the chunking dimensions
        /// \param compressionLevel The GZip compression level to use, must be 0-9
        static ioda::VariableCreationParameters makeCreationParams(
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel);
    };
}  // namespace Ingester


