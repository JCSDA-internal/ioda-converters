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

        size_t nrows() const final;
        size_t ncols() const final;

        // Getters
        inline IngesterArray get() const { return eigArray_; }

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


