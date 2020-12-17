/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include "DataObject.h"

#include <string>
#include <vector>


namespace Ingester
{
    /// \brief Container for data that can be expressed as lists of strings
    class StrVecDataObject : public DataObject
    {
     public:
        explicit StrVecDataObject(const std::vector<std::string>& strVector);
        ~StrVecDataObject() = default;

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

        /// \brief Get the number of rows represented in the data.
        size_t nrows() const final;

        /// \brief Get the number of columns represented in the data.
        size_t ncols() const final;

        // Getters
        inline std::vector<std::string> get() const { return strVector_; }

     private:
        /// \brief The data
        const std::vector<std::string> strVector_;

        /// \brief Create an ioda::VariableCreationParameters for the data
        /// \param chunks List of integers specifying the chunking dimensions
        /// \param compressionLevel The GZip compression level to use, must be 0-9
        static ioda::VariableCreationParameters makeCreationParams(
                                                    const std::vector<ioda::Dimensions_t>& chunks,
                                                    int compressionLevel);
    };
}  // namespace Ingester


