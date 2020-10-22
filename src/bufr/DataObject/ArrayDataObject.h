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
        ioda::Variable createVariable(ioda::ObsGroup& obsGroup,
                                      const std::string& name,
                                      const std::vector<ioda::Variable>& dimensions) final;

        /// \brief Print data to stdout for debug purposes.
        void print() const final;

        //Getters
        inline IngesterArray get() const { return eigArray_; }

     private:
        /// \brief Eigen Array that holds the data
        const IngesterArray eigArray_;

        /// \brief Create an ioda::VariableCreationParameters for the data.
        static ioda::VariableCreationParameters makeCreationParams();
    };
}  // namespace Ingester


