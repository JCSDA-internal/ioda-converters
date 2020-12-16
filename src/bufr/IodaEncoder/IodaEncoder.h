/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <memory>

#include "eckit/config/LocalConfiguration.h"
#include "ioda/Group.h"
#include "ioda/Engines/Factory.h"
#include "ioda/ObsGroup.h"

#include "DataContainer.h"
#include "IodaDescription.h"


namespace Ingester
{
    /// \brief Uses IodaDescription and parsed data to create IODA data.
    class IodaEncoder
    {
     public:
        explicit IodaEncoder(const eckit::Configuration& conf);
        explicit IodaEncoder(const IodaDescription& description);

        /// \brief Encode the data into an ioda::ObsGroup object
        /// \param data The data container to use
        /// \param append Add data to existing file?
        std::map<SubCategory, ioda::ObsGroup> encode(const std::shared_ptr<DataContainer>& data,
                                                    bool append = false);

     private:
        /// \brief The description
        const IodaDescription description_;

        /// \brief Create a string from a template string.
        /// \param prototype A template string ex: "my {dogType} barks". Sections labeled {__key__}
        ///        are treated as keys into the dictionary that defines their replacment values.
        std::string makeStrWithSubstitions(const std::string& prototype,
                                           const std::map<std::string, std::string>& subMap);

        /// \brief Used to find indecies of { and } by the makeStrWithSubstitions method.
        /// \param str Template string to search.
        std::vector<std::pair<std::string, std::pair<int, int>>>
        findSubIdxs(const std::string& str);

        bool isInteger(const std::string& str) const;
    };
}  // namespace Ingester
