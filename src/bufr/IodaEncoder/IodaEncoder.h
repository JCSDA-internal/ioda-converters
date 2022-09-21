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
#include "ioda/Engines/EngineUtils.h"
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

        /// \brief Encode the data into an ioda::ObsGroup object
        /// \param data The data container to use
        /// \param append Add data to existing file?
        std::map<SubCategory, ioda::ObsGroup> encode(const std::shared_ptr<DataContainer>& data,
                                                    bool append = false);

     private:
        typedef std::map<std::vector<std::string>, std::string> NamedPathDims;

        /// \brief The description
        const IodaDescription description_;

        /// \brief Create a string from a template string.
        /// \param prototype A template string ex: "my {dogType} barks". Sections labeled {__key__}
        ///        are treated as keys into the dictionary that defines their replacment values.
        std::string makeStrWithSubstitions(const std::string& prototype,
                                           const std::map<std::string, std::string>& subMap);

        /// \brief Used to find indicies of { and } by the makeStrWithSubstitions method.
        /// \param str Template string to search.
        std::vector<std::pair<std::string, std::pair<int, int>>>
        findSubIdxs(const std::string& str);

        /// \brief Check if the subquery string is a named dimension.
        /// \param path The subquery string to check.
        /// \param pathMap The map of named dimensions.
        /// \return True if the subquery string is a named dimension.
        bool existsInNamedPath(const std::string& path, const NamedPathDims& pathMap) const;

        /// \brief Get the name associated with the named dimension.
        /// \param path The subquery string for the dimension.
        /// \param pathMap The map of named dimensions.
        /// \return The name associated with the named dimension.
        std::string nameForDimPath(const std::string& path, const NamedPathDims& pathMap) const;
    };
}  // namespace Ingester
