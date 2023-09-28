/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include <string>
#include <iostream>
#include <ostream>

#include "eckit/config/YAMLConfiguration.h"
#include "eckit/exception/Exceptions.h"
#include "eckit/filesystem/PathName.h"

#include "BufrParser/BufrParser.h"
#include "ObjectFactory.h"
#include "IodaEncoder/IodaDescription.h"
#include "IodaEncoder/IodaEncoder.h"


namespace Ingester
{
    typedef ObjectFactory<Ingester::Parser, const eckit::LocalConfiguration&> ParseFactory;

    std::shared_ptr<DataContainer> parse1(const std::string& yamlPath, std::size_t numMsgs = 0);

    void encode_save(const std::string& yamlPath, std::shared_ptr<DataContainer> data);
}

