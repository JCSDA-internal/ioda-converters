/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <iostream>
#include <iomanip>
#include <memory>
#include <string>
#include <vector>
#include <set>

#include "eckit/config/LocalConfiguration.h"

#define ECKIT_TESTING_SELF_REGISTER_CASES 0
#include "eckit/testing/Test.h"

#include "oops/runs/Test.h"
#include "oops/util/Expect.h"
#include "oops/test/TestEnvironment.h"

#include "bufr/Ingester/BufrParser/BufrDescription.h"
#include "bufr/Ingester/BufrParser/BufrMnemonicSet.h"




namespace Ingester
{
    namespace test
    {
        void testConstructor()
        {
            const eckit::LocalConfiguration conf(::test::TestEnvironment::config());

            auto datapath = conf.getString("datapath");

            for (const auto& bufrConf : conf.getSubConfigurations("bufr"))
            {
                auto description = Ingester::BufrDescription(bufrConf, datapath);
                unsigned int numConfSets = bufrConf.getSubConfigurations("mnemonicSets").size();
                EXPECT(description.getMnemonicSets().size() == numConfSets);
            }
        }

        class BufrDescription : public oops::Test
        {
        public:
            BufrDescription() {}
            virtual ~BufrDescription() {}
        private:
            std::string testid() const override { return "ingester::test::BufrParser"; }
            void register_tests() const override
            {
                std::vector<eckit::testing::Test>& ts = eckit::testing::specification();

                ts.emplace_back(CASE("ingester/BufrParser/testConstructor")
                {
                    testConstructor();
                });
            }
        };
    }
}