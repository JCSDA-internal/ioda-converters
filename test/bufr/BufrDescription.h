/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#define ECKIT_TESTING_SELF_REGISTER_CASES 0

#include <iostream>
#include <iomanip>
#include <memory>
#include <string>
#include <vector>
#include <set>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"

#include "oops/runs/Test.h"
#include "oops/util/Expect.h"
#include "oops/util/IntSetParser.h"
#include "oops/test/TestEnvironment.h"

#include "bufr/Ingester/BufrParser/BufrDescription.h"
#include "bufr/Ingester/BufrParser/BufrMnemonicSet.h"


namespace Ingester
{
    namespace test
    {
        void test_constructor()
        {
            const eckit::LocalConfiguration conf(::test::TestEnvironment::config());

            auto datapath = conf.getString("datapath");

            const auto& bufrConfs = conf.getSubConfigurations("bufr");

            if (bufrConfs.size() > 0)
            {
                auto description = Ingester::BufrDescription(bufrConfs.front(), datapath);
                unsigned int numConfSets = bufrConfs.front().getSubConfigurations("mnemonicSets").size();
                EXPECT(description.getMnemonicSets().size() == numConfSets);
            }
            else
            {
                throw eckit::BadValue("Configuration File is missing the \"bufr\" section.");
            }
        }

        void test_createDescriptionManually()
        {
            //Create Description
            auto description = BufrDescription();

            const std::string dummyFilePath = "/some/file/path";

            description.setFilepath(dummyFilePath);

            EXPECT(description.getMnemonicSets().size() == 0);
            EXPECT(description.filepath() == dummyFilePath);

            auto set1 = BufrMnemonicSet("SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS", {1});
            auto set2 = BufrMnemonicSet("SAZA SOZA BEARAZ SOLAZI", {1});
            auto set3 = BufrMnemonicSet("TMBR", oops::parseIntSet("1-15"));

            description.addMnemonicSet(set1);
            description.addMnemonicSet(set2);
            description.addMnemonicSet(set3);

            EXPECT(description.getMnemonicSets().front().getMnemonicStr() == "SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON CLATH CLONH HOLS");
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
                    test_constructor();
                });
                ts.emplace_back(CASE("ingester/BufrParser/testCreateDescriptionManually")
                {
                    test_createDescriptionManually();
                });
            }
        };
    }
}