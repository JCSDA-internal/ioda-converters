/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#define ECKIT_TESTING_SELF_REGISTER_CASES 0

#include <iomanip>
#include <iostream>
#include <memory>
#include <set>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"
#include "oops/runs/Test.h"
#include "oops/test/TestEnvironment.h"
#include "oops/util/Expect.h"
#include "oops/util/IntSetParser.h"

#include "BufrParser/BufrDescription.h"
#include "IodaEncoder/IodaDescription.h"


namespace Ingester
{
    namespace test
    {
        void test_constructor()
        {
            const eckit::LocalConfiguration conf(::test::TestEnvironment::config());

            if (conf.has("observations"))
            {
                for (const auto &obsConf : conf.getSubConfigurations("observations"))
                {
                    if (obsConf.has("obs space") &&
                        obsConf.getSubConfiguration("obs space").has("name") &&
                        obsConf.getSubConfiguration("obs space").getString("name") == "bufr")
                    {
                        auto bufrConf = obsConf.getSubConfiguration("obs space");
                        auto description = Ingester::BufrDescription(bufrConf);

                        // EXPECT(description.getMnemonicSets().size() > 0);
                        EXPECT(description.getExport().getVariables().size() > 0);
                    }
                    else
                    {
                        throw eckit::BadValue(
                            "Configuration File is missing the \"bufr\" section.");
                    }

                    if (obsConf.has("ioda"))
                    {
                        auto iodaConf = obsConf.getSubConfiguration("ioda");
                        auto description = Ingester::IodaDescription(iodaConf);

                        EXPECT(description.getDims().size() > 0);
                        EXPECT(description.getVariables().size() > 0);
                    }
                    else
                    {
                        throw eckit::BadValue(
                            "Configuration File is missing the \"ioda\" section.");
                    }
                }
            }
        }

        class BufrDescription : public oops::Test
        {
         public:
            BufrDescription() = default;
            virtual ~BufrDescription() = default;
         private:
            std::string testid() const override { return "ingester::test::BufrParser"; }
            void register_tests() const override
            {
                std::vector<eckit::testing::Test>& ts = eckit::testing::specification();

                ts.emplace_back(CASE("ingester/BufrParser/testConstructor")
                {
                    test_constructor();
                });
            }

            void clear() const override
            {
            }
        };
    }  // namespace test
}  // namespace Ingester
