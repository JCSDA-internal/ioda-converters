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
#include "BufrParser/BufrMnemonicSet.h"
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

                        EXPECT(description.getMnemonicSets().size() > 0);
                        EXPECT(description.getExportMap().size() > 0);
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

        void test_createDescriptionManually()
        {
            // Create Description
            auto bufrDesc = BufrDescription();

            const std::string dummyFilePath = "/some/file/path";

            bufrDesc.setFilepath(dummyFilePath);

            EXPECT(bufrDesc.getMnemonicSets().size() == 0);
            EXPECT(bufrDesc.filepath() == dummyFilePath);

            std::vector<std::string> set1Mnemonic = {"SAID",
                                                     "FOVN",
                                                     "YEAR",
                                                     "MNTH",
                                                     "DAYS",
                                                     "HOUR",
                                                     "MINU",
                                                     "SECO",
                                                     "CLAT",
                                                     "CLON",
                                                     "CLATH",
                                                     "CLONH",
                                                     "HOLS"};
            std::vector<std::string> set2Mnemonic = {"SAZA", "SOZA", "BEARAZ", "SOLAZI"};
            std::vector<std::string> set3Mnemonic = {"TMBR"};

            auto set1 = Ingester::BufrMnemonicSet(set1Mnemonic, {1});
            auto set2 = Ingester::BufrMnemonicSet(set2Mnemonic, {1});
            auto set3 = Ingester::BufrMnemonicSet(set3Mnemonic, oops::parseIntSet("1-15"));

            bufrDesc.addMnemonicSet(set1);
            bufrDesc.addMnemonicSet(set2);
            bufrDesc.addMnemonicSet(set3);

            EXPECT(bufrDesc.getMnemonicSets().size() > 0);

            auto iodaDesc = IodaDescription();

            DimensionDescription scale;
            scale.name = "scale_name";
            scale.size = "1";

            VariableDescription varDesc;
            varDesc.name = "variable_desc";
            varDesc.source = "source";
            varDesc.dimensions = {"1"};
            varDesc.longName = "long_variable_name";
            varDesc.units = "units";

            iodaDesc.addDimension(scale);
            iodaDesc.addVariable(varDesc);

            EXPECT(iodaDesc.getDims().size() > 0);
            EXPECT(iodaDesc.getVariables().size() > 0);
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
                ts.emplace_back(CASE("ingester/BufrParser/testCreateDescriptionManually")
                {
                    test_createDescriptionManually();
                });
            }

            void clear() const override
            {
            }
        };
    }  // namespace test
}  // namespace Ingester
