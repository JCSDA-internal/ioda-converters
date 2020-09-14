/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#define ECKIT_TESTING_SELF_REGISTER_CASES 0

#include <iomanip>
#include <memory>
#include <string>
#include <vector>

#include "eckit/config/LocalConfiguration.h"
#include "eckit/testing/Test.h"
#include "oops/parallel/mpi/mpi.h"
#include "oops/runs/Test.h"
#include "oops/util/Expect.h"
#include "test/TestEnvironment.h"

#include "bufr/BufrParser/BufrDescription.h"
#include "bufr/BufrParser/BufrMnemonicSet.h"
#include "bufr/BufrParser/BufrParser.h"
#include "bufr/BufrParser/BufrTypes.h"
#include "bufr/DataContainer.h"


namespace Ingester
{
    namespace test
    {
        class BufrParserTestFixture : private boost::noncopyable
        {
         public:
            static std::shared_ptr<BufrParser>& bufrParser() { return getInstance().bufrParser_; }

         private:
            std::shared_ptr<BufrParser> bufrParser_;

            static BufrParserTestFixture& getInstance()
            {
                static BufrParserTestFixture bufrParserTestFixture;
                return bufrParserTestFixture;
            }

            BufrParserTestFixture()
            {
                const eckit::LocalConfiguration conf(::test::TestEnvironment::config());

                auto datapath = conf.getString("datapath");

                const auto& bufrConfs = conf.getSubConfigurations("bufr");

                if (bufrConfs.size() > 0)
                {
                    auto description = Ingester::BufrDescription(bufrConfs.front(), datapath);
                    bufrParser_ = std::make_shared<Ingester::BufrParser> (description);
                }
                else
                {
                    throw eckit::BadValue("Configuration File is missing the \"bufr\" section.");
                }
            }

            ~BufrParserTestFixture() = default;
        };


        void test_constructor()
        {
            BufrParserTestFixture::bufrParser();
        }

        void test_parsePartialFile()
        {
            auto data = BufrParserTestFixture::bufrParser()->parse(5);
            EXPECT(abs(data->get("TMBR")(0, 0) - 248.17) < .01);
        }

        void test_parseFileIncrementally()
        {
            bool endReached = false;
            std::shared_ptr<DataContainer> data;
            do
            {
                auto nextData = BufrParserTestFixture::bufrParser()->parse(10);

                if (nextData->size() > 0)
                {
                    data = nextData;
                }
                else
                {
                    endReached = true;
                }
            } while (!endReached);
        }


        class BufrParser : public oops::Test
        {
         public:
            BufrParser() = default;
            ~BufrParser() override = default;
         private:
            std::string testid() const override { return "ingester::test::BufrParser"; }
            void register_tests() const override
            {
                std::vector<eckit::testing::Test>& ts = eckit::testing::specification();

                ts.emplace_back(CASE("ingester/BufrParser/testConstructor")
                {
                    test_constructor();
                });
                ts.emplace_back(CASE("ingester/BufrParser/testParsePartialFile")
                {
                    test_parsePartialFile();
                });
                ts.emplace_back(CASE("ingester/BufrParser/testParseFileIncrementally")
                {
                    test_parseFileIncrementally();
                });
            }
        };
    }  // namespace test
}  // namespace Ingester
