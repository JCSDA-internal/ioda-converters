/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

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

#include "bufr/Ingester/BufrParser/BufrTypes.h"
#include "bufr/Ingester/BufrParser/BufrParser.h"


namespace Ingester
{
    namespace test
    {
        void testConstructor()
        {
            util::DateTime bgn((::test::TestEnvironment::config().getString("window begin")));
            util::DateTime end((::test::TestEnvironment::config().getString("window end")));

            auto bufrParser = BufrParser();
        }

        class BufrParser : public oops::Test {
        private:
            std::string testid() const override {return "ingester::test::BufrParser";}
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