#include <gtest/gtest.h>
#include <netcdf>
#include <fstream>
#include <cstdio>

#include "netcdf_file.h"
#include "netcdf_dimension.h"
#include "netcdf_error.h"

namespace {

    using namespace Obs2Ioda;

    class NetcdfAddDimTest : public ::testing::Test {
    protected:
        std::string filePath;
        int netcdfID;
        std::shared_ptr<netCDF::NcFile> file;

        void SetUp() override {
            filePath = "test_add_dim.nc";
            std::remove(filePath.c_str());

            // Create NetCDF file
            file = std::make_shared<netCDF::NcFile>(filePath, netCDF::NcFile::replace);
            netcdfID = file->getId();

            // Register in FileMap
            FileMap::getInstance().addFile(netcdfID, file);
        }

        void TearDown() override {
            try {
                FileMap::getInstance().removeFile(netcdfID);
            } catch (...) {}
            std::remove(filePath.c_str());
        }
    };

/**
 * @test AddsDimToRootGroup
 * @brief Tests adding a dimension to the root group of a NetCDF file.
 *
 * This test uses `netcdfAddDim` to add a named dimension ("Location") of fixed length
 * to the root group (no group name provided). It verifies that:
 * - The dimension is defined in the schema.
 * - The dimension is created successfully with a valid ID.
 * - The dimension exists in the root group with the expected size.
 */
    TEST_F(NetcdfAddDimTest, AddsDimToRootGroup) {
        const char *dimName = "Location";
        int dimLen = 10;
        int dimID = -1;

        auto dimInfo = iodaSchema.getDimension(dimName);
        ASSERT_NE(dimInfo, nullptr);  // schema must define this dimension

        int ret = netcdfAddDim(netcdfID, nullptr, dimName, dimLen, &dimID);
        EXPECT_EQ(ret, 0);
        EXPECT_GT(dimID, -1);

        auto dim = file->getDim(dimInfo->getValidName());
        EXPECT_TRUE(!dim.isNull());
        EXPECT_EQ(dim.getSize(), dimLen);
    }

/**
 * @test AddsDimToNamedGroup
 * @brief Tests adding a dimension to a specific named group within the NetCDF file.
 *
 * This test:
 * - Retrieves a group ("MetaData") and dimension ("Channel") from the schema.
 * - Creates the group in the file.
 * - Adds the dimension to that group using `netcdfAddDim`.
 * - Validates the dimension ID is valid and the dimension exists in the correct group
 *   with the expected size.
 */
    TEST_F(NetcdfAddDimTest, AddsDimToNamedGroup) {
        const char *groupName = "MetaData";
        const char *dimName = "Channel";
        int dimLen = 5;
        int dimID = -1;

        // Create the group first
        auto groupInfo = iodaSchema.getGroup(groupName);
        ASSERT_NE(groupInfo, nullptr);
        file->addGroup(groupInfo->getValidName());

        auto dimInfo = iodaSchema.getDimension(dimName);
        ASSERT_NE(dimInfo, nullptr);

        int ret = netcdfAddDim(netcdfID, groupInfo->getValidName().c_str(), dimName, dimLen, &dimID);
        EXPECT_EQ(ret, 0);
        EXPECT_GT(dimID, -1);

        auto group = file->getGroup(groupInfo->getValidName());
        auto dim = group.getDim(dimInfo->getValidName());
        EXPECT_TRUE(!dim.isNull());
        EXPECT_EQ(dim.getSize(), dimLen);
    }

}  // namespace
