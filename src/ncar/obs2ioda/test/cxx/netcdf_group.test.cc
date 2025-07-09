#include <gtest/gtest.h>
#include <netcdf>
#include <fstream>
#include <cstdio>

#include "netcdf_file.h"
#include "netcdf_group.h"
#include "netcdf_error.h"

namespace {

    using namespace Obs2Ioda;

    class NetcdfAddGroupTest : public ::testing::Test {
    protected:
        std::string filePath;
        int netcdfID;
        std::shared_ptr<netCDF::NcFile> file;

        void SetUp() override {
            filePath = "test_group.nc";
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
 * @test AddsGroupToRoot
 * @brief Tests that a named group can be added to the root of the NetCDF file.
 *
 * This test:
 * - Retrieves group metadata from the schema for a known group ("ObsValue").
 * - Uses `netcdfAddGroup` with a `nullptr` parent to add it to the root group.
 * - Verifies the group was successfully created in the NetCDF file.
 */
    TEST_F(NetcdfAddGroupTest, AddsGroupToRoot) {
        // This group must exist in the schema
        auto validGroupName = "ObsValue";

        // Ensure schema has this group
        auto groupInfo = iodaSchema.getGroup(validGroupName);
        ASSERT_NE(groupInfo, nullptr);

        // Add group
        int ret = netcdfAddGroup(netcdfID, nullptr, validGroupName);
        EXPECT_EQ(ret, 0);

        // Validate it exists
        auto group = file->getGroup(groupInfo->getValidName());
        EXPECT_TRUE(group.isNull() == false);
    }
/**
 * @test AddsGroupToParent
 * @brief Tests that a subgroup can be added under a specified parent group.
 *
 * This test:
 * - Adds a parent group ("MetaData") to the file.
 * - Then adds a child group ("BiasCorrection") to that parent using `netcdfAddGroup`.
 * - Verifies both the parent and child groups exist in the file hierarchy.
 */
    TEST_F(NetcdfAddGroupTest, AddsGroupToParent) {
        // Add parent group first
        std::string parentName = "MetaData";
        auto parentInfo = iodaSchema.getGroup(parentName);
        ASSERT_NE(parentInfo, nullptr);
        file->addGroup(parentInfo->getValidName());

        // Add sub-group to it
        std::string childName = "BiasCorrection";
        auto childInfo = iodaSchema.getGroup(childName);
        ASSERT_NE(childInfo, nullptr);

        int ret = netcdfAddGroup(netcdfID, parentInfo->getValidName().c_str(), childName.c_str());
        EXPECT_EQ(ret, 0);

        auto parentGroup = file->getGroup(parentInfo->getValidName());
        auto subGroup = parentGroup.getGroup(childInfo->getValidName());
        EXPECT_TRUE(subGroup.isNull() == false);
    }
/**
 * @test NonExistantParentGroupThrowsAndIsCaught
 * @brief Tests that adding a group to a nonexistent parent group fails gracefully.
 *
 * This test attempts to add a group under a parent that does not exist in the file.
 * It expects `netcdfAddGroup` to return a negative value, indicating an error was caught
 * and reported correctly (e.g., via `netcdfErrorMessage`).
 */
    TEST_F(NetcdfAddGroupTest, NonExistantParentGroupThrowsAndIsCaught) {
        std::string invalidName = "ThisGroupDoesNotExist";

        int ret = netcdfAddGroup(netcdfID, invalidName.c_str(), invalidName.c_str());

        EXPECT_LT(ret, 0);  // netcdfErrorMessage returns -1 or error code
    }

}  // namespace
