#include <netcdf>
#include <fstream>
#include <cstdio>

#include "netcdf_file.h"
#include "netcdf_group.h"
#include "netcdf_error.h"

#include "eckit/testing/Test.h"

using namespace eckit::testing;
using namespace Obs2Ioda;

namespace {

struct NetcdfAddGroupFixture {
    std::string filePath;
    int netcdfID;
    std::shared_ptr<netCDF::NcFile> file;

    NetcdfAddGroupFixture() : filePath("test_group.nc"), netcdfID(-1) {
        std::remove(filePath.c_str());

        // Create NetCDF file
        file = std::make_shared<netCDF::NcFile>(filePath, netCDF::NcFile::replace);
        netcdfID = file->getId();

        // Register in FileMap
        FileMap::getInstance().addFile(netcdfID, file);
    }

    ~NetcdfAddGroupFixture() {
        try {
            FileMap::getInstance().removeFile(netcdfID);
        } catch (...) {}
        std::remove(filePath.c_str());
    }
};

} // namespace

//--------------------------------------------------------------------
// Add group to root
//--------------------------------------------------------------------

CASE("NetcdfAddGroup - Adds group to root") {
    NetcdfAddGroupFixture f;

    auto validGroupName = "ObsValue";

    // Ensure schema has this group
    auto groupInfo = iodaSchema.getGroup(validGroupName);
    EXPECT(groupInfo != nullptr);

    // Add group
    int ret = netcdfAddGroup(f.netcdfID, "", validGroupName);
    EXPECT(ret == 0);

    // Validate it exists
    auto group = f.file->getGroup(groupInfo->getValidName());
    EXPECT(!group.isNull());
}

//--------------------------------------------------------------------
// Add subgroup to parent
//--------------------------------------------------------------------

CASE("NetcdfAddGroup - Adds group to parent") {
    NetcdfAddGroupFixture f;

    std::string parentName = "MetaData";
    auto parentInfo = iodaSchema.getGroup(parentName);
    EXPECT(parentInfo != nullptr);

    // Add parent group
    f.file->addGroup(parentInfo->getValidName());

    // Add child group
    std::string childName = "BiasCorrection";
    auto childInfo = iodaSchema.getGroup(childName);
    EXPECT(childInfo != nullptr);

    int ret = netcdfAddGroup(f.netcdfID, parentInfo->getValidName().c_str(), childName.c_str());
    EXPECT(ret == 0);

    auto parentGroup = f.file->getGroup(parentInfo->getValidName());
    auto subGroup = parentGroup.getGroup(childInfo->getValidName());
    EXPECT(!subGroup.isNull());
}

//--------------------------------------------------------------------
// Non-existent parent returns error
//--------------------------------------------------------------------

CASE("NetcdfAddGroup - Non-existent parent group returns error") {
    NetcdfAddGroupFixture f;

    std::string invalidName = "ThisGroupDoesNotExist";
    int ret = netcdfAddGroup(f.netcdfID, invalidName.c_str(), invalidName.c_str());
    EXPECT(ret < 0);  // Should be an error code
}

//--------------------------------------------------------------------
// Null parent group name returns error
//--------------------------------------------------------------------

CASE("NetcdfAddGroup - Null parent group name returns error") {
    NetcdfAddGroupFixture f;

    int ret = netcdfAddGroup(f.netcdfID, nullptr, "InvalidGroup");
    EXPECT(ret == -116);  // Specific error for null parent
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char* argv[]) {
    return run_tests(argc, argv);
}
