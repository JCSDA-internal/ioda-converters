#include <netcdf>
#include <fstream>
#include <cstdio>

#include "netcdf_file.h"
#include "netcdf_group.h"
#include "netcdf_utils.h"

#include "eckit/testing/Test.h"

using namespace eckit::testing;
using namespace Obs2Ioda;

namespace {

struct SetNetcdfGroupFixture {
    std::string filePath;
    std::string groupName;
    int netcdfID;
    std::shared_ptr<netCDF::NcFile> file;
    std::shared_ptr<netCDF::NcGroup> refGroup;

    SetNetcdfGroupFixture() : filePath("test_set_netcdf_group.nc"), groupName("group"), netcdfID(-1) {
        std::remove(filePath.c_str());

        // Create NetCDF file
        file = std::make_shared<netCDF::NcFile>(filePath, netCDF::NcFile::replace);
        netcdfID = file->getId();

        FileMap::getInstance().addFile(netcdfID, file);
        refGroup = std::make_shared<netCDF::NcGroup>(file->addGroup(groupName));
    }

    ~SetNetcdfGroupFixture() {
        try {
            FileMap::getInstance().removeFile(netcdfID);
        } catch (...) {}
        std::remove(filePath.c_str());
    }
};

} // namespace

//--------------------------------------------------------------------
// Set group for global component
//--------------------------------------------------------------------

CASE("SetNetcdfGroup - SetGroupForGlobalComponent") {
    SetNetcdfGroupFixture f;

    auto group = Obs2Ioda::setNetcdfGroup(f.file, "");
    EXPECT(f.file == group);
}

//--------------------------------------------------------------------
// Set group for group-level component
//--------------------------------------------------------------------

CASE("SetNetcdfGroup - SetGroupForGroupLevelComponent") {
    SetNetcdfGroupFixture f;

    auto group = Obs2Ioda::setNetcdfGroup(f.file, f.groupName.c_str());
    EXPECT(f.refGroup->getName() == group->getName());
}

//--------------------------------------------------------------------
// Set group throws for nullptr group name
//--------------------------------------------------------------------

CASE("SetNetcdfGroup - SetGroupThrowsForNullptrGroupName") {
    SetNetcdfGroupFixture f;

    EXPECT_THROWS_AS(
        Obs2Ioda::setNetcdfGroup(f.file, nullptr),
        netCDF::exceptions::NcBadGroupId
    );
}

//--------------------------------------------------------------------
// Set group throws for non-existent group name
//--------------------------------------------------------------------

CASE("SetNetcdfGroup - SetGroupThrowsForNonExistentGroupName") {
    SetNetcdfGroupFixture f;

    EXPECT_THROWS_AS(
        Obs2Ioda::setNetcdfGroup(f.file, "foo"),
        netCDF::exceptions::NcBadGroupId
    );
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char* argv[]) {
    return run_tests(argc, argv);
}
