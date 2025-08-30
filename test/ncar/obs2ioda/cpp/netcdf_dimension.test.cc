#include <netcdf>
#include <fstream>
#include <cstdio>

#include "netcdf_file.h"
#include "netcdf_dimension.h"
#include "netcdf_error.h"

#include "eckit/testing/Test.h"

using namespace eckit::testing;
using namespace Obs2Ioda;

namespace {

struct NetcdfAddDimFixture {
    std::string filePath;
    int netcdfID;
    std::shared_ptr<netCDF::NcFile> file;

    NetcdfAddDimFixture() : filePath("test_add_dim.nc"), netcdfID(-1) {
        std::remove(filePath.c_str());

        // Create NetCDF file
        file = std::make_shared<netCDF::NcFile>(filePath, netCDF::NcFile::replace);
        netcdfID = file->getId();

        // Register in FileMap
        FileMap::getInstance().addFile(netcdfID, file);
    }

    ~NetcdfAddDimFixture() {
        try {
            FileMap::getInstance().removeFile(netcdfID);
        } catch (...) {}
        std::remove(filePath.c_str());
    }
};

} // namespace

//--------------------------------------------------------------------
// Adds dimension to root group
//--------------------------------------------------------------------

CASE("NetcdfAddDim - AddsDimToRootGroup") {
    NetcdfAddDimFixture f;

    const char *dimName = "Location";
    int dimLen = 10;
    int dimID = -1;

    auto dimInfo = iodaSchema.getDimension(dimName);
    EXPECT(dimInfo != nullptr);

    int ret = netcdfAddDim(f.netcdfID, "", dimName, dimLen, &dimID);
    EXPECT(ret == 0);
    EXPECT(dimID > -1);

    auto dim = f.file->getDim(dimInfo->getValidName());
    EXPECT(!dim.isNull());
    EXPECT(dim.getSize() == dimLen);
}

//--------------------------------------------------------------------
// Adds dimension to named group
//--------------------------------------------------------------------

CASE("NetcdfAddDim - AddsDimToNamedGroup") {
    NetcdfAddDimFixture f;

    const char *groupName = "MetaData";
    const char *dimName = "Channel";
    int dimLen = 5;
    int dimID = -1;

    // Create the group first
    auto groupInfo = iodaSchema.getGroup(groupName);
    EXPECT(groupInfo != nullptr);
    f.file->addGroup(groupInfo->getValidName());

    auto dimInfo = iodaSchema.getDimension(dimName);
    EXPECT(dimInfo != nullptr);

    int ret = netcdfAddDim(f.netcdfID, groupInfo->getValidName().c_str(),
                           dimName, dimLen, &dimID);
    EXPECT(ret == 0);
    EXPECT(dimID > -1);

    auto group = f.file->getGroup(groupInfo->getValidName());
    auto dim = group.getDim(dimInfo->getValidName());
    EXPECT(!dim.isNull());
    EXPECT(dim.getSize() == dimLen);
}

//--------------------------------------------------------------------
// AddDim with null group name returns error
//--------------------------------------------------------------------

CASE("NetcdfAddDim - AddDimWithNullGroupNameReturnsError") {
    NetcdfAddDimFixture f;

    int dimID = -1;
    int ret = netcdfAddDim(f.netcdfID, nullptr, "InvalidDim", 10, &dimID);

    EXPECT(ret == -116);   // Specific error for null group name
    EXPECT(dimID == -1);   // ID should not be set
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char* argv[]) {
    return run_tests(argc, argv);
}
