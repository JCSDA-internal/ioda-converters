#include <netcdf>
#include <cstdio>
#include "netcdf_file.h"

#include "eckit/testing/Test.h"

using namespace eckit::testing;

namespace {

struct FileMapFixture {
    std::string testFilePath;
    int netcdfID;

    FileMapFixture() : testFilePath("test_temp_file.nc"), netcdfID(-1) {
        std::remove(testFilePath.c_str());  // Ensure clean slate
    }

    ~FileMapFixture() {
        // Best effort cleanup
        try {
            Obs2Ioda::FileMap::getInstance().removeFile(netcdfID);
        } catch (...) {}
        std::remove(testFilePath.c_str());
    }
};

} // namespace

//--------------------------------------------------------------------
// Create and close NetCDF file
//--------------------------------------------------------------------

CASE("FileMap - Create and close NetCDF file") {
    FileMapFixture f;

    // Create
    int ret = Obs2Ioda::netcdfCreate(f.testFilePath.c_str(),
                                     &f.netcdfID,
                                     netCDF::NcFile::replace);
    EXPECT(ret == 0);
    EXPECT(f.netcdfID > 0);

    // Retrieve file from map
    auto file = Obs2Ioda::FileMap::getInstance().getFile(f.netcdfID);
    EXPECT(file != nullptr);
    EXPECT(file->getId() == f.netcdfID);

    // Close
    ret = Obs2Ioda::netcdfClose(f.netcdfID);
    EXPECT(ret == 0);

    // Ensure it was removed
    EXPECT_THROWS_AS(
        Obs2Ioda::FileMap::getInstance().getFile(f.netcdfID),
        netCDF::exceptions::NcBadId
    );
}

//--------------------------------------------------------------------
// Duplicate add throws
//--------------------------------------------------------------------

CASE("FileMap - Duplicate add throws") {
    FileMapFixture f;

    int ret = Obs2Ioda::netcdfCreate(f.testFilePath.c_str(),
                                     &f.netcdfID,
                                     netCDF::NcFile::replace);
    EXPECT(ret == 0);
    auto file = Obs2Ioda::FileMap::getInstance().getFile(f.netcdfID);

    // Adding again should throw
    EXPECT_THROWS_AS(
        Obs2Ioda::FileMap::getInstance().addFile(f.netcdfID, file),
        netCDF::exceptions::NcCantCreate
    );

    Obs2Ioda::netcdfClose(f.netcdfID);
}

//--------------------------------------------------------------------
// Remove invalid ID throws
//--------------------------------------------------------------------

CASE("FileMap - Remove invalid ID throws") {
    EXPECT_THROWS_AS(
        Obs2Ioda::FileMap::getInstance().removeFile(9999),
        netCDF::exceptions::NcBadId
    );
}

//--------------------------------------------------------------------
// Get invalid ID throws
//--------------------------------------------------------------------

CASE("FileMap - Get invalid ID throws") {
    EXPECT_THROWS_AS(
        Obs2Ioda::FileMap::getInstance().getFile(9999),
        netCDF::exceptions::NcBadId
    );
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char* argv[]) {
    return run_tests(argc, argv);
}
