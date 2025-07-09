#include <gtest/gtest.h>
#include <netcdf>
#include <cstdio>
#include "netcdf_file.h"

namespace {

    class FileMapTest : public ::testing::Test {
    protected:
        std::string testFilePath;
        int netcdfID;

        void SetUp() override {
            testFilePath = "test_temp_file.nc";
            std::remove(testFilePath.c_str()); // Ensure clean slate
            netcdfID = -1;
        }

        void TearDown() override {
            // Best effort cleanup
            try {
                Obs2Ioda::FileMap::getInstance().removeFile(netcdfID);
            } catch (...) {}
            std::remove(testFilePath.c_str());
        }
    };

    /**
 * @test CreateAndCloseNetCDFFile
 * @brief Tests creation, retrieval, and cleanup of a NetCDF file using the FileMap utility.
 *
 * This test:
 * - Creates a new NetCDF file using `netcdfCreate`, which registers the file in the FileMap singleton.
 * - Retrieves the file using its NetCDF ID and verifies the ID matches.
 * - Closes the file using `netcdfClose`, which also removes it from FileMap.
 * - Confirms that accessing the file after closing throws an `NcBadId` exception.
 */
    TEST_F(FileMapTest, CreateAndCloseNetCDFFile) {
        // Create
        int ret = Obs2Ioda::netcdfCreate(testFilePath.c_str(),
                                         &netcdfID,
                                         netCDF::NcFile::replace);
        EXPECT_EQ(ret, 0);
        EXPECT_GT(netcdfID, 0);

        // Retrieve file from map
        auto file = Obs2Ioda::FileMap::getInstance().getFile(netcdfID);
        EXPECT_TRUE(file != nullptr);
        EXPECT_EQ(file->getId(), netcdfID);

        // Close
        ret = Obs2Ioda::netcdfClose(netcdfID);
        EXPECT_EQ(ret, 0);

        // Ensure it was removed
        EXPECT_THROW(Obs2Ioda::FileMap::getInstance().getFile(netcdfID),
                     netCDF::exceptions::NcBadId);
    }

    /**
 * @test DuplicateAddThrows
 * @brief Ensures that trying to add the same NetCDF file ID to the FileMap twice results in an exception.
 *
 * After creating and registering a file with `netcdfCreate`, this test attempts to register it again
 * manually using `FileMap::addFile`. This operation is invalid and should throw a `NcCantCreate` exception.
 * The test concludes by closing the file to clean up the test environment.
 */
    TEST_F(FileMapTest, DuplicateAddThrows) {
        int ret = Obs2Ioda::netcdfCreate(testFilePath.c_str(),
                                         &netcdfID,
                                         netCDF::NcFile::replace);
        EXPECT_EQ(ret, 0);
        auto file = Obs2Ioda::FileMap::getInstance().getFile(netcdfID);

// Adding again should throw
        EXPECT_THROW(
                Obs2Ioda::FileMap::getInstance().addFile(netcdfID,
                                                         file),
                netCDF::exceptions::NcCantCreate
        );

        Obs2Ioda::netcdfClose(netcdfID);
    }

    TEST_F(FileMapTest, RemoveInvalidIDThrows) {
        EXPECT_THROW(
                Obs2Ioda::FileMap::getInstance().removeFile(9999),
                netCDF::exceptions::NcBadId
        );
    }

    TEST_F(FileMapTest, GetInvalidIDThrows) {
        EXPECT_THROW(
                Obs2Ioda::FileMap::getInstance().getFile(9999),
                netCDF::exceptions::NcBadId
        );
    }

}  // namespace
