#include <gtest/gtest.h>
#include <netcdf>
#include <fstream>
#include <cstdio>
#include <cstring>

#include "netcdf_attribute.h"
#include "netcdf_file.h"
#include "netcdf_error.h"

namespace {

    using namespace Obs2Ioda;

    class NetcdfPutAttTest : public ::testing::Test {
    protected:
        std::string filePath;
        int netcdfID;
        std::shared_ptr<netCDF::NcFile> file;

        void SetUp() override {
            filePath = "test_att.nc";
            std::remove(filePath.c_str());
            file = std::make_shared<netCDF::NcFile>(filePath,
                                                    netCDF::NcFile::replace);
            netcdfID = file->getId();
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
 * @test PutIntAttributeToVariable
 * @brief Tests writing a scalar integer attribute to a NetCDF variable.
 *
 * This test creates a NetCDF file with one dimension and one integer variable.
 * It then writes an integer attribute ("my_attr") to the variable using `netcdfPutAttInt`.
 * The test verifies that the attribute exists and that its value is correctly stored.
 */
    TEST_F(NetcdfPutAttTest, PutIntAttributeToVariable) {
        auto dim = file->addDim("dim", 1);
        auto var = file->addVar("var", netCDF::ncInt, {dim});

        int value = 42;
        int ret = netcdfPutAttInt(netcdfID, "my_attr", &value, "var",
                                  nullptr);
        EXPECT_EQ(ret, 0);

        auto attr = var.getAtt("my_attr");
        EXPECT_FALSE(attr.isNull());

        int readVal = -1;
        attr.getValues(&readVal);
        EXPECT_EQ(readVal, value);
    }
/**
 * @test PutIntArrayAttributeToVariable
 * @brief Tests writing an array of integers as an attribute to a NetCDF variable.
 *
 * This test creates a variable and writes a 4-element integer array as an attribute.
 * It uses `netcdfPutAttIntArray` and verifies that the resulting attribute contains
 * the expected values.
 */
    TEST_F(NetcdfPutAttTest, PutIntArrayAttributeToVariable) {
        auto dim = file->addDim("dim", 4);
        auto var = file->addVar("arrvar", netCDF::ncInt, {dim});

        int values[] = {1, 2, 3, 4};
        int ret = netcdfPutAttIntArray(netcdfID, "arr_attr", values, 4,
                                       "arrvar", nullptr);
        EXPECT_EQ(ret, 0);

        auto attr = var.getAtt("arr_attr");
        ASSERT_FALSE(attr.isNull());

        int readVals[4] = {};
        attr.getValues(readVals);
        for (int i = 0; i < 4; ++i) {
            EXPECT_EQ(readVals[i], values[i]);
        }
    }
/**
 * @test PutRealArrayAttributeToGroup
 * @brief Tests writing a float array as a global attribute to the NetCDF file (group).
 *
 * The test calls `netcdfPutAttRealArray` with `nullptr` for both variable and group names,
 * meaning the attribute is written to the root group. It verifies the content using `getValues`.
 */
    TEST_F(NetcdfPutAttTest, PutRealArrayAttributeToGroup) {
        float values[] = {3.14f, 2.71f};
        int ret = netcdfPutAttRealArray(netcdfID, "real_attr", values,
                                        2, nullptr, nullptr);
        EXPECT_EQ(ret, 0);

        auto attr = file->getAtt("real_attr");
        ASSERT_FALSE(attr.isNull());

        float readVals[2] = {};
        attr.getValues(readVals);
        EXPECT_FLOAT_EQ(readVals[0], values[0]);
        EXPECT_FLOAT_EQ(readVals[1], values[1]);
    }

/**
 * @test PutStringAttributeToGroup
 * @brief Tests writing a single string as a global (group-level) attribute.
 *
 * The attribute "greeting" is written using `netcdfPutAttString` to the root group.
 * The test reads back the value using `NcVar::getAtt` and checks that it matches.
 */
    TEST_F(NetcdfPutAttTest, PutStringAttributeToGroup) {
        const char *msg = "hello world";
        int ret = netcdfPutAttString(netcdfID, "greeting", msg, nullptr,
                                     nullptr);
        EXPECT_EQ(ret, 0);

        auto attr = file->getAtt("greeting");
        ASSERT_FALSE(attr.isNull());

        std::string value;
        attr.getValues(value);
        EXPECT_EQ(value, msg);
    }

/**
 * @test PutStringAttributeToVariable
 * @brief Tests writing a string attribute to a specific NetCDF variable.
 *
 * A float variable is created and a label ("temperature") is added to it as a string
 * attribute. The test validates that the attribute was written and contains the correct value.
 */
    TEST_F(NetcdfPutAttTest, PutStringAttributeToVariable) {
        auto dim = file->addDim("dim", 1);
        auto var = file->addVar("name", netCDF::ncFloat, {dim});

        const char *label = "temperature";
        int ret = netcdfPutAttString(netcdfID, "label", label, "name",
                                     nullptr);
        EXPECT_EQ(ret, 0);

        auto attr = var.getAtt("label");
        ASSERT_FALSE(attr.isNull());

        std::string value;
        attr.getValues(value);
        EXPECT_EQ(value, label);
    }

}  // namespace
