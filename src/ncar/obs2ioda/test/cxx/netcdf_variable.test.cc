#include <gtest/gtest.h>
#include <netcdf>
#include <cstdio>
#include <string>
#include "netcdf_variable.h"
#include "netcdf_file.h"

namespace {

    using namespace Obs2Ioda;

    class NetcdfVariableTest : public ::testing::Test {
    protected:
        std::string filePath;
        int netcdfID;
        std::shared_ptr<netCDF::NcFile> file;

        void SetUp() override {
            filePath = "test_variable.nc";
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
 * @test AddVarAndPutIntValues
 * @brief Tests adding an integer variable and writing scalar values to it.
 */
    TEST_F(NetcdfVariableTest, AddVarAndPutIntValues) {
        auto dim = file->addDim("loc", 4);
        const char *dims[] = {"loc"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "var_int", NC_INT, 1, dims), 0);
        int values[] = {1, 2, 3, 4};
        ASSERT_EQ(netcdfPutVarInt(netcdfID, nullptr, "var_int", values), 0);

        int result[4];
        auto var = file->getVar("var_int");
        var.getVar(result);
        EXPECT_EQ(result[0], 1);
        EXPECT_EQ(result[3], 4);
    }

/**
 * @test PutDoubleValues
 * @brief Tests writing double-precision floating-point values to a NetCDF variable.
 */
    TEST_F(NetcdfVariableTest, PutDoubleValues) {
        auto dim = file->addDim("dim1", 2);
        const char *dims[] = {"dim1"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "var_double", NC_DOUBLE, 1, dims), 0);

        double vals[] = {3.14, 2.71};
        ASSERT_EQ(netcdfPutVarDouble(netcdfID, nullptr, "var_double", vals), 0);

        double out[2];
        file->getVar("var_double").getVar(out);
        EXPECT_DOUBLE_EQ(out[1], 2.71);
    }

/**
 * @test PutFloatValues
 * @brief Tests writing single-precision float values to a NetCDF variable.
 */
    TEST_F(NetcdfVariableTest, PutFloatValues) {
        auto dim = file->addDim("d", 3);
        const char *dims[] = {"d"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "var_float", NC_FLOAT, 1, dims), 0);
        float vals[] = {1.1f, 2.2f, 3.3f};
        ASSERT_EQ(netcdfPutVarReal(netcdfID, nullptr, "var_float", vals), 0);
        float out[3];
        file->getVar("var_float").getVar(out);
        EXPECT_FLOAT_EQ(out[0], 1.1f);
        EXPECT_FLOAT_EQ(out[1], 2.2f);
        EXPECT_FLOAT_EQ(out[2], 3.3f);
    }

/**
 * @test PutCharArrayValues
 * @brief Tests writing a 2D character array (array of fixed-length strings) to a NetCDF variable.
 */
    TEST_F(NetcdfVariableTest, PutCharArrayValues) {
        auto dim1 = file->addDim("nstr", 3);
        auto dim2 = file->addDim("len", 7);  // string length
        const char *dims[] = {"nstr", "len"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "char_arr", NC_CHAR, 2, dims), 0);

        const char *values[] = {"apple", "banana", "pear"};
        ASSERT_EQ(netcdfPutVarChar(netcdfID, nullptr, "char_arr", values), 0);

        char buffer[3][7] = {};
        file->getVar("char_arr").getVar(&buffer[0][0]);
        EXPECT_STREQ(buffer[0], "apple");
        EXPECT_STREQ(buffer[1], "banana");
        EXPECT_STREQ(buffer[2], "pear");
    }

/**
 * @test PutInt64Values
 * @brief Tests writing 64-bit integer values to a NetCDF variable.
 */
    TEST_F(NetcdfVariableTest, PutInt64Values) {
        file->addDim("d", 2);
        const char *dims[] = {"d"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "var_i64", NC_INT64, 1, dims), 0);

        long long vals[] = {123456789LL, -987654321LL};
        ASSERT_EQ(netcdfPutVarInt64(netcdfID, nullptr, "var_i64", vals), 0);
    }

/**
 * @test SetFillValueInt
 * @brief Tests setting an integer fill value for a variable.
 */
    TEST_F(NetcdfVariableTest, SetFillValueInt) {
        file->addDim("n", 1);
        const char *dims[] = {"n"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "var_with_fill", NC_INT, 1, dims), 0);
        ASSERT_EQ(netcdfSetFillInt(netcdfID, nullptr, "var_with_fill", true, -999), 0);
    }

/**
 * @test SetFillValueString
 * @brief Tests setting a string fill value for a NetCDF string variable.
 */
    TEST_F(NetcdfVariableTest, SetFillValueString) {
        file->addDim("n", 1);
        const char *dims[] = {"n"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "str_fill", NC_STRING, 1, dims), 0);
        ASSERT_EQ(netcdfSetFillString(netcdfID, nullptr, "str_fill", true, ""), 0);
    }

/**
 * @test FlattenCharArrayPadsAndNullTerminates
 * @brief Tests the flattening of a C-style string array into a flat char buffer with null termination.
 */
    TEST_F(NetcdfVariableTest, FlattenCharArrayPadsAndNullTerminates) {
        const char* values[] = {"apple", "banana", "pear"};
        size_t numStrings = 3;
        size_t stringLen = 7;

        std::vector<char> result = flattenCharArray(values, numStrings, stringLen);

        ASSERT_EQ(result.size(), numStrings * stringLen);
        EXPECT_STREQ(&result[0], "apple");
        EXPECT_STREQ(&result[7], "banana");
        EXPECT_STREQ(&result[14], "pear");
        EXPECT_EQ(result[5], '\0');   // null terminator for "apple"
        EXPECT_EQ(result[13], '\0');  // null terminator for "banana"
        EXPECT_EQ(result[21], '\0');  // null terminator for "pear"
    }

/**
 * @test PutStringValues
 * @brief Tests writing an array of NetCDF string values (`NC_STRING`) to a variable.
 */
    TEST_F(NetcdfVariableTest, PutStringValues) {
        auto dim = file->addDim("nstr", 2);
        const char* dims[] = {"nstr"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "var_str", NC_STRING, 1, dims), 0);

        const char* inputValues[] = {"hello", "world"};
        ASSERT_EQ(netcdfPutVarString(netcdfID, nullptr, "var_str", inputValues), 0);

        char* outputValues[2] = {nullptr, nullptr};
        file->getVar("var_str").getVar(outputValues);

        EXPECT_STREQ(outputValues[0], "hello");
        EXPECT_STREQ(outputValues[1], "world");

        for (auto & outputValue : outputValues) {
            if (outputValue) {
                free(outputValue);  // NetCDF allocates strings using malloc
            }
        }
    }

/**
 * @test PutEmptyAndSpecialStringValues
 * @brief Tests writing empty strings and special characters (e.g., newlines, Unicode) to a NetCDF string variable.
 */
    TEST_F(NetcdfVariableTest, PutEmptyAndSpecialStringValues) {
        auto dim = file->addDim("nstr", 3);
        const char* dims[] = {"nstr"};
        ASSERT_EQ(netcdfAddVar(netcdfID, nullptr, "special_str", NC_STRING, 1, dims), 0);

        const char* inputValues[] = {"", "foo\nbar", "©2025!"};
        ASSERT_EQ(netcdfPutVarString(netcdfID, nullptr, "special_str", inputValues), 0);

        char* outputValues[3] = {nullptr, nullptr, nullptr};
        file->getVar("special_str").getVar(outputValues);

        EXPECT_STREQ(outputValues[0], "");
        EXPECT_STREQ(outputValues[1], "foo\nbar");
        EXPECT_STREQ(outputValues[2], "©2025!");

        for (auto & outputValue : outputValues) {
            if (outputValue) free(outputValue);
        }
    }
}
