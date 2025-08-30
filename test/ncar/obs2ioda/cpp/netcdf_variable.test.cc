#include <netcdf>
#include <cstdio>
#include <string>
#include <vector>

#include "netcdf_variable.h"
#include "netcdf_file.h"

#include "eckit/testing/Test.h"

using namespace eckit::testing;
using namespace Obs2Ioda;

namespace {

struct NetcdfVariableFixture {
    std::string filePath;
    int netcdfID;
    std::shared_ptr<netCDF::NcFile> file;

    NetcdfVariableFixture() : filePath("test_variable.nc"), netcdfID(-1) {
        std::remove(filePath.c_str());
        file = std::make_shared<netCDF::NcFile>(filePath, netCDF::NcFile::replace);
        netcdfID = file->getId();
        FileMap::getInstance().addFile(netcdfID, file);
    }

    ~NetcdfVariableFixture() {
        try {
            FileMap::getInstance().removeFile(netcdfID);
        } catch (...) {}
        std::remove(filePath.c_str());
    }
};

} // namespace

//--------------------------------------------------------------------
// Add int variable and put values
//--------------------------------------------------------------------

CASE("NetcdfVariable - AddVarAndPutIntValues") {
    NetcdfVariableFixture f;

    f.file->addDim("loc", 4);
    const char *dims[] = {"loc"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "var_int", NC_INT, 1, dims) == 0);

    int values[] = {1, 2, 3, 4};
    EXPECT(netcdfPutVarInt(f.netcdfID, "", "var_int", values) == 0);

    int result[4];
    f.file->getVar("var_int").getVar(result);
    EXPECT(result[0] == 1);
    EXPECT(result[3] == 4);
}

//--------------------------------------------------------------------
// Put double values
//--------------------------------------------------------------------

CASE("NetcdfVariable - PutDoubleValues") {
    NetcdfVariableFixture f;

    f.file->addDim("dim1", 2);
    const char *dims[] = {"dim1"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "var_double", NC_DOUBLE, 1, dims) == 0);

    double vals[] = {3.14, 2.71};
    EXPECT(netcdfPutVarDouble(f.netcdfID, "", "var_double", vals) == 0);

    double out[2];
    f.file->getVar("var_double").getVar(out);
    EXPECT(out[1] == 2.71);
}

//--------------------------------------------------------------------
// Put float values
//--------------------------------------------------------------------

CASE("NetcdfVariable - PutFloatValues") {
    NetcdfVariableFixture f;

    f.file->addDim("d", 3);
    const char *dims[] = {"d"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "var_float", NC_FLOAT, 1, dims) == 0);

    float vals[] = {1.1f, 2.2f, 3.3f};
    EXPECT(netcdfPutVarReal(f.netcdfID, "", "var_float", vals) == 0);

    float out[3];
    f.file->getVar("var_float").getVar(out);
    EXPECT(out[0] == 1.1f);
    EXPECT(out[1] == 2.2f);
    EXPECT(out[2] == 3.3f);
}

//--------------------------------------------------------------------
// Put char array values
//--------------------------------------------------------------------

CASE("NetcdfVariable - PutCharArrayValues") {
    NetcdfVariableFixture f;

    f.file->addDim("nstr", 3);
    f.file->addDim("len", 7);
    const char *dims[] = {"nstr", "len"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "char_arr", NC_CHAR, 2, dims) == 0);

    const char *values[] = {"apple", "banana", "pear"};
    EXPECT(netcdfPutVarChar(f.netcdfID, "", "char_arr", values) == 0);

    char buffer[3][7] = {};
    f.file->getVar("char_arr").getVar(&buffer[0][0]);
    EXPECT(std::string(buffer[0]) == "apple");
    EXPECT(std::string(buffer[1]) == "banana");
    EXPECT(std::string(buffer[2]) == "pear");
}

//--------------------------------------------------------------------
// Put int64 values
//--------------------------------------------------------------------

CASE("NetcdfVariable - PutInt64Values") {
    NetcdfVariableFixture f;

    f.file->addDim("d", 2);
    const char *dims[] = {"d"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "var_i64", NC_INT64, 1, dims) == 0);

    long long vals[] = {123456789LL, -987654321LL};
    EXPECT(netcdfPutVarInt64(f.netcdfID, "", "var_i64", vals) == 0);
}

//--------------------------------------------------------------------
// Set fill value int
//--------------------------------------------------------------------

CASE("NetcdfVariable - SetFillValueInt") {
    NetcdfVariableFixture f;

    f.file->addDim("n", 1);
    const char *dims[] = {"n"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "var_with_fill", NC_INT, 1, dims) == 0);
    EXPECT(netcdfSetFillInt(f.netcdfID, "", "var_with_fill", true, -999) == 0);
}

//--------------------------------------------------------------------
// Set fill value string
//--------------------------------------------------------------------

CASE("NetcdfVariable - SetFillValueString") {
    NetcdfVariableFixture f;

    f.file->addDim("n", 1);
    const char *dims[] = {"n"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "str_fill", NC_STRING, 1, dims) == 0);
    EXPECT(netcdfSetFillString(f.netcdfID, "", "str_fill", true, "") == 0);
}

//--------------------------------------------------------------------
// Flatten char array pads and null terminates
//--------------------------------------------------------------------

CASE("NetcdfVariable - FlattenCharArrayPadsAndNullTerminates") {
    const char *values[] = {"apple", "banana", "pear"};
    size_t numStrings = 3;
    size_t stringLen = 7;

    std::vector<char> result = flattenCharArray(values, numStrings, stringLen);

    EXPECT(result.size() == numStrings * stringLen);
    EXPECT(std::string(&result[0]) == "apple");
    EXPECT(std::string(&result[7]) == "banana");
    EXPECT(std::string(&result[14]) == "pear");
    EXPECT(result[5] == '\0');
    EXPECT(result[13] == '\0');
    EXPECT(result[21] == '\0');
}

//--------------------------------------------------------------------
// Put string values
//--------------------------------------------------------------------

CASE("NetcdfVariable - PutStringValues") {
    NetcdfVariableFixture f;

    f.file->addDim("nstr", 2);
    const char *dims[] = {"nstr"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "var_str", NC_STRING, 1, dims) == 0);

    const char *inputValues[] = {"hello", "world"};
    EXPECT(netcdfPutVarString(f.netcdfID, "", "var_str", inputValues) == 0);

    char *outputValues[2] = {nullptr, nullptr};
    f.file->getVar("var_str").getVar(outputValues);

    EXPECT(std::string(outputValues[0]) == "hello");
    EXPECT(std::string(outputValues[1]) == "world");

    for (auto &outputValue : outputValues) {
        if (outputValue) free(outputValue);
    }
}

//--------------------------------------------------------------------
// Put empty and special string values
//--------------------------------------------------------------------

CASE("NetcdfVariable - PutEmptyAndSpecialStringValues") {
    NetcdfVariableFixture f;

    f.file->addDim("nstr", 3);
    const char *dims[] = {"nstr"};
    EXPECT(netcdfAddVar(f.netcdfID, "", "special_str", NC_STRING, 1, dims) == 0);

    const char *inputValues[] = {"", "foo\nbar", "©2025!"};
    EXPECT(netcdfPutVarString(f.netcdfID, "", "special_str", inputValues) == 0);

    char *outputValues[3] = {nullptr, nullptr, nullptr};
    f.file->getVar("special_str").getVar(outputValues);

    EXPECT(std::string(outputValues[0]) == "");
    EXPECT(std::string(outputValues[1]) == "foo\nbar");
    EXPECT(std::string(outputValues[2]) == "©2025!");

    for (auto &outputValue : outputValues) {
        if (outputValue) free(outputValue);
    }
}

//--------------------------------------------------------------------
// AddVar with null group name returns error
//--------------------------------------------------------------------

CASE("NetcdfVariable - AddVarWithNullGroupNameReturnsError") {
    NetcdfVariableFixture f;

    const char *dims[] = {"dim"};
    int ret = netcdfAddVar(f.netcdfID, nullptr, "var_null_group", NC_INT, 1, dims);
    EXPECT(ret == -116);
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char* argv[]) {
    return run_tests(argc, argv);
}
