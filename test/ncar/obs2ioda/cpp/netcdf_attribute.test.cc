#include <netcdf>
#include <fstream>
#include <cstdio>
#include <cstring>

#include "netcdf_attribute.h"
#include "netcdf_file.h"
#include "netcdf_error.h"

#include "eckit/testing/Test.h"

using namespace eckit::testing;
using namespace Obs2Ioda;

namespace {

struct NetcdfPutAttFixture {
    std::string filePath;
    int netcdfID;
    std::shared_ptr<netCDF::NcFile> file;

    NetcdfPutAttFixture() : filePath("test_att.nc"), netcdfID(-1) {
        std::remove(filePath.c_str());
        file = std::make_shared<netCDF::NcFile>(filePath, netCDF::NcFile::replace);
        netcdfID = file->getId();
        FileMap::getInstance().addFile(netcdfID, file);
    }

    ~NetcdfPutAttFixture() {
        try {
            FileMap::getInstance().removeFile(netcdfID);
        } catch (...) {}
        std::remove(filePath.c_str());
    }
};

} // namespace

//--------------------------------------------------------------------
// Put int attribute to variable
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutIntAttributeToVariable") {
    NetcdfPutAttFixture f;

    auto dim = f.file->addDim("dim", 1);
    auto var = f.file->addVar("var", netCDF::ncInt, {dim});

    int value = 42;
    int ret = netcdfPutAttInt(f.netcdfID, "my_attr", &value, "var", "");
    EXPECT(ret == 0);

    auto attr = var.getAtt("my_attr");
    EXPECT(!attr.isNull());

    int readVal = -1;
    attr.getValues(&readVal);
    EXPECT(readVal == value);
}

//--------------------------------------------------------------------
// Put int array attribute to variable
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutIntArrayAttributeToVariable") {
    NetcdfPutAttFixture f;

    auto dim = f.file->addDim("dim", 4);
    auto var = f.file->addVar("arrvar", netCDF::ncInt, {dim});

    int values[] = {1, 2, 3, 4};
    int ret = netcdfPutAttIntArray(f.netcdfID, "arr_attr", values, 4, "arrvar", "");
    EXPECT(ret == 0);

    auto attr = var.getAtt("arr_attr");
    EXPECT(!attr.isNull());

    int readVals[4] = {};
    attr.getValues(readVals);
    for (int i = 0; i < 4; ++i) {
        EXPECT(readVals[i] == values[i]);
    }
}

//--------------------------------------------------------------------
// Put real array attribute to root group
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutRealArrayAttributeToGroup") {
    NetcdfPutAttFixture f;

    float values[] = {3.14f, 2.71f};
    int ret = netcdfPutAttRealArray(f.netcdfID, "real_attr", values, 2, "", "");
    EXPECT(ret == 0);

    auto attr = f.file->getAtt("real_attr");
    EXPECT(!attr.isNull());

    float readVals[2] = {};
    attr.getValues(readVals);
    EXPECT(readVals[0] == values[0]);
    EXPECT(readVals[1] == values[1]);
}

//--------------------------------------------------------------------
// Put string attribute to root group
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutStringAttributeToGroup") {
    NetcdfPutAttFixture f;

    const char *msg = "hello world";
    int ret = netcdfPutAttString(f.netcdfID, "greeting", msg, "", "");
    EXPECT(ret == 0);

    auto attr = f.file->getAtt("greeting");
    EXPECT(!attr.isNull());

    std::string value;
    attr.getValues(value);
    EXPECT(value == msg);
}

//--------------------------------------------------------------------
// Put string attribute to variable
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutStringAttributeToVariable") {
    NetcdfPutAttFixture f;

    auto dim = f.file->addDim("dim", 1);
    auto var = f.file->addVar("name", netCDF::ncFloat, {dim});

    const char *label = "temperature";
    int ret = netcdfPutAttString(f.netcdfID, "label", label, "name", "");
    EXPECT(ret == 0);

    auto attr = var.getAtt("label");
    EXPECT(!attr.isNull());

    std::string value;
    attr.getValues(value);
    EXPECT(value == label);
}

//--------------------------------------------------------------------
// Put attribute with null group name returns error
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutAttWithNullGroupNameReturnsError") {
    NetcdfPutAttFixture f;

    int value = 100;
    int status = netcdfPutAttInt(f.netcdfID, "null_group_attr", &value, "var", nullptr);
    EXPECT(status == -116);  // Expect error for null group name
}

//--------------------------------------------------------------------
// Put attribute with null variable name returns error
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutAttWithNullVarNameReturnsError") {
    NetcdfPutAttFixture f;

    int value = 100;
    int status = netcdfPutAttInt(f.netcdfID, "null_group_attr", &value, nullptr, "");
    EXPECT(status == -59);  // Expect error for null var name
}

//--------------------------------------------------------------------
// Put attribute with null variable and null group returns error
//--------------------------------------------------------------------

CASE("NetcdfPutAtt - PutAttWithNullVarNameAndNullGroupNameReturnsError") {
    NetcdfPutAttFixture f;

    int value = 100;
    int status = netcdfPutAttInt(f.netcdfID, "null_group_attr", &value, nullptr, "");
    EXPECT(status != 0);  // Must be an error
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char* argv[]) {
    return run_tests(argc, argv);
}
