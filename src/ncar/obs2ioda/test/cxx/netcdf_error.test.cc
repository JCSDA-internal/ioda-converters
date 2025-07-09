#include <gtest/gtest.h>
#include <sstream>
#include <iostream>
#include <string>
#include <netcdf>
#include "netcdf_error.h"

namespace {

// A mock exception class since NcException is abstract
class MockNcException : public netCDF::exceptions::NcException {
public:
    MockNcException(const std::string &msg, int code)
        : netCDF::exceptions::NcException(code, "MockException", msg.c_str(), 0),
          msg_(msg), code_(code) {}

    int errorCode() const  {
        return code_;
    }

    const char* what() const noexcept override {
        return msg_.c_str();
    }

private:
    std::string msg_;
    int code_;
};

class NetcdfErrorTest : public ::testing::Test {
protected:
    std::streambuf *originalCerr;
    std::ostringstream capturedCerr;

    void SetUp() override {
        originalCerr = std::cerr.rdbuf(capturedCerr.rdbuf());
    }

    void TearDown() override {
        std::cerr.rdbuf(originalCerr);
    }
};

/**
 * @test OutputsMessageAndReturnsErrorCode
 * @brief Tests that `netcdfErrorMessage` prints a detailed error message and returns the correct error code.
 *
 * This test constructs a mock NetCDF exception with a non-zero error code (42) and passes it to
 * `netcdfErrorMessage` along with a filename and line number. It verifies that:
 * - The returned error code matches the one in the exception.
 * - The standard error output contains the expected diagnostic fields, including code, file, line, and message.
 */
TEST_F(NetcdfErrorTest, OutputsMessageAndReturnsErrorCode) {
    MockNcException mock("Something went wrong", 42);

    int code = Obs2Ioda::netcdfErrorMessage(mock, 99, "mock_file.cpp");

    std::string output = capturedCerr.str();
    EXPECT_NE(output.find("Code: 42"), std::string::npos);
    EXPECT_NE(output.find("File: mock_file.cpp"), std::string::npos);
    EXPECT_NE(output.find("Line: 99"), std::string::npos);
    EXPECT_NE(output.find("Message: Something went wrong"), std::string::npos);
    EXPECT_EQ(code, 42);
}

/**
 * @test ReturnsMinusOneWhenCodeIsZero
 * @brief Tests that `netcdfErrorMessage` returns -1 when the original exception code is zero.
 *
 * Some NetCDF operations may throw exceptions with code 0 (e.g., benign or informational cases).
 * This test verifies that `netcdfErrorMessage` interprets such cases as errors and returns -1,
 * while still logging the correct information to standard error.
 */
TEST_F(NetcdfErrorTest, ReturnsMinusOneWhenCodeIsZero) {
    MockNcException mock("Benign issue", 0);

    int code = Obs2Ioda::netcdfErrorMessage(mock, 0, "");

    std::string output = capturedCerr.str();
    EXPECT_NE(output.find("Code: 0"), std::string::npos);
    EXPECT_EQ(code, -1);
}

} // namespace

