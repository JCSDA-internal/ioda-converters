#include <sstream>
#include <iostream>
#include <string>
#include <netcdf>
#include "netcdf_error.h"

#include "eckit/testing/Test.h"

using namespace eckit::testing;

namespace {

// A mock exception class since NcException is abstract
class MockNcException : public netCDF::exceptions::NcException {
public:
    MockNcException(const std::string &msg, int code)
        : netCDF::exceptions::NcException(code, "MockException", msg.c_str(), 0),
          msg_(msg), code_(code) {}

    int errorCode() const {
        return code_;
    }

    const char* what() const noexcept override {
        return msg_.c_str();
    }

private:
    std::string msg_;
    int code_;
};

// Helper for capturing std::cerr output
struct CerrCapture {
    std::streambuf* originalCerr;
    std::ostringstream captured;

    CerrCapture() {
        originalCerr = std::cerr.rdbuf(captured.rdbuf());
    }

    ~CerrCapture() {
        std::cerr.rdbuf(originalCerr);
    }

    std::string str() const { return captured.str(); }
};

} // namespace

//--------------------------------------------------------------------
// Outputs message and returns error code
//--------------------------------------------------------------------

CASE("NetcdfError - OutputsMessageAndReturnsErrorCode") {
    CerrCapture capture;
    MockNcException mock("Something went wrong", 42);

    int code = Obs2Ioda::netcdfErrorMessage(mock, 99, "mock_file.cpp");
    std::string output = capture.str();

    EXPECT(output.find("Code: 42") != std::string::npos);
    EXPECT(output.find("File: mock_file.cpp") != std::string::npos);
    EXPECT(output.find("Line: 99") != std::string::npos);
    EXPECT(output.find("Message: Something went wrong") != std::string::npos);
    EXPECT(code == 42);
}

//--------------------------------------------------------------------
// Returns -1 when code is zero
//--------------------------------------------------------------------

CASE("NetcdfError - ReturnsMinusOneWhenCodeIsZero") {
    CerrCapture capture;
    MockNcException mock("Benign issue", 0);

    int code = Obs2Ioda::netcdfErrorMessage(mock, 0, "");
    std::string output = capture.str();

    EXPECT(output.find("Code: 0") != std::string::npos);
    EXPECT(code == -1);
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char* argv[]) {
    return run_tests(argc, argv);
}
