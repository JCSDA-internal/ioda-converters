#include "ioda_obs_schema.h"
#include "eckit/testing/Test.h"

using namespace eckit::testing;

namespace {
    std::shared_ptr<IodaObsSchema> makeSchema() {
        auto yamlNode = std::make_shared<YamlEckitNode>(
            Obs2Ioda::IODA_SCHEMA_YAML
        );
        return std::make_shared<IodaObsSchema>(yamlNode);
    }
} // namespace

//--------------------------------------------------------------------
// Variable tests
//--------------------------------------------------------------------

CASE("IodaObsSchema - Variable aliases and canonical names") {
    auto iodaSchema = makeSchema();

    const auto station_id_Variable = iodaSchema->getVariable(
        "station_id"
    );
    const auto stationIdentification_Variable = iodaSchema->getVariable(
        "stationIdentification"
    );
    const auto sensorCentralFrequency_Variable = iodaSchema->
            getVariable("sensorCentralFrequency");
    const auto nlocs_Variable = iodaSchema->getVariable("nlocs");
    const auto Location_Variable = iodaSchema->getVariable("Location");

    EXPECT(station_id_Variable == stationIdentification_Variable);
    EXPECT(
        stationIdentification_Variable->getValidName() ==
        "stationIdentification"
    );
    EXPECT(stationIdentification_Variable->getNames().size() == 4);
    EXPECT(
        sensorCentralFrequency_Variable->getValidName() ==
        "sensorCentralFrequency"
    );
    EXPECT(sensorCentralFrequency_Variable->getNames().size() == 4);

    // Global dimension-as-variable logic
    EXPECT(nlocs_Variable == Location_Variable);
    EXPECT(Location_Variable->getValidName() == "Location");
    EXPECT(Location_Variable->getNames().size() == 2);
}

//--------------------------------------------------------------------
// Dimension tests
//--------------------------------------------------------------------

CASE("IodaObsSchema - Dimension aliases and canonical names") {
    auto iodaSchema = makeSchema();

    const auto nlocs_Dimension = iodaSchema->getDimension("nlocs");
    const auto Location_Dimension = iodaSchema->
            getDimension("Location");
    const auto nstring_Dimension = iodaSchema->getDimension("nstring");

    EXPECT(nlocs_Dimension == Location_Dimension);
    EXPECT(nlocs_Dimension->getValidName() == "Location");
    EXPECT(nlocs_Dimension->getNames().size() == 2);
    EXPECT(nstring_Dimension->getValidName() == "nstring");
    EXPECT(nstring_Dimension->getNames().size() == 1);
}

//--------------------------------------------------------------------
// Attribute tests
//--------------------------------------------------------------------

CASE("IodaObsSchema - Attribute aliases and canonical names") {
    auto iodaSchema = makeSchema();

    const auto _ioda_layout_Attribute = iodaSchema->getAttribute(
        "_ioda_layout"
    );
    const auto ioda_object_type_Attribute = iodaSchema->getAttribute(
        "ioda_object_type"
    );

    EXPECT(_ioda_layout_Attribute == ioda_object_type_Attribute);
    EXPECT(
        ioda_object_type_Attribute->getValidName() == "ioda_object_type"
    );
    EXPECT(ioda_object_type_Attribute->getNames().size() == 2);
}

//--------------------------------------------------------------------
// Group tests
//--------------------------------------------------------------------

CASE("IodaObsSchema - Group resolution and canonical names") {
    auto iodaSchema = makeSchema();

    const auto MetaData_Group = iodaSchema->getGroup("MetaData");
    EXPECT(MetaData_Group->getValidName() == "MetaData");
    EXPECT(MetaData_Group->getNames().size() == 1);
}

//--------------------------------------------------------------------
// Entry point
//--------------------------------------------------------------------

int main(int argc, char *argv[]) {
    return run_tests(argc, argv);
}
