#include <gtest/gtest.h>
#include <yaml-cpp/yaml.h>
#include "ioda_obs_schema.h"


class IodaObsSchemaFixture : public ::testing::Test {
protected:
    void SetUp() override {
        this->schema = YAML::LoadFile(Obs2Ioda::IODA_SCHEMA_YAML);
        this->iodaSchema = std::make_shared<IodaObsSchema>(this->schema);
    }
    YAML::Node schema;
    std::shared_ptr<IodaObsSchema> iodaSchema;
};

/**
 * @brief Tests variable alias resolution and canonical name mapping.
 *
 * This test ensures:
 * - Deprecated variable names resolve to the same IodaObsVariable instance.
 * - Canonical and deprecated names are stored correctly.
 *
 * The test also verifies support for global dimension variables:
 * - "Location" is a dimension that also appears as a global variable in IODA files.
 * - "nlocs" is a deprecated name for "Location" and should resolve to the same component.
 *
 * All entries from the `names` list in the YAML schema are inserted into the `componentMap`
 * as keys that point to the same shared component. Because they are stored as
 * `std::shared_ptr`, comparisons between any aliases should yield equality (`==`).
 */
TEST_F(IodaObsSchemaFixture, Variable) {
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
    // <-- Dimension used as variable

    EXPECT_EQ(station_id_Variable, stationIdentification_Variable);
    EXPECT_EQ(
        stationIdentification_Variable->getValidName(),
        "stationIdentification"
    );
    EXPECT_EQ(stationIdentification_Variable->getNames().size(), 4);
    EXPECT_EQ(
        sensorCentralFrequency_Variable->getValidName(),
        "sensorCentralFrequency"
    );
    EXPECT_EQ(sensorCentralFrequency_Variable->getNames().size(), 4);

    // Test for global dimension-as-variable logic
    EXPECT_EQ(nlocs_Variable, Location_Variable);
    EXPECT_EQ(Location_Variable->getValidName(), "Location");
    EXPECT_EQ(Location_Variable->getNames().size(), 2);
}

/**
 * @brief Tests dimension alias resolution and canonical name mapping.
 *
 * This test verifies:
 * - Deprecated dimension names resolve to the same IodaObsDimension instance.
 * - Canonical and deprecated names are correctly tracked.
 *
 * All names listed in the `names` vector for a component are stored in the `componentMap`
 * as keys pointing to the same `std::shared_ptr`. Therefore, any two names for the same
 * component should compare equal using `EXPECT_EQ(...)`.
 */
TEST_F(IodaObsSchemaFixture, Dimension) {
    const auto nlocs_Dimension = iodaSchema->getDimension("nlocs");
    const auto Location_Dimension = iodaSchema->
            getDimension("Location");
    const auto nstring_Dimension = iodaSchema->getDimension("nstring");

    EXPECT_EQ(nlocs_Dimension, Location_Dimension);
    EXPECT_EQ(nlocs_Dimension->getValidName(), "Location");
    EXPECT_EQ(nlocs_Dimension->getNames().size(), 2);
    EXPECT_EQ(nstring_Dimension->getValidName(), "nstring");
    EXPECT_EQ(nstring_Dimension->getNames().size(), 1);
}

/**
 * @brief Tests attribute alias resolution and canonical name mapping.
 *
 * This test ensures:
 * - Multiple names referring to the same attribute resolve to the same instance.
 * - Canonical and deprecated names are correctly reported.
 *
 * The underlying `std::shared_ptr` stored in the component map guarantees that
 * all aliases point to the same object, so pointer comparisons should succeed.
 */
TEST_F(IodaObsSchemaFixture, Attribute) {
    const auto _ioda_layout_Attribute = iodaSchema->getAttribute(
        "_ioda_layout"
    );
    const auto ioda_object_type_Attribute = iodaSchema->getAttribute(
        "ioda_object_type"
    );

    EXPECT_EQ(_ioda_layout_Attribute, ioda_object_type_Attribute);
    EXPECT_EQ(
        ioda_object_type_Attribute->getValidName(), "ioda_object_type"
    );
    EXPECT_EQ(ioda_object_type_Attribute->getNames().size(), 2);
}

/**
 * @brief Tests group resolution and canonical name recognition.
 *
 * This test ensures that the "MetaData" group is parsed and stored correctly.
 * Since there are no deprecated names for this group, its name vector should
 * contain only one entry.
 */
TEST_F(IodaObsSchemaFixture, Group) {
    const auto MetaData_Group = iodaSchema->getGroup("MetaData");
    EXPECT_EQ(MetaData_Group->getValidName(), "MetaData");
    EXPECT_EQ(MetaData_Group->getNames().size(), 1);
}

/**
 * @brief Entry point for all tests in this suite.
 */
int main(int argc, char **argv) {
    ::testing::InitGoogleTest(&argc, argv);
    return RUN_ALL_TESTS();
}
