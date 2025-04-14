#ifndef IODASCHEMAFIXTURE_H
#define IODASCHEMAFIXTURE_H

#include <gtest/gtest.h>
#include "yaml-cpp/yaml.h"
#include "ioda_obs_schema.h"

class IodaObsSchemaFixture : public ::testing::Test {
protected:
    void SetUp() override;

    void TearDown() override;

    YAML::Node schema;
    std::shared_ptr<IodaObsSchema> iodaSchema;
};

#endif //IODASCHEMAFIXTURE_H
