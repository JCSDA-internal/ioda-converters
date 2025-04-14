#include "IodaObsSchemaFixture.h"


void IodaObsSchemaFixture::SetUp() {
    this->schema = YAML::LoadFile(Obs2Ioda::IODA_SCHEMA_YAML);
    this->iodaSchema = std::make_shared<IodaObsSchema>(this->schema);
}

void IodaObsSchemaFixture::TearDown() {
}
