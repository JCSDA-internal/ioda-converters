//
// Created by Ronald McLaren on 8/17/20.
//

#pragma once

#include <string>
#include <memory>

#include "MappedField.h"

class Field
{
protected:
    std::string name_;
    MappedField mappedField_;
};

class MetaField : public Field
{

};

class ObsField : public Field
{

};

class TimestampField : public MetaField
{
public:
    TimestampField(const eckit::Configuration& conf);

private:
    std::shared_ptr<Field> year_;
    std::shared_ptr<Field> month_;
    std::shared_ptr<Field> day_;
    std::shared_ptr<Field> hour_;
    std::shared_ptr<Field> minute_;
    std::shared_ptr<Field> second_;
};

class CoordinateField : public MetaField
{

};

class LongitudeField : public CoordinateField
{

};

class LatitudeField : public CoordinateField
{

};





