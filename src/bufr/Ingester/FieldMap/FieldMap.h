//
// Created by Ronald McLaren on 8/17/20.
//

#pragma once

#include <vector>
#include <memory>

#include "IngesterData.h"

#include "Field.h"


#include <eckit/config/LocalConfiguration.h>

class FieldMap
{
public:
    FieldMap() = default;
    explicit FieldMap(const eckit::Configuration&);

private:
    std::shared_ptr<LongitudeField> longitude_;
    std::shared_ptr<LatitudeField> latitude_;
    std::shared_ptr<TimestampField> timestamp_;
    std::vector<std::shared_ptr<ObsField>> observations_;
};


