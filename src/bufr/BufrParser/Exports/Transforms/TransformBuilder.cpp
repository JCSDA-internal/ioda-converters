//
// Created by Ronald McLaren on 9/25/20.
//

#include "TransformBuilder.h"

#include "eckit/exception/Exceptions.h"

#include "ScalingTransform.h"
#include "OffsetTransform.h"


static const char* TRANSFORMS_SECTION = "transforms";
static const char* OFFSET_KEY = "offset";
static const char* SCALE_KEY = "scale";

namespace Ingester
{
    std::shared_ptr<Transform> TransformBuilder::makeTransform(const eckit::Configuration& conf)
    {
        std::shared_ptr <Transform> transform;
        if (conf.has(OFFSET_KEY))
        {
            transform = std::make_shared<OffsetTransform>(conf.getFloat(OFFSET_KEY));
        }
        else if (conf.has(SCALE_KEY))
        {
            transform = std::make_shared<ScalingTransform>(conf.getFloat(SCALE_KEY));
        }
        else
        {
            throw eckit::BadParameter("Tried to create unknown export transform type. "
                                      "Check your configuration.");
        }

        return transform;
    }

    Transforms TransformBuilder::makeTransforms(const eckit::Configuration& conf)
    {
        Transforms transforms;
        if (conf.has(TRANSFORMS_SECTION))
        {
            for (const auto& transformConf : conf.getSubConfigurations(TRANSFORMS_SECTION))
            {
                transforms.push_back(makeTransform(transformConf));
            }
        }

        return transforms;
    }
}
