
#include "DataObject.h"

#include "eckit/exception/Exceptions.h"


namespace Ingester 
{
    static std::shared_ptr<DataObjectBase> 
        DataObjectBase::fromResult(const std::shared_ptr<bufr::ResultBase>& resultBase)
    {
        std::shared_ptr<DataObjectBase> dataObject = nullptr;

        if (auto result = std::dynamic_pointer_cast<bufr::Result<FloatType>>(resultBase))
        {
            dataObject = std::make_shared<DataObject<FloatType>> (result->data, result->dims);
        }
        else if (auto result = std::dynamic_pointer_cast<bufr::Result<std::string>>(resultBase))
        {
            dataObject = std::make_shared<DataObject<std::string>> (result->data, result->dims);
        }
        else
        {
            throw eckit::BadParameter("Encountered unsupported Result Type.");
        }

        return dataObject;
    }


    DataObject::DataObject(const std::vector<T>& data, const Dimensions& dimensions) :
        DataObjectBase(dimensions), 
        data_(data) 
    {
    }

    ioda::VariableCreationParameters DataObject::makeCreationParams(
                                            const std::vector<ioda::Dimensions_t>& chunks,
                                            int compressionLevel) const
    {
        ioda::VariableCreationParameters params;
        params.chunk = true;
        params.chunks = chunks;
        params.compressWithGZIP(compressionLevel);
        // params.setFillValue<FloatType>(10e10);

        return params;
    }
}  // namespace Ingester
