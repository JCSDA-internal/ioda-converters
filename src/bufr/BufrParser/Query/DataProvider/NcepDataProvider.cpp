//
// Created by Ronald McLaren on 10/5/22.
//

#include "NcepDataProvider.h"
#include "bufr_interface.h"

#include <algorithm>


namespace Ingester {
namespace bufr {
    NcepDataProvider::NcepDataProvider(const std::string& filePath) :
      DataProvider(filePath)
    {
    }

    void NcepDataProvider::open()
    {
        open_f(FileUnit, filePath_.c_str());
        openbf_f(FileUnit, "IN", FileUnit);
    }

    void NcepDataProvider::run(const QuerySet& querySet,
                               const std::function<void(const DataProviderType&)> processMsg,
                               const std::function<void(const DataProviderType&)> processSubset,
                               const std::function<void(const DataProviderType&)> processFinish,
                               const std::function<bool(const DataProviderType&)> continueProcessing)
    {
        static int SubsetLen = 9;
        unsigned int messageNum = 0;
        char subsetChars[SubsetLen];
        int iddate;

        int bufrLoc;
        int il, im;  // throw away

        while (ireadmg_f(FileUnit, subsetChars, &iddate, SubsetLen) == 0)
        {
            auto subset = std::string(subsetChars);
            subset.erase(std::remove_if(subset.begin(), subset.end(), isspace), subset.end());

            if (querySet.includesSubset(subset))
            {
                while (ireadsb_f(FileUnit) == 0)
                {
                    status_f(FileUnit, &bufrLoc, &il, &im);
                    updateData(subset, bufrLoc);

                    processSubset(shared_from_this());
                }

                processMsg(shared_from_this());
                if (!continueProcessing(shared_from_this())) break;
            }
        }

        processFinish(shared_from_this());
        deleteData();
    }

    void NcepDataProvider::updateTableData(const std::string& subset)
    {
        int size = 0;
        int *intPtr = nullptr;
        double *dataPtr = nullptr;
        int strLen = 0;
        char *charPtr = nullptr;

        if (isc_.empty())
        {
            get_isc_f(&intPtr, &size);
            isc_ = gsl::span<const int>(intPtr, size);

            get_link_f(&intPtr, &size);
            link_ = gsl::span<const int>(intPtr, size);

            get_itp_f(&intPtr, &size);
            itp_ = gsl::span<const int>(intPtr, size);

            get_typ_f(&charPtr, &strLen, &size);
            typ_.resize(size);
            for (int wordIdx = 0; wordIdx < size; wordIdx++)
            {
                auto typ = std::string(&charPtr[wordIdx * strLen], strLen);
                typ_[wordIdx] = TypMap.at(typ);
            }

            get_tag_f(&charPtr, &strLen, &size);
            tag_.resize(size);
            for (int wordIdx = 0; wordIdx < size; wordIdx++)
            {
                auto tag = std::string(&charPtr[wordIdx * strLen], strLen);
                tag_[wordIdx] = tag.substr(0, tag.find_first_of(' '));
            }

            get_jmpb_f(&intPtr, &size);
            jmpb_ = gsl::span<const int>(intPtr, size);
        }
    }
}  // namespoce bufr
}  // namespace Ingester