//
// Created by Ronald McLaren on 10/5/22.
//

#include "NcepDataProvider.h"
#include "bufr_interface.h"

#include <algorithm>
#include <iostream>


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

    size_t NcepDataProvider::variantId() const
    {
        return 0;
    }

    bool NcepDataProvider::hasVariants() const
    {
        return false;
    }

    void NcepDataProvider::_deleteData()
    {
        isc_ = gsl::span<const int>(nullptr, 0);
        link_ = gsl::span<const int>(nullptr, 0);
        itp_ = gsl::span<const int>(nullptr, 0);
        jmpb_ = gsl::span<const int>(nullptr, 0);
    }
}  // namespoce bufr
}  // namespace Ingester