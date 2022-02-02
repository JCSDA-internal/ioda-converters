//
// Created by rmclaren on 1/30/22.
//

#include "BufrDataManager.h"
#include "bufr_interface.h"


namespace Ingester
{
    namespace bufr
    {
        void BufrDataManager::loadTableInfo()
        {
            int size = 0;
            int strLen = 0;
            int *intPtr = nullptr;
            char *charPtr = nullptr;

            get_inode_f(&intPtr, &size);
            inode_ = std::vector<int>(intPtr, intPtr + size);

            get_isc_f(&intPtr, &size);
            isc_ = std::vector<int>(intPtr, intPtr + size);

            get_link_f(&intPtr, &size);
            link_ = std::vector<int>(intPtr, intPtr + size);

            get_itp_f(&intPtr, &size);
            itp_ = std::vector<int>(intPtr, intPtr + size);

            get_typ_f(&charPtr, &strLen, &size);
            typ_ = strToVec(charPtr, strLen, size);

            get_tag_f(&charPtr, &strLen, &size);
            tag_ = strToVec(charPtr, strLen, size);

            get_jmpb_f(&intPtr, &size);
            jmpb_ = std::vector<int>(intPtr, intPtr + size);
        }

        void BufrDataManager::loadDataInfo() {

        }

        void BufrDataManager::deleteTableInfo() {
            delete_subset_data_f();
        }

        void BufrDataManager::deleteDataInfo() {
            delete_subset_data_f();
        }

        std::vector<std::string> BufrDataManager::strToVec(const char *str, size_t strLen, size_t size) {
            std::vector<std::string> res(size / strLen);

            for (size_t i = 0; i < size; i += strLen) {
                if (str[i] == '\0') break;
                res[i / strLen] = std::string(str + i, strLen);
            }

            return res;
        }
    }
}