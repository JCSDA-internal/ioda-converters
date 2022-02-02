//
// Created by rmclaren on 1/30/22.
//

#pragma once

#include <string>
#include <vector>

namespace Ingester
{
    namespace bufr {
        class BufrDataManager {
        public:
            BufrDataManager() = default;
            ~BufrDataManager() = default;

            void loadTableInfo();
            void loadDataInfo();
            void deleteTableInfo();
            void deleteDataInfo();

            // Getters to get the raw data by idx
            inline auto getInode(int idx) const { return inode_[idx]; }
            inline auto getIsc(int idx) const { return isc_[idx]; }
            inline auto getLink(int idx) const { return link_[idx]; }
            inline auto getItp(int idx) const { return itp_[idx]; }
            inline auto getJmpb(int idx) const { return jmpb_[idx]; }
            inline auto getTyp(int idx) const { return typ_[idx]; }
            inline auto getTag(int idx) const { return tag_[idx]; }


        private:
            std::vector<std::string> strToVec(const char *str, size_t strLen, size_t size);

            // BUFR table data;
            std::vector<int> inode_;
            std::vector<int> isc_;
            std::vector<int> link_;
            std::vector<int> itp_;
            std::vector<int> jmpb_;
            std::vector<std::string> typ_;
            std::vector<std::string> tag_;
        };
    }
}

