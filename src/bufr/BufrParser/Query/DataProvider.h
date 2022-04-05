/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <string>
#include <vector>
#include <memory>

#include <gsl/gsl-lite.hpp>

namespace Ingester{
namespace bufr {
    typedef unsigned int FortranIdx;

    enum class Typ
    {
        Subset,
        DelayedRep,
        FixedRep,
        DelayedRepStacked,
        DelayedBinary,
        Sequence,
        Repeat,
        StackedRepeat,
        Number,
        Character
    };

    class DataProvider
    {
     public:
        DataProvider() = default;
        ~DataProvider() = default;

        void updateData(int bufrLoc);
        void deleteData();

        std::string getSubset() const { return subset_; }

        // Getters to get the raw data by idx. Since fortran indices are 1-based,
        // we need to subtract 1 to get the correct c style index.
        inline FortranIdx getIsc(FortranIdx idx) const { return isc_[idx - 1]; }
        inline FortranIdx getLink(FortranIdx idx) const { return link_[idx - 1]; }
        inline FortranIdx getItp(FortranIdx idx) const { return itp_[idx - 1]; }
        inline FortranIdx getJmpb(FortranIdx idx) const { return jmpb_[idx - 1]; }
        inline Typ getTyp(FortranIdx idx) const { return typ_[idx - 1]; }
        inline std::string getTag(FortranIdx idx) const { return tag_[idx - 1]; }

        inline FortranIdx getInode() const { return inode_; }
        inline FortranIdx getNVal() const { return nval_; }
        inline FortranIdx getInv(FortranIdx idx) const { return inv_[idx - 1]; }
        inline double getVal(FortranIdx idx) const { return val_[idx - 1]; }

     private:
        std::string subset_;

        // Table data;
        gsl::span<const int> isc_;
        gsl::span<const int> link_;
        gsl::span<const int> itp_;
        gsl::span<const int> jmpb_;
        std::vector<Typ> typ_;
        std::vector<std::string> tag_;

        // Subset data
        int inode_;
        int nval_;
        gsl::span<const double> val_;
        gsl::span<const int> inv_;

        bool tableInfoLoaded_ = false;
    };
}  // namespace bufr
}  // namespace Ingester
