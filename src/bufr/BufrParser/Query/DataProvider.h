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

    /// \brief Responsible for exposing the data found in a BUFR file in a C friendly way.
    class DataProvider
    {
     public:
        DataProvider() = default;
        ~DataProvider() = default;

        /// \brief Read the data from the BUFR interface for the current subset and reset the
        /// internal data structures.
        ////// \param bufrLoc The Fortran idx for the subset we need to read.
        void updateData(int bufrLoc);

        /// \brief Tells the Fortran BUFR interface to delete its temporary data structures that are
        /// are needed to support this class instanc.
        void deleteData();

        /// \brief Get the subset string for the currently active message subset.
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
