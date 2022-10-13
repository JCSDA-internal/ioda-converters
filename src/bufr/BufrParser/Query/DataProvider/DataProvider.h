/*
 * (C) Copyright 2022 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <functional>
#include <set>
#include <string>
#include <vector>
#include <math.h>
#include <memory>
#include <gsl/gsl-lite.hpp>
#include <unordered_map>

#include "bufr_interface.h"
#include "../QuerySet.h"



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

    const std::unordered_map<std::string, Typ> TypMap =
        {{"SUB",            Typ::Subset},
         {"DRP",        Typ::DelayedRep},
         {"REP",          Typ::FixedRep},
         {"DRS", Typ::DelayedRepStacked},
         {"DRB",     Typ::DelayedBinary},
         {"SEQ",          Typ::Sequence},
         {"RPC",            Typ::Repeat},
         {"RPS",     Typ::StackedRepeat},
         {"NUM",            Typ::Number},
         {"CHR",         Typ::Character}};



    struct TypeInfo
    {
        int scale = 0;
        int reference = 0;
        int bits = 0;
        std::string unit;
        std::string description;

        bool isString() const { return unit == "CCITT IA5"; }
        bool isSigned() const
        {
            // To better support Fortran clients for the generated ObsGroups we will assume all
            // fields are signed. Otherwise this code would be reference < 0.
            return true;
        }
        bool isInteger() const { return scale <= 0; }
        bool is64Bit() const
        {
            bool is64Bit;
            if (isInteger() && !isSigned())
            {
                is64Bit = (log2((pow(2, bits) - 1) / pow(10, scale) + reference) > 32);
            }
            else if (isInteger() && isSigned())
            {
                is64Bit = (log2(fmax(-1 * reference,
                    (pow(2, bits - 1) - 1) / pow(10, scale) + reference) * 2) + 1 > 32);
            }
            else
            {
                is64Bit = false;
            }

            return is64Bit;
        }
    };

    class DataProvider;
    typedef std::shared_ptr<DataProvider> DataProviderType;

    /// \brief Responsible for exposing the data found in a BUFR file in a C friendly way.
class DataProvider : public std::enable_shared_from_this<DataProvider>
    {
     public:
        DataProvider() = delete;

        explicit DataProvider(const std::string filePath) :
            filePath_(filePath)
        {
        }

        ~DataProvider() = default;

        virtual void run(const QuerySet& querySet,
                         const std::function<void(const DataProviderType&)> processMsg,
                         const std::function<void(const DataProviderType&)> processSubset,
                         const std::function<void(const DataProviderType&)> processFinish,
                         const std::function<bool(const DataProviderType&)> continueProcessing =
                             [](const DataProviderType&){return true;}) = 0;

        virtual void open() = 0;

        void close()
        {
            closbf_f(FileUnit);
            close_f(FileUnit);
        }

        void rewind()
        {
            close();
            open();
        }

        /// \brief Tells the Fortran BUFR interface to delete its temporary data structures that are
        /// are needed to support this class instanc.
        void deleteData();

        /// \brief Get the subset string for the currently active message subset.
        std::string getSubset() const { return subset_; }

        /// \brief Get a complete set of subsets in the data file. WARNING: using this will be slow
        ///        and reset the file pointer.
        std::set<std::string> getSubsets();

        inline FortranIdx getInode() const { return inode_; }
        inline FortranIdx getNVal() const { return nval_; }
        inline FortranIdx getInv(FortranIdx idx) const { return inv_[idx - 1]; }
        inline double getVal(FortranIdx idx) const { return val_[idx - 1]; }
        TypeInfo getTypeInfo(FortranIdx idx) const;

        virtual inline FortranIdx getIsc(FortranIdx idx) const = 0;
        virtual inline FortranIdx getLink(FortranIdx idx) const = 0;
        virtual inline FortranIdx getItp(FortranIdx idx) const = 0;
        virtual inline FortranIdx getJmpb(FortranIdx idx) const = 0;
        virtual inline Typ getTyp(FortranIdx idx) const = 0;
        virtual inline std::string getTag(FortranIdx idx) const = 0;

     protected:
        const static int FileUnit = 12;

        const std::string filePath_;
        std::string subset_;

        // Subset data
        int inode_;
        int nval_;
        int bufrLoc_;
        gsl::span<const double> val_;
        gsl::span<const int> inv_;

        virtual void updateTableData(const std::string& subset) = 0;

        /// \brief Read the data from the BUFR interface for the current subset and reset the
        /// internal data structures.
        ////// \param bufrLoc The Fortran idx for the subset we need to read.
        void updateData(const std::string& subset, int bufrLoc);
    };
}  // namespace bufr
}  // namespace Ingester
