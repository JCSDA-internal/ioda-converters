/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#pragma once

#include <gsl/gsl>
#include <set>

#include "Eigen/Dense"

#include "BufrTypes.h"

namespace BufrParser
{
    /// \brief Accumulates provided data into a dynamically expanding Eigen Array
    class BufrAccumulator
    {
     public:
        /// \param numColumns Width of collected data.
        /// \param blockSize The amount to allocate when we need to extend the Eigen Array
        explicit BufrAccumulator(Eigen::Index numColumns, Eigen::Index blockSize = 50000);

        /// \brief Add row of data to the internal ddata structure
        /// \param newRow Collection of values to add (size must match the number of columns)
        void addRow(std::vector<IodaEncoder::FloatType>& newRow);

        /// \brief Get an Eigen Array that contains a slice of the collected data.
        /// \param startCol Column offset where to start (should be size(channels) * paramNumber)
        /// \param channels Channels to collect starting from the start position
        IodaEncoder::EncoderArray getData(Eigen::Index startCol, const Channels& channels = {1});

        /// \brief Start over
        void reset();

        // Getters
        inline Eigen::Index getNumColumns() const { return numColumns_; }

     private:
        /// \brief Eigen Array that holds the accumulated data
        IodaEncoder::EncoderArray dataArray_;

        /// \brief Total number of columns (width of data structure)
        Eigen::Index numColumns_;

        /// \brief Number of data rows of collected data.
        Eigen::Index numDataRows_;

        /// \brief Amount to allocate when we need to extend the Eigen Array
        Eigen::Index blockSize_;
    };
}  // namespace BufrParser
