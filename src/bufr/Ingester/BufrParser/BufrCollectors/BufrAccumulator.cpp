/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "BufrAccumulator.h"


using namespace Ingester;
using namespace Eigen;

BufrAccumulator::BufrAccumulator(Index numColumns, Index blockSize) :
    dataArray_(blockSize, numColumns),
    numColumns_(numColumns),
    numDataRows_(0),
    blockSize_(blockSize)
{
}

void BufrAccumulator::addRow(double* newRow)
{
    if (numDataRows_ + 1 > dataArray_.rows())
    {
        dataArray_.conservativeResize(dataArray_.rows() + blockSize_, numColumns_);
    }

    dataArray_.row(numDataRows_) = Map<IngesterArray>(newRow, 1, numColumns_);
    numDataRows_++;
}

IngesterArray BufrAccumulator::getData(Index startCol, const Channels& channels)
{
    IngesterArray resultArr(numDataRows_, channels.size());
    unsigned int colIdx = 0;
    for (auto col : channels)
    {
        unsigned int offset = startCol + (col - 1);
        resultArr.block(0, colIdx, numDataRows_, 1) =
            dataArray_.block(0, offset, numDataRows_, 1);

        colIdx++;
    }

    return resultArr;
}

void BufrAccumulator::reset()
{
    numDataRows_ = 0;
}
