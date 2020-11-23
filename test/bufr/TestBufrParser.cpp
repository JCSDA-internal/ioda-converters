/*
 * (C) Copyright 2020 NOAA/NWS/NCEP/EMC
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */

#include "oops/runs/Run.h"

#include "TestBufrParser.h"

int main(int argc,  char ** argv)
{
    oops::Run run(argc, argv);
    Ingester::test::BufrParser tests;
    return run.execute(tests);
}
