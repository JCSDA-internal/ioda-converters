/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <map>
#include <vector>

#include "product.h"

namespace tio_converter {
void create_sat_inst_specs(TMS_Type typ, int sat_bufr_id, int sat_sub_bufr_id, int inst_bufr_id,
                           ioda::ObsGroup &out, ioda::Variable Location, ioda::Variable Channel,
                           size_t numScans) {
  using namespace ioda;
  using namespace std;
  const size_t numLocs = Location.getDimensions().dimsCur[0];

  vector<double> sensorCentralFrequency_GHz
    = (typ == TMS_Type::TROPICS)
        // TROPICS
        ? vector<double>{91.655, 114.50, 115.95, 116.65, 117.25, 117.80,
                         118.24, 118.58, 184.41, 186.51, 190.31, 204.80}  // TIO
        : vector<double>{91.65,  118.75, 118.75, 118.75, 118.75, 118.75,
                         118.75, 118.75, 184.41, 186.51, 190.31, 204.8};

  Variable sensorChannelNumber
    = out.vars.createWithScales<int>("MetaData/sensorChannelNumber", {Channel});
  Variable sensorCentralFrequency
    = out.vars.createWithScales<float>("MetaData/sensorCentralFrequency", {Channel});
  sensorCentralFrequency.atts.add("units", string("Hz"));

  // Custom missing value for bufr id fields.
  VariableCreationParameters id_params;
  id_params.chunk = true;
  id_params.compressWithGZIP();
  id_params.setFillValue<int32_t>(999);

  // satelliteIdentifier
  vector<int> vSatelliteIdentifier(numLocs, sat_bufr_id);
  Variable satelliteIdentifier
    = out.vars.createWithScales<int>("MetaData/satelliteIdentifier", {Location}, id_params);
  satelliteIdentifier.write<int>(vSatelliteIdentifier);

  // satelliteSubIdentifier
  vector<int> vSatelliteSubIdentifier(numLocs, sat_sub_bufr_id);
  Variable satelliteSubIdentifier
    = out.vars.createWithScales<int>("MetaData/satelliteSubIdentifier", {Location}, id_params);
  satelliteSubIdentifier.write<int>(vSatelliteSubIdentifier);

  // instrumentIdentifier
  vector<int> vSatelliteInstrument(numLocs, inst_bufr_id);
  Variable satelliteInstrument
    = out.vars.createWithScales<int>("MetaData/instrumentIdentifier", {Location}, id_params);
  satelliteInstrument.write<int>(vSatelliteInstrument);

  // sensorChannelNumber is a sequence if ints from 1 to number of channels
  vector<uint32_t> chnum(12);
  iota(chnum.begin(), chnum.end(), 1);
  sensorChannelNumber.write<uint32_t>(chnum);
  Channel.write<uint32_t>(chnum);
  // sensorCentralFrequency
  using namespace ioda::udunits;
  auto GHzToHz = Units("GHz").getConverterTo(Units("Hz"));
  vector<double> cfhz(sensorCentralFrequency_GHz.size());
  GHzToHz->convert(sensorCentralFrequency_GHz.data(), sensorCentralFrequency_GHz.size(),
                   cfhz.data());
  sensorCentralFrequency.write<double>(cfhz);

  // NOTE (RH): sensorScanPosition is deliberately not written. Users
  // should instead use sensorViewAngle and sensorZenithAngle when filtering.
}
}  // namespace tio_converter
