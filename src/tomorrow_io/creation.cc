/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <exception>
#include <map>
#include <string>

#include "product.h"

namespace tio_converter {

void create_vars_tropics(ioda::ObsGroup &out, ioda::Variable Location, ioda::Variable Channel,
                         ioda::VariableCreationParameters uint8_params,
                         ioda::VariableCreationParameters float_params) {
  using namespace ioda;
  using namespace std;
  // The TROPICS calQualityFlag is channels * scans * spots.
  // We need to break up the variable's contents into several sub-variables.
  // Several of these are covered in the general TIO/TROPICS function.
  // | IODA Name           | TROPICS Bit Pos | Meaning |
  // | ------------------- | --------------- | ------- |
  // | flagNonOcean_legacy | 1               | Is this ocean or not? |
  // | flagIntrusion       | 2               | Solar or Lunar intrusion (TROPICS only) |
  // | flagManeuver        | 3               | Is spacecraft repositioning? |
  // | flagColdCal         | 4               | Cold caibration inconsistency |
  // | flagHotCal          | 5               | Hot calibration inconsistency |
  // | flagAscDesc         | 6               | Ascending / Descending flag. 0: asc, 1: desc. |
  // | flagDayNight        | 7               | Day / night flag. 0: day, 1: night. |
  // | flagPLOrientation   | 8               | Payload orientation (0: fore / 1: aft) |

  Variable flagNonOcean_legacy = out.vars.createWithScales<uint8_t>(
    "MetaData/nonOceanFlag_legacy", {Location, Channel}, uint8_params);

  Variable flagIntrusion = out.vars.createWithScales<uint8_t>("MetaData/intrusionFlag",
                                                              {Location, Channel}, uint8_params);
  flagIntrusion.atts.add("description", string("Set to 1 if potential solar/lunar intrusion."));

  Variable flagHotCal = out.vars.createWithScales<uint8_t>("MetaData/hotCalibrationFlag",
                                                           {Location, Channel}, uint8_params);
  flagHotCal.atts.add("description", string("Set to 1 if hot calibration is inconsistent."));

  Variable compositePreQCFlag = out.vars.createWithScales<uint8_t>(
    "MetaData/compositePreQCFlag", {Location, Channel}, uint8_params);
  compositePreQCFlag.atts.add(
    "description",
    std::string("Set to 1 if this observation has failed one or more upstream checks."));
}

void create_vars_tio(ioda::ObsGroup &out, ioda::Variable Location, ioda::Variable Channel,
                     ioda::VariableCreationParameters uint8_params,
                     ioda::VariableCreationParameters float_params) {
  using namespace ioda;
  using namespace std;
  Variable MultiMask
    = out.vars.createWithScales<uint8_t>("MetaData/multiMask", {Location}, uint8_params);
  MultiMask.atts.add("description",
                     std::string("1=clear land; 2=cloudy ocean; 3=clear ocean, 4=cloudy land"));
  MultiMask.atts.add("long_name",
                     std::string("Combined land/ocean/cloud mask derived from channel 1."));

  Variable clear_fraction = out.vars.createWithScales<float>("MetaData/cloudClearFraction",
                                                             {Location, Channel}, float_params);
  clear_fraction.atts.add(
    "description", std::string("Cloud-clear fraction of each spot from time-matched geostationary "
                               "satellite cloud mask, weighted by each channel's antenna pattern"));
  clear_fraction.atts.add("long_name", std::string("Cloud-clear fraction"));
  clear_fraction.atts.add("units", std::string("1"));

  Variable flagICTCal = out.vars.createWithScales<uint8_t>("MetaData/ICTCalibrationFlag",
                                                           {Location, Channel}, uint8_params);
  flagICTCal.atts.add("description",
                      std::string("Outlier detection flag for internal calibration target spots."));
  flagICTCal.atts.add("long_name", std::string("Internal Calibration Target Spot Flag"));

  Variable flagLunarIntrusion = out.vars.createWithScales<uint8_t>(
    "MetaData/lunarIntrusionFlag", {Location, Channel}, uint8_params);
  flagLunarIntrusion.atts.add("description",
                              std::string("True if there is a lunar intrusion into the cold space "
                                          "or noise diode calibration sectors."));
  flagLunarIntrusion.atts.add("long_name", std::string("Lunar intrusion flag"));

  Variable flagNDCal = out.vars.createWithScales<uint8_t>("MetaData/NDCalibrationFlag",
                                                          {Location, Channel}, uint8_params);
  flagNDCal.atts.add("description",
                     std::string("Outlier detection flag for noise diode calibration spots."));
  flagNDCal.atts.add("long_name", std::string("Noise Diode Calibration Spot Flag"));

  Variable flagSolarIntrusion = out.vars.createWithScales<uint8_t>(
    "MetaData/solarIntrusionFlag", {Location, Channel}, uint8_params);
  flagSolarIntrusion.atts.add("description",
                              std::string("True if there is a solar intrusion into the cold space "
                                          "or noise diode calibration sectors."));
  flagSolarIntrusion.atts.add("long_name", std::string("Solar intrusion flag"));

  Variable land_fraction = out.vars.createWithScales<float>("MetaData/landAreaFraction",
                                                            {Location, Channel}, float_params);
  land_fraction.atts.add("description",
                         std::string("Land fraction weighted by each channel's antenna pattern"));
  land_fraction.atts.add("long_name", std::string("Land fraction"));
  land_fraction.atts.add("units", std::string("1"));
}

void create_vars(TMS_Type typ, int sat_bufr_id, int sat_sub_bufr_id, int inst_bufr_id,
                 ioda::ObsGroup &out, ioda::Variable Location, ioda::Variable Channel) {
  using namespace ioda;
  VariableCreationParameters datetime_params;
  VariableCreationParameters float_params;
  VariableCreationParameters uint8_params;

  datetime_params.chunk = true;
  datetime_params.compressWithGZIP();
  datetime_params.setFillValue<int64_t>(0);  // Must be set for ioda to read this variable.

  float_params.chunk = true;
  float_params.compressWithGZIP();
  float_params.setFillValue<float>(-999);

  uint8_params.chunk = true;
  uint8_params.compressWithGZIP();
  uint8_params.setFillValue<uint8_t>(255);

  // MetaData

  //   datetime

  Variable datetime
    = out.vars.createWithScales<int64_t>("MetaData/dateTime", {Location}, datetime_params);
  datetime.atts.add("units", std::string("seconds since 1970-01-01T00:00:00Z"));

  //   Orbit geometry

  Variable latitude
    = out.vars.createWithScales<float>("MetaData/latitude", {Location}, float_params);
  latitude.atts.add("units", std::string("degrees_north"));

  Variable longitude
    = out.vars.createWithScales<float>("MetaData/longitude", {Location}, float_params);
  longitude.atts.add("units", std::string("degrees_east"));

  Variable sensorAzimuthAngle
    = out.vars.createWithScales<float>("MetaData/sensorAzimuthAngle", {Location}, float_params);
  sensorAzimuthAngle.atts.add("units", std::string("degrees"));

  Variable sensorViewAngle
    = out.vars.createWithScales<float>("MetaData/sensorViewAngle", {Location}, float_params);
  sensorViewAngle.atts.add("units", std::string("degrees"));

  Variable sensorZenithAngle
    = out.vars.createWithScales<float>("MetaData/sensorZenithAngle", {Location}, float_params);
  sensorZenithAngle.atts.add("units", std::string("degrees"));

  Variable solarAzimuthAngle
    = out.vars.createWithScales<float>("MetaData/solarAzimuthAngle", {Location}, float_params);
  solarAzimuthAngle.atts.add("units", std::string("degrees"));

  Variable solarZenithAngle
    = out.vars.createWithScales<float>("MetaData/solarZenithAngle", {Location}, float_params);
  solarZenithAngle.atts.add("units", std::string("degrees"));

  Variable lunarAzimuthAngle
    = out.vars.createWithScales<float>("MetaData/lunarAzimuthAngle", {Location}, float_params);
  lunarAzimuthAngle.atts.add("units", std::string("degrees"));

  Variable lunarZenithAngle
    = out.vars.createWithScales<float>("MetaData/lunarZenithAngle", {Location}, float_params);
  lunarZenithAngle.atts.add("units", std::string("degrees"));

  //   Diagnostic flags

  Variable flagAscDesc = out.vars.createWithScales<uint8_t>("MetaData/satelliteAscendingFlag",
                                                            {Location}, uint8_params);
  flagAscDesc.atts.add("description", std::string("True if in descending portion of orbit."));
  flagAscDesc.atts.add("long_name", std::string("Ascending/Descending flag"));

  Variable flagColdCal = out.vars.createWithScales<uint8_t>("MetaData/coldCalibrationFlag",
                                                            {Location, Channel}, uint8_params);
  flagColdCal.atts.add("description",
                       std::string("Outlier detection flag for deep space calibration spots."));
  flagColdCal.atts.add("long_name", std::string("Cold Calibration Spot Flag"));

  Variable flagDayNight
    = out.vars.createWithScales<uint8_t>("MetaData/dayOrNightQualifier", {Location}, uint8_params);
  flagDayNight.atts.add("description",
                        std::string("True if earth is between the sun and spacecraft."));
  flagDayNight.atts.add("long_name", std::string("Day/Night flag"));

  Variable flagManeuver = out.vars.createWithScales<uint8_t>("MetaData/spacecraftManeuverFlag",
                                                             {Location}, uint8_params);
  flagManeuver.atts.add("description",
                        std::string("True if the spacecraft is in an active maneuver."));
  flagManeuver.atts.add("long_name", std::string("Spacecraft maneuver flag"));

  Variable flagNonOcean
    = out.vars.createWithScales<uint8_t>("MetaData/nonOceanFlag", {Location}, uint8_params);
  flagNonOcean.atts.add(
    "description", std::string("0 is ocean, 1 is land, coastline, or undefined. Uses ch 1 spot."));
  flagNonOcean.atts.add("long_name", std::string("Non-ocean Flag"));

  Variable flagPLOrientation = out.vars.createWithScales<uint8_t>("MetaData/payloadOrientationFlag",
                                                                  {Location}, uint8_params);
  flagPLOrientation.atts.add("description",
                             std::string("True if spacecraft is flying payload-first."));
  flagPLOrientation.atts.add("long_name", std::string("Payload Orientation flag"));

  // ObsValue and ObsError

  Variable brightnessTemperature = out.vars.createWithScales<float>(
    "ObsValue/brightnessTemperature", {Location, Channel}, float_params);
  brightnessTemperature.atts.add("units", std::string("K"));
  // ObsError/brightnessTemperature is needed for ioda to read the file. We fill with dummy values since
  // we will assign error upstream in later processing.
  Variable brightnessTemperatureError = out.vars.createWithScales<float>(
    "ObsError/brightnessTemperature", {Location, Channel}, float_params);
  brightnessTemperatureError.atts.add("units", std::string("K"));

  //   Satellite instrument specs
  const size_t numLocs  = Location.getDimensions().dimsCur[0];
  const size_t numScans = numLocs / 81;
  create_sat_inst_specs(typ, sat_bufr_id, sat_sub_bufr_id, inst_bufr_id, out, Location, Channel,
                        numScans);

  if (typ == TMS_Type::TIO) create_vars_tio(out, Location, Channel, uint8_params, float_params);
  if (typ == TMS_Type::TROPICS)
    create_vars_tropics(out, Location, Channel, uint8_params, float_params);
}

}  // namespace tio_converter
