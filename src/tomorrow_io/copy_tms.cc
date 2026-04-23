/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <bitset>
#include <exception>
#include <iostream>
#include <map>
#include <valarray>
#include <vector>

#include "copy.h"
#include "product.h"

namespace tio_converter {

void expandAndCopyCalQualityFlag(const ioda::Variable &from,
                                 std::vector<std::pair<std::bitset<8>, ioda::Variable>> &bits_to) {
  // Source var has dims of channels x scans x spots
  // Destination vars have dims of Location x Channel.
  // A bit of remapping magic must occur to keep the values in line.
  // Let's read and write one channel at a time.
  using namespace ioda;
  using std::valarray;
  using std::vector;
  Dimensions from_dims             = from.getDimensions();
  Type mem_type                    = from.getType();
  size_t num_channels              = from_dims.dimsCur[0];
  size_t num_locations_per_channel = from_dims.dimsCur[1] * from_dims.dimsCur[2];
  size_t buf_typesz                = mem_type.getSize();
  size_t buf_sz                    = num_locations_per_channel * buf_typesz;

  vector<char> buf(buf_sz);

  for (size_t ich = 0; ich < num_channels; ++ich) {
    Selection from_file_selection;
    vector<Dimensions_t> from_starts{(Dimensions_t)ich, 0, 0},
      from_counts{1, from_dims.dimsCur[1], from_dims.dimsCur[2]};
    from_file_selection.extent(from_dims.dimsCur)
      .select({SelectionOperator::SET, from_starts, from_counts});

    Selection buf_selection;
    vector<Dimensions_t> buf_starts{0, 0}, buf_counts{from_dims.dimsCur[1], from_dims.dimsCur[2]};
    buf_selection.extent(buf_counts).select({SelectionOperator::SET, buf_starts, buf_counts});

    from.read(gsl::make_span(buf.data(), buf.size()), mem_type, buf_selection, from_file_selection);
    valarray<char> buf_from(buf.data(), buf_sz);

    // Break up the input data according to the bit mapping in bits_to.
    for (auto &bit_to : bits_to) {
      const auto &bit_mask = bit_to.first;
      Variable to          = bit_to.second;

      valarray<char> buf_to(buf_sz);
      buf_to = buf_from & (char)bit_mask.to_ulong();

      vector<char> buf_to2(std::begin(buf_to), std::end(buf_to));
      replace_if(
        buf_to2.begin(), buf_to2.end(), [](char val) { return val > 0; }, 1);

      Dimensions to_dims = to.getDimensions();

      Selection to_file_selection;
      vector<Dimensions_t> to_starts{0, (Dimensions_t)ich},
        to_counts{(Dimensions_t)num_locations_per_channel, 1};
      to_file_selection.extent(to_dims.dimsCur)
        .select({SelectionOperator::SET, to_starts, to_counts});

      to.write(gsl::make_span(buf_to2.data(), buf_to2.size()), mem_type, buf_selection,
               to_file_selection);
    }
  }
}

void convert_tropics(Converter_Params &p) {
  using namespace std;
  using namespace ioda;

  const DimensionRanges_t offset_start{{{p.locationOffset, p.locationOffset + p.inNumLocs - 1}}};

  //   Diagnostic flags

  auto makeBits = [](std::initializer_list<short> bits) -> std::bitset<8> {
    std::bitset<8> res;
    for (auto bit : bits) res.set(bit);
    return res;
  };
  vector<pair<bitset<8>, Variable>> calmap{
    {makeBits({0}), p.out.vars["MetaData/nonOceanFlag_legacy"]},
    {makeBits({1}), p.out.vars["MetaData/intrusionFlag"]},
    {makeBits({2}), p.out.vars["MetaData/spacecraftManeuverFlag"]},
    {makeBits({3}), p.out.vars["MetaData/coldCalibrationFlag"]},
    {makeBits({4}), p.out.vars["MetaData/hotCalibrationFlag"]},
    {makeBits({5}), p.out.vars["MetaData/satelliteAscendingFlag"]},
    {makeBits({6}), p.out.vars["MetaData/dayOrNightQualifier"]},
    {makeBits({7}), p.out.vars["MetaData/payloadOrientationFlag"]},
    {makeBits({1, 2, 3, 4}), p.out.vars["MetaData/compositePreQCFlag"]}};
  expandAndCopyCalQualityFlag(p.in.vars["calQualityFlag"], calmap);

  copy(VariableInfo{.var = p.in.vars["NonOceanFlag"]},
       VariableInfo{.var = p.out.vars["MetaData/nonOceanFlag"], .range = offset_start});

  // TROPICS TB has channel first, and we need to change to a var that is location first.
  for (size_t ich = 0; ich < 12; ++ich) {
    const DimensionRanges_t single_ch{{{ich, ich}}, {}, {}};
    const DimensionRanges_t dest_range{{{p.locationOffset, p.locationOffset + p.inNumLocs - 1}},
                                       {{ich, ich}}};

    copy(VariableInfo{.var = p.in.vars["brightness_temperature"], .range = single_ch},
         VariableInfo{.var = p.out.vars["ObsValue/brightnessTemperature"], .range = dest_range});
  }
}

void convert_tio(Converter_Params &p) {
  using namespace std;
  using namespace ioda;

  const DimensionRanges_t only_ch1{{}, {}, {{0, 0}}};
  const DimensionRanges_t offset_start{{{p.locationOffset, p.locationOffset + p.inNumLocs - 1}}};

  //   Diagnostic flags

  // NOTE (RH): many of the same variables are common to TIO and TROPICS TMS satellites,
  // but they come from different source variables. Several of these are distinct variables
  // in TIO, but in TROPICS they are gathered into a single "calQualityFlag" variable.
  copy(VariableInfo{.var = p.in.vars["MultiMask"]},
       VariableInfo{.var = p.out.vars["MetaData/multiMask"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["clear_fraction"]},
       VariableInfo{.var = p.out.vars["MetaData/cloudClearFraction"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagICTCal"]},
       VariableInfo{.var = p.out.vars["MetaData/ICTCalibrationFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagLunarIntrusion"]},
       VariableInfo{.var = p.out.vars["MetaData/lunarIntrusionFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagNDCal"]},
       VariableInfo{.var = p.out.vars["MetaData/NDCalibrationFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagSolarIntrusion"]},
       VariableInfo{.var = p.out.vars["MetaData/solarIntrusionFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["land_fraction"]},
       VariableInfo{.var = p.out.vars["MetaData/landAreaFraction"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagAscDesc"], .range = only_ch1},
       VariableInfo{.var = p.out.vars["MetaData/satelliteAscendingFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagColdCal"]},
       VariableInfo{.var = p.out.vars["MetaData/coldCalibrationFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagDayNight"], .range = only_ch1},
       VariableInfo{.var = p.out.vars["MetaData/dayOrNightQualifier"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagManeuver"], .range = only_ch1},
       VariableInfo{.var = p.out.vars["MetaData/spacecraftManeuverFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagNonOcean"], .range = only_ch1},
       VariableInfo{.var = p.out.vars["MetaData/nonOceanFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["flagPLOrientation"], .range = only_ch1},
       VariableInfo{.var = p.out.vars["MetaData/payloadOrientationFlag"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["brightness_temperature"]},
       VariableInfo{.var = p.out.vars["ObsValue/brightnessTemperature"], .range = offset_start});

  // Note (RH):
  // For user convenience, we could introduce a "compositePreQCFlag" that merges the flags in
  // ICTCalibrationFlag, NDCalibrationFlag, coldCalibrationFlag, spacecraftManeuverFlag,
  // lunarIntrusionFlag, and solarIntrusionFlag.
  // This flag is constructed by reading in the relevant variables and running a bitwise OR on the contents.
  /*
  {
    VariableInfo vartest{.var   = p.out.vars["MetaData/spacecraftManeuverFlag"],
                         .range = offset_start};
    VariableDerivedInfo vartest_info(vartest);
    const size_t sz = vartest_info.selection_num_elements;
    valarray<char> compositePreQCFlag_data(sz);
    vector<char> buffer(sz);

    const vector<string> qcvars{
      {"MetaData/ICTCalibrationFlag"},  {"MetaData/lunarIntrusionFlag"},
      {"MetaData/NDCalibrationFlag"},   {"MetaData/solarIntrusionFlag"},
      {"MetaData/coldCalibrationFlag"}, {"MetaData/spacecraftManeuverFlag"}};
    for (const auto &qcvar : qcvars) {
      std::cout << qcvar << std::endl;
      p.out.vars[qcvar].read(
        gsl::make_span(buffer.data(), sz),  // Read into this buffer
	// No need for a type conversion. These QC flags are just a stream of single-byte fields.
        vartest_info.type,
        vartest_info.selection_membuf,  // Where should this be put in "buffer"?
        vartest_info.selection_ioda     // The hyperslab to be read
      );
      compositePreQCFlag_data = compositePreQCFlag_data & valarray<char>(buffer.data(), sz);
    }
    vector<char> outvar(begin(compositePreQCFlag_data), end(compositePreQCFlag_data));
    p.out.vars["MetaData/compositePreQCFlag"].write(
      gsl::make_span(outvar.data(), sz), vartest_info.type, vartest_info.selection_membuf,
      vartest_info.selection_ioda);
  }
  */
}

void convert(TMS_Type typ, Converter_Params &p) {
  using namespace std;
  using namespace ioda;

  // Note that the ordering of the variable dimensions is inconsistent between
  // TROPICS and TMS products.
  // TROPICS: channels x scans x spots
  // TMS v6: spots x scans x channels
  // Outgoing: Location x Channel
  // So long as all of the variables are treated in
  // the same way within a product, we are okay. TMS will be written with locations in
  // spot x scan ordering, while tropics will be written in scan x spot.
  // The subsidiary functions will handle any discrepancies when relevant. This
  // only applies for manually reconstructed fields like dateTime.

  // This specifies that only data for the first channel or band should be copied.
  const DimensionRanges_t only_ch1_TMS{{}, {}, {{0, 0}}};
  const DimensionRanges_t only_band1_TROPICS{{{0, 0}}, {}, {}};
  const DimensionRanges_t single_ch{(typ == TMS_Type::TIO) ? only_ch1_TMS : only_band1_TROPICS};

  // This specifies that we are copying to a certain range in the output variable.
  const DimensionRanges_t offset_start{{{p.locationOffset, p.locationOffset + p.inNumLocs - 1}}};

  // MetaData

  //   datetime
  convert_datetime(typ, p);

  //   Satellite instrument specs are populated elsewhere. No copying needed.

  //   Orbit geometry

  copy(VariableInfo{.var = p.in.vars["latitude"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/latitude"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["longitude"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/longitude"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["sensor_azimuth_angle"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/sensorAzimuthAngle"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["sensor_view_angle"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/sensorViewAngle"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["sensor_zenith_angle"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/sensorZenithAngle"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["solar_azimuth_angle"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/solarAzimuthAngle"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["solar_zenith_angle"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/solarZenithAngle"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["lunar_azimuth_angle"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/lunarAzimuthAngle"], .range = offset_start});

  copy(VariableInfo{.var = p.in.vars["lunar_zenith_angle"], .range = single_ch},
       VariableInfo{.var = p.out.vars["MetaData/lunarZenithAngle"], .range = offset_start});

  //   Diagnostic flags

  // ObsValue is handled in the TIO and TOPICS-specific conversion section.

  // No ObsError assignments

  // TIO and TROPICS-specific conversions
  if (typ == TMS_Type::TIO)
    convert_tio(p);
  else
    convert_tropics(p);
}

}  // namespace tio_converter
