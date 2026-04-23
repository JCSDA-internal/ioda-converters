/*
 * (C) Copyright 2024 The Tomorrow Companiec, Inc.
 *
 * This software is licensed under the terms of the Apache Licence Version 2.0
 * which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
 */
#include <chrono>
#include <cstdlib>  // setenv
#include <iomanip>  // std::put_time
#include <iostream>
#include <sstream>
#include <vector>

#include "product.h"

#if __cplusplus < 202002L
/// \brief Stringify a time point.
/// \note This code will be superseded with the switch to C++20, as
///   C++20 adds ostream operators for all time types.
template <class Clock_T, class Duration_T>
std::ostream &operator<<(std::ostream &os, const std::chrono::time_point<Clock_T, Duration_T> &tp) {
  const std::time_t t = Clock_T::to_time_t(tp);
  os << std::put_time(std::gmtime(&t), ioda::Types::Chrono_Time_Format);
  return os;
}
#endif

typedef int64_t Chrono_Time_Rep_t;
typedef std::ratio<1, 1> Chrono_Time_Period_t;
typedef std::chrono::system_clock Chrono_Clock_t;
typedef std::chrono::duration<Chrono_Time_Rep_t, Chrono_Time_Period_t> Chrono_Duration_t;
typedef std::chrono::time_point<Chrono_Clock_t, Chrono_Duration_t> Chrono_Time_Point_t;

namespace tio_converter {

namespace {
Chrono_Time_Point_t makeTimePoint(uint16_t year, uint16_t month, uint16_t dayofmonth, uint16_t hour,
                                  uint16_t min, uint16_t sec) {
  struct std::tm timeTemp {};
  timeTemp.tm_year  = year - 1900;
  timeTemp.tm_mon   = month - 1;
  timeTemp.tm_mday  = dayofmonth;
  timeTemp.tm_hour  = hour;
  timeTemp.tm_min   = min;
  timeTemp.tm_sec   = sec;
  timeTemp.tm_isdst = 0;
  std::time_t tt    = std::mktime(&timeTemp);
  return std::chrono::time_point_cast<Chrono_Duration_t>(
    std::chrono::system_clock::from_time_t(tt));
}

std::string stringifyEpoch(const Chrono_Time_Point_t &epoch) {
  auto tt = std::chrono::system_clock::to_time_t(epoch);
  struct std::tm *epochTime;
  epochTime = std::gmtime(&tt);
  char epochString[21];
  size_t ret = std::strftime(epochString, 21, "%Y-%m-%dT%H:%M:%SZ", epochTime);
  if (!ret) throw std::logic_error("Invalid time");
  return std::string("seconds since ") + std::string(epochString);
}

}  // namespace

void convert_datetime(TMS_Type typ, Converter_Params &p) {
  using namespace ioda;
  using namespace std;

  // Force UTC time zone. Needed for time calculations before C++20.
  setenv("TZ", "UTC", 1);

  Variable datetime = p.out.vars["MetaData/dateTime"];
  auto epoch        = makeTimePoint(1970, 1, 1, 0, 0, 0);

  // dateTime
  vector<Chrono_Time_Point_t> scantimestamps(p.inNumScans);
  {
    vector<uint16_t> hour, minute, second, month, day;
    vector<uint16_t> year;
    // IODA's type system does not support 8-bit unsigned integers because these are interpreted as
    // 'unsigned char' in C++, and not all compilers have support.
    p.in.vars["Hour"].read(hour);
    p.in.vars["Minute"].read(minute);
    p.in.vars["Second"].read(second);
    p.in.vars["Month"].read(month);
    p.in.vars["Day"].read(day);
    p.in.vars["Year"].read(year);
    for (size_t i = 0; i < p.inNumScans; ++i) {
      scantimestamps[i] = makeTimePoint(year[i], month[i], day[i], hour[i], minute[i], second[i]);

      // For debugging:
      // if (!i) {
      //   std::cout << "First time point is " << year[0] << "-" << month[0] << "-" << day[0] << "  "
      //             << hour[0] << ":" << minute[0] << ":" << second[0] << std::endl;
      // }
    }

    vector<int64_t> locationtimestamps_as_seconds(p.inNumLocs);
    for (size_t scan = 0; scan < p.inNumScans; ++scan)
      for (size_t spot = 0; spot < p.inNumSpots; ++spot) {
        // The locations need to align to typical variable coordinate ordering in the file.
        // We use the brightness_temperature variable as the basis for this ordering.
        // TMS dims are spot x scan x ch.
        // TROPICS dims are ch x scan x spot.
        // The relative ordering of spot and scan are key.
        size_t i = (typ == TMS_Type::TIO) ? (spot * p.inNumScans) + scan   // T.io TMS
                                          : (scan * p.inNumSpots) + spot;  // TROPICS TMS
        locationtimestamps_as_seconds[i]
          = std::chrono::duration_cast<std::chrono::seconds>(scantimestamps[scan] - epoch).count();
      }

    // time stamps are for each scan line, but need to map to each location
    vector<Dimensions_t> sel_zero{0}, sel_start{p.locationOffset}, sel_count{p.inNumLocs};

    ioda::Selection selection_ioda, selection_membuf;
    selection_ioda.extent(p.out.vars["Location"].getDimensions().dimsCur)
      .select({SelectionOperator::SET, sel_start, sel_count});
    selection_membuf.extent({p.inNumLocs}).select({SelectionOperator::SET, sel_zero, sel_count});

    datetime.write<int64_t>(locationtimestamps_as_seconds, selection_membuf, selection_ioda);
  }
}
}  // namespace tio_converter
