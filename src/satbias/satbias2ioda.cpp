#include <memory>
#include <string>
#include <vector>
#include <fstream>

#include <Eigen/Dense>

#include "ioda/ObsGroup.h"
#include "ioda/Engines/HH.h"

#include "oops/util/missingValues.h"

/// GSI predictor names
namespace gsi_predictors {
  /// Default predictors; some predictors are different for
  /// goessndr .or. goes_img .or. ahi .or. seviri .or. ssmi .or. ssmis .or. gmi .or. abi
  const std::vector<std::string> default_predictors = {"constant",
                                                       "zenith_angle",
                                                       "cloud_liquid_water",
                                                       "lapse_rate_order_2",
                                                       "lapse_rate",
                                                       "cosine_of_latitude_times_orbit_node",
                                                       "sine_of_latitude",
                                                       "emissivity",
                                                       "scan_angle_order_4",
                                                       "scan_angle_order_3",
                                                       "scan_angle_order_2",
                                                       "scan_angle"
                                                      };
  const size_t npredictors = default_predictors.size();
}

/// Read info for one channel from the GSI text file
/// Returns sensor (instrument + satellite) name and channel index
void readonechannel(std::ifstream & infile, std::string & sensor, size_t & channel) {
  float tlap, tsum;
  std::size_t ntlapupdate;
  float par;
  infile >> sensor;
  infile >> channel;
  infile >> tlap;
  infile >> tsum;
  infile >> ntlapupdate;
  for (size_t ii = 0; ii < gsi_predictors::npredictors; ++ii) {
    infile >> par;
  }
}

/// Finds all sensors in the file (returned in \p sensors) and number of channels
/// for all the sensors (returned in \p nchannels)
void findSensorsChannels(const std::string & filename, std::vector<std::string> & sensors,
                         std::vector<int> & nchannels) {
  std::ifstream infile(filename);

  std::size_t ich;     //  sequential number
  std::string sensor;  //  sensor/instrument/satellite
  std::size_t channel;  //  channel number

  if (infile.is_open())
  {
    /// Read the first entry, save first sensor
    infile >> ich;
    readonechannel(infile, sensor, channel);
    sensors.push_back(sensor);
    size_t nsensors = 0;
    nchannels.push_back(1);

    while (infile >> ich)
    {
      readonechannel(infile, sensor, channel);
      if (sensor == sensors[nsensors]) {
        // still processing the same sensor, just update the number of channels
        nchannels[nsensors] ++;
      } else {
        // new sensor: move on
        sensors.push_back(sensor);
        nsensors++;
        nchannels.push_back(1);
      }
    }
  }
  infile.close();
}

/// Read bias coefficients for the \p sensor instrument+satellite in \p filename GSI coeff file
/// Returns coefficients in \p coeffs and channel indices in \p channels
void readObsBiasCoefficients(const std::string & filename, const std::string & sensor,
                             std::vector<int> & channels, Eigen::ArrayXXf & coeffs ) {
  std::ifstream infile(filename);

  std::size_t ich;     //  sequential number
  std::string nusis;   //  sensor/instrument/satellite
  std::size_t nuchan;  //  channel number
  float tlap, tsum;
  std::size_t ntlapupdate;

  if (infile.is_open())
  {
    float par;
    size_t jchan = 0;
    while (infile >> ich)
    {
      infile >> nusis;
      infile >> nuchan;
      infile >> tlap;
      infile >> tsum;
      infile >> ntlapupdate;
      if (nusis == sensor) {
        /// it's the sensor we're interested in; read in coefficients and channel
        /// indices
        for (size_t jpred = 0; jpred < gsi_predictors::npredictors; ++jpred) {
          infile >> par;
          coeffs(jpred, jchan) = par;
          channels[jchan] = nuchan;
        }
        jchan++;
      } else {
        /// not interested in this channel; passing
        for (size_t jpred = 0; jpred < gsi_predictors::npredictors; ++jpred) {
          infile >> par;
        }
      }
    }
  }
  infile.close();
}

// Return ObsGroup with bias coefficients for a given sensor
ioda::ObsGroup makeObsBiasObject(ioda::Group &empty_base_object, const std::string & filename,
                                 const std::string & sensor, const size_t nchannels) {
  // Channels & predictors
  const std::vector<std::string> & predictors = gsi_predictors::default_predictors;
  std::vector<int> channels(nchannels);
  long numPreds = predictors.size();
  long numChans = channels.size();

  // Allocate space for bias coefficients and read them from GSI satbias file
  Eigen::ArrayXXf biascoeffs(numPreds, numChans);
  readObsBiasCoefficients(filename, sensor, channels, biascoeffs);

  // Creating dimensions: npredictors & nchannels
  ioda::NewDimensionScales_t newDims;
  newDims.push_back(std::make_shared<ioda::NewDimensionScale<int>>("npredictors",
                    numPreds, numPreds, numPreds));
  newDims.push_back(std::make_shared<ioda::NewDimensionScale<int>>("nchannels",
                    numChans, numChans, numChans));

  // Construct an ObsGroup object, with 2 dimensions npred, nchans
  ioda::ObsGroup ogrp = ioda::ObsGroup::generate(empty_base_object, newDims);

  // Save the predictors and the channels values
  ioda::Variable predsVar = ogrp.vars.create<std::string>("predictors", {numPreds});
  predsVar.write(predictors);
  ioda::Variable chansVar = ogrp.vars.create<int>("channels", {numChans});
  chansVar.write(channels);

  // Set up the creation parameters for the bias coefficients variable
  ioda::VariableCreationParameters float_params;
  float_params.chunk = true;               // allow chunking
  float_params.compressWithGZIP();         // compress using gzip
  float missing_value = util::missingValue(missing_value);
  float_params.setFillValue<float>(missing_value);

  // Create a variable for bias coefficients, save bias coeffs to the variable
  ioda::Variable biasVar = ogrp.vars.createWithScales<float>("bias_coefficients",
                     {ogrp.vars["npredictors"], ogrp.vars["nchannels"]}, float_params);
  biasVar.writeWithEigenRegular(biascoeffs);

  return ogrp;
}


int main(int, char**) {
  /// Filenames hardcoded for now
  const std::string input_filename = "satbias_crtm_in";

  std::vector<std::string> sensors;
  std::vector<int> nchannels;

  /// Find all sensors and number of channels in the GSI bias coefficients file
  findSensorsChannels(input_filename, sensors, nchannels);

  std::cout << "Found " << sensors.size() << " sensors:" << std::endl;
  for (size_t jj = 0; jj < sensors.size(); ++jj) {
    std::cout << "-- " << sensors[jj] << ", " << nchannels[jj] << " channels." << std::endl;
    /// Create new ioda::Group
    /// For debugging, persist the ObsBias data structure on disk. Write as an HDF5 file.
    const std::string output_filename = "satbias_" + sensors[jj] + ".ioda";
    ioda::Group group = ioda::Engines::HH::createFile(output_filename,
                        ioda::Engines::BackendCreateModes::Truncate_If_Exists);
    makeObsBiasObject(group, input_filename, sensors[jj], nchannels[jj]);
  }
  return 0;
}
