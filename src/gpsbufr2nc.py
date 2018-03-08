from __future__ import print_function
import ncepbufr, os, sys, netCDF4
from optparse import OptionParser
import numpy as np
from datetime import datetime

# Grab input arguments
ScriptName = os.path.basename(sys.argv[0])
UsageString = "USAGE: {0:s} [options] <obs_type> <ref_date> <input_prepbufr> <output_netcdf>".format(ScriptName)

# Parse command line
op = OptionParser(usage=UsageString)
op.add_option("-m", "--max-msgs", type="int", dest="max_msgs", default=-1,
              help="maximum number of messages to keep", metavar="<max_num_msgs>")

MyOptions, MyArgs = op.parse_args()

if len(MyArgs) != 4:
    print("ERROR: must supply exactly 4 arguments")
    print(UsageString)
    sys.exit(1)

MaxMsgs = MyOptions.max_msgs

ObsType = MyArgs[0] # 'bend' or 'refrac'
if not ObsType.startswith('bend') and not ObsType.startswith('refr'):
    print("ERROR: obs type string must start with 'bend' or 'refr'")
    sys.exit(1)

refdate = MyArgs[1]
if len(refdate) != 10:
    print("refdate must be YYYYMMDDHH")
    sys.exit(1)
bufrFname = MyArgs[2]
netcdfFname = MyArgs[3]

hdrstr ='YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU'

# read gpsro file.

bufr = ncepbufr.open(bufrFname)
nc = netCDF4.Dataset(netcdfFname,'w',format='NETCDF4')
nc.createDimension('nobs',None)
lat = nc.createVariable('Latitude',np.float32,'nobs',zlib=True)
lat.units='degrees north'
lon = nc.createVariable('Longitude',np.float32,'nobs',zlib=True)
lat.units='degress east'
hgt = nc.createVariable('Height',np.float32,'nobs',zlib=True)
hgt.units='meters'
time = nc.createVariable('Time',np.float32,'nobs',zlib=True)
bufr.advance()
YYYY = refdate[0:4]
MM = refdate[4:6]
DD = refdate[6:8]
HH = refdate[8:10]
bufr.rewind()
time.units = 'hours since %04s-%02s-%02s %02s:00 UTC' % (YYYY,MM,DD,HH)
ob = nc.createVariable('Observation',np.float32,'nobs',zlib=True)
ob.missing_value=bufr.missing_value
#if ObsType.startswith('bend'):
#    ob = nc.createVariable('Incremental_Bending_Angle',np.float32,'nobs')

nob = 0
while bufr.advance() == 0:
    print(bufr.msg_counter, bufr.msg_type, bufr.msg_date)
    while bufr.load_subset() == 0:
        hdr = bufr.read_subset(hdrstr).squeeze()
        yyyymmddhh ='%04i%02i%02i%02i%02i' % tuple(hdr[0:5])
        date = datetime(int(hdr[0]),int(hdr[1]),int(hdr[2]),int(hdr[3]),int(hdr[4]))
        timeval = netCDF4.date2num(date,units=time.units)
        satid = int(hdr[7])
        ptid = int(hdr[8])
        nreps_this_ROSEQ2 = bufr.read_subset('{ROSEQ2}').squeeze()
        nreps_this_ROSEQ1 = len(nreps_this_ROSEQ2)
        data1 = bufr.read_subset('ROSEQ1',seq=True) # bending angle
        data2 = bufr.read_subset('ROSEQ3',seq=True) # refractivity
        levs1 = data1.shape[1]
        levs2 = data2.shape[1]
        if levs1 != levs2:
            print('skip report due to bending angle/refractivity mismatch')
            continue
        levs = levs1
        print('sat id,platform transitter id, levels, yyyymmddhhmm =',\
        satid,ptid,levs1,yyyymmddhh)
        #print('k, height, lat, lon, ref, bend:')
        lats = []; lons = []; hgts = []; obs = []
        ncount = 0
        for k in range(levs):
            latval = data1[0,k]
            lonval = data1[1,k]
            hgtval = data2[0,k]
            try:
                if latval.mask or lonval.mask or hgtval.mask:
                    continue
            except:
                pass
            ncount += 1
            lats.append(latval)
            lons.append(lonval)
            hgts.append(hgtval)
            if ObsType.startswith('refr'):
                obs.append(data2[1,k])
                #print(k,rlat,rlon,height,ob)
            elif ObsType.startswith('bend'):
                for i in range(int(nreps_this_ROSEQ2[k])):
                    m = 6*(i+1)-3
                    freq = data1[m,k]
                    observation = data1[m+2,k]
                    # look for zero frequency bending angle ob
                    if int(freq) == 0: break
                obs.append(observation)
                #print(k,rlat,rlon,height,ob)
        if ncount:
            lat[nob:nob+ncount] = lats
            lon[nob:nob+ncount] = lons
            ob[nob:nob+ncount] = obs
            hgt[nob:nob+ncount] = hgts
            time[nob:nob+ncount] = timeval
            nob += ncount
    # only loop over first MaxMsgs messages
    if MaxMsgs > 0 and bufr.msg_counter == MaxMsgs: break
bufr.close()
nc.close()
