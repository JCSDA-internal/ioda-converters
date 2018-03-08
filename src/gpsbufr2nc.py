from __future__ import print_function
import ncepbufr, os, sys, netCDF4
from optparse import OptionParser
import numpy as np
from datetime import datetime

# function to convert number to binary string (without 0b prefix)
get_bin = lambda x, n: format(x, 'b').zfill(n)

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

ObsType = MyArgs[0] # 'bending ang' or 'refractivity' ('bend' or 'ref' is OK)
if not ObsType.startswith('bend') and not ObsType.startswith('ref'):
    print("ERROR: obs type string must start with 'bend' or 'ref'")
    sys.exit(1)

refdate = MyArgs[1]
if len(refdate) != 10:
    print("refdate must be YYYYMMDDHH")
    sys.exit(1)
bufrFname = MyArgs[2]
netcdfFname = MyArgs[3]

hdrstr ='YEAR MNTH DAYS HOUR MINU SECO PCCF ELRC SAID PTID GEODU QFRO'

# read gpsro file.

bufr = ncepbufr.open(bufrFname)
nc = netCDF4.Dataset(netcdfFname,'w',format='NETCDF4')
nc.createDimension('nobs',None)
#nc.createDimension('nflags',16) # number of bits in quality flag table
lat = nc.createVariable('Latitude',np.float32,'nobs',zlib=True,fill_value=np.nan)
lat.units='degrees north'
lon = nc.createVariable('Longitude',np.float32,'nobs',zlib=True,fill_value=np.nan)
lat.units='degress east'
if ObsType.startswith('ref'):
    hgt = nc.createVariable('Height',np.float32,'nobs',zlib=True,fill_value=np.nan)
    hgt.units='meters'
time = nc.createVariable('Time',np.int64,'nobs',zlib=True)
bufr.advance()
YYYY = refdate[0:4]
MM = refdate[4:6]
DD = refdate[6:8]
HH = refdate[8:10]
bufr.rewind()
time.units = 'seconds since %04s-%02s-%02s %02s:00 UTC' % (YYYY,MM,DD,HH)
ob = nc.createVariable('Observation',np.float32,'nobs',zlib=True,fill_value=np.nan)
if ObsType.startswith('bend'):
    ob.long_name = 'bending angle observation at zero frequency'
else:
    ob.long_name = 'refractivity observation'
oberr = nc.createVariable('ObservationErrorBufr',np.float32,'nobs',zlib=True,fill_value=np.nan)
obpcc = nc.createVariable('ObservationPercentConfidence',np.float32,'nobs',zlib=True,fill_value=np.nan)
profpcc = nc.createVariable('ProfilePercentConfidence',np.float32,'nobs',zlib=True,fill_value=np.nan)
satidn = nc.createVariable('SatelliteID',np.int16,'nobs',zlib=True)
platidn = nc.createVariable('PlatformTransmitterID',np.int16,'nobs',zlib=True)
rcurv = nc.createVariable('EarthLocalRadiusCurv',np.float32,'nobs',zlib=True,fill_value=np.nan)
if ObsType.startswith('bend'):
    imp = nc.createVariable('ImpactParameter',np.float32,'nobs',zlib=True,fill_value=np.nan)
    imp.units = 'meters'
#qf = nc.createVariable('QualityFlags',np.int8,('nobs','nflags'),zlib=True)
qf = nc.createVariable('QualityFlags',np.int16,'nobs',zlib=True)
geo = nc.createVariable('GeoidUndulation',np.float32,'nobs',zlib=True,fill_value=np.nan)
#if ObsType.startswith('bend'):
#    ob = nc.createVariable('Incremental_Bending_Angle',np.float32,'nobs')

nob = 0
while bufr.advance() == 0:
    print(bufr.msg_counter, bufr.msg_type, bufr.msg_date)
    while bufr.load_subset() == 0:
        hdr = bufr.read_subset(hdrstr).squeeze()
        yyyymmddhhss ='%04i%02i%02i%02i%02i%02i' % tuple(hdr[0:6])
        date=datetime(int(hdr[0]),int(hdr[1]),int(hdr[2]),int(hdr[3]),int(hdr[4]),int(hdr[5]))
        timeval = netCDF4.date2num(date,units=time.units)
        pcc = hdr[6] # profile percent confidence
        roc = hdr[7] # Earth local radius of curvature
        satid = int(hdr[8]) # satellite identifier
        ptid = int(hdr[9]) # Platform transmitter ID number
        geoid = hdr[10] # geod undulation
        qfro = int(hdr[11]) # quality flag (used by read_gps to flag bad profile)
        #ibits = get_bin(qfro, 16) # convert to 16 bit binary string
        #iflags = np.zeros(16,np.int8)
        #for n,ibit in enumerate(ibits):
        #    if int(ibit): iflags[n]=1
# Get the number of occurences of sequence ROSEQ2 in this subset
# (will also be the number of replications of sequence ROSEQ1).
# Also determine the number of replications of sequence ROSEQ2 nested
# inside each replication of ROSEQ1,
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
        #print('sat id,platform transitter id, levels, yyyymmddhhmm =',\
        #satid,ptid,levs1,yyyymmddhh)
        #print('k, height, lat, lon, ref, bend:')
        lats = []; lons = []; hgts = []; obs = []
        pccs = []; rocs = []; satids = []; ptids = []
        geoids = []; obserr = []; obspccf = []; rocs = []
        qflags = []; impacts = []
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
            geoids.append(geoid)
            rocs.append(roc)
            pccs.append(pcc)
            satids.append(satid)
            ptids.append(ptid)
            #qflags.append(iflags)
            qflags.append(qfro)
            if ObsType.startswith('ref'):
                ref=data2[1,k]
                ref_error=data2[3,k]
                ref_pccf=data2[5,k]
                obs.append(ref)
                obserr.append(ref_error)
                obspccf.append(ref_pccf)
                #print(k,rlat,rlon,height,ob)
            elif ObsType.startswith('bend'):
                for i in range(int(nreps_this_ROSEQ2[k])):
                    m = 6*(i+1)-3
                    freq = data1[m,k]
                    impact = data1[m+1,k] # impact parameter
                    bend = data1[m+2,k]
                    bend_error = data1[m+4,k]
                    # look for zero frequency bending angle ob
                    # don't want non-zero frequency (read_gps in gsi)
                    if int(freq) == 0: break
                bend_pccf=data1[int(6*nreps_this_ROSEQ2[k])+3,k] # % conf
                obs.append(bend)
                obserr.append(bend_error)
                obspccf.append(bend_pccf)
                impacts.append(impact)
                #print(k,rlat,rlon,height,ob)
        if ncount:
            lat[nob:nob+ncount] = lats
            lon[nob:nob+ncount] = lons
            ob[nob:nob+ncount] = obs
            oberr[nob:nob+ncount] = obserr
            obpcc[nob:nob+ncount] = obspccf
            if ObsType.startswith('ref'):
                hgt[nob:nob+ncount] = hgts
            time[nob:nob+ncount] = timeval
            profpcc[nob:nob+ncount] = pccs
            satidn[nob:nob+ncount] = satids
            platidn[nob:nob+ncount] = ptids
            rcurv[nob:nob+ncount] = rocs
            geo[nob:nob+ncount] = geoids
            #qf[nob:nob+ncount,:] = qflags
            qf[nob:nob+ncount] = qflags
            if ObsType.startswith('bend'):
                imp[nob:nob+ncount] = impacts
            nob += ncount
    # only loop over first MaxMsgs messages
    if MaxMsgs > 0 and bufr.msg_counter == MaxMsgs: break
bufr.close()
nc.close()
