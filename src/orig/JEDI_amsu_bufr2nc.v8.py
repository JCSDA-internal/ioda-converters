#!/usr/bin/env python3
"""convert amsubufr file to netCDF"""
from __future__ import print_function
import ncepbufr
import numpy as np
import netCDF4
import numbers
import sys
from netCDF4 import Dataset
import re

# original routine from Scott Gregory 
# CSD: cleaned up, matched to gsi obs_diag output
# Todo: test for presence of CLATH, CLONH

if len(sys.argv) < 3:
    raise SystemExit('radbufr2nc <input prepbufr> <output netcdf PATH>')
##radbufr_filename='1bamua'
# input and output file names from command line args.
radbufr_filename = sys.argv[1]
netcdf_filePATH=sys.argv[2]

## EXAMPLE BUFR /pan2/projects/gfsenkf/whitaker/cfsr_dumps/2005022506/gdas/1bamua.gdas.2005022506
datefrombufr=re.findall(r'(\d{10})', radbufr_filename) #finds ten digit number in the string preceded by PERIOD...  ## EXAMPLE BUFR /pan2/projects/gfsenkf/whitaker/cfsr_dumps/2005022506/gdas/1bamua.gdas.2005022506
try:
        filetime=datefrombufr[len(datefrombufr)-1]
except:
        filetime=datefrombufr

##datefrombufr=re.findall(r'(\d{10})', radbufr_filename) #finds ten digit number in the string preceded by PERIOD...  ## EXAMPLE BUFR /pan2/projects/gfsenkf/whitaker/cfsr_dumps/2005022506/gdas/1bamua.gdas.2005022506
print('datefrombufr=',datefrombufr)
print('filetime=',filetime)
#########################################################################################################
#########################################################################################################
#########################################################################################################

hdstr1 ='SAID FOVN YEAR MNTH DAYS HOUR MINU SECO CLAT CLON HOLS'
hdstr2 ='SAZA SOZA BEARAZ SOLAZI'
obstr = 'TMBR'

####################################################
# read amsua radiance file.
bufr = ncepbufr.open(radbufr_filename)
####################################################
####################################################
####################################################
nhd1=len(hdstr1.split())
nhd2=len(hdstr2.split())
nhd=nhd1+nhd2
###################################################
nob=0
nob1=0

nrec_count=0

# VARIABLES FROM INPUT BUFR FILE
DUMsaid    = []
DUMfovn    = []
DUMyear   = []
DUMmnth   = []
DUMdays   = []
DUMhour   = []
DUMminu   = []
DUMseco    = []
DUMhols    = []
DUMclat     = []
DUMclon     = []
DUMsaza    = []
DUMbearaz  = []
DUMsoza    = []
DUMsolazi  = []
DUMtmbr    = []

while bufr.advance() == 0:
    nobs_message = 0
    print('dumsaid=', np.unique(DUMsaid))
    while bufr.load_subset() == 0:
        nrec_count=nrec_count+1
        hdr1 = bufr.read_subset(hdstr1).squeeze()
        hdr2 = bufr.read_subset(hdstr2).squeeze()

        said=int(hdr1[0])
        fovn=int(hdr1[1])
        year=int(hdr1[2])
        mnth=int(hdr1[3])
        days=int(hdr1[4])
        hour=int(hdr1[5])
        minu=int(hdr1[6])
        seco=int(hdr1[7])
        clat=float(hdr1[8])
        clon=float(hdr1[9])
        if clon<0:
            clon=clon+360.
        #print('clon=', clon)
        hols=float(hdr1[10]) ##height of landsurface

        saza=float(hdr2[0])
        soza=float(hdr2[1])
        bearaz=float(hdr2[2])
        solazi=float(hdr2[3])

        tmbr = bufr.read_subset(obstr,rep=True).squeeze()
        nchanl = len(tmbr)

        DUMtmbr.append(tmbr)

        DUMsaid.append(said)
        DUMfovn.append(fovn)
        DUMyear.append(year) 
        DUMmnth.append(mnth) 
        DUMdays.append(days)
        DUMhour.append(hour)
        DUMminu.append(minu)
        DUMseco.append(seco)
        DUMhols.append(hols)
        DUMclat.append(clat)
        DUMclon.append(clon)
        DUMsaza.append(saza)
        DUMbearaz.append(bearaz)
        DUMsoza.append(soza)
        DUMsolazi.append(solazi)

bufr.close()
print('nrec=',nrec_count)
#########################################################################################################
#########################################################################################################
#########################################################################################################
# NOTE: /home/Jeffrey.S.Whitaker/.local/lib/python2.7/site-packages/
#       does not have satellite_names: need to use own version
from ncepbufr import satellite_names as code2sat_name
import string

punct=string.punctuation
space=' '
hyphen='-'
digits=string.digits



uniqsats=np.unique(DUMsaid)
numsat=len(uniqsats)

for n in range(numsat):
    holsarr = []
    fovnarr = []
    yeararr = []
    mntharr = []
    daysarr = []
    hourarr = []
    minuarr = []
    secoarr = []
    clatarr = []
    clonarr = []
    latarr = []
    lonarr = []
    clatharr = []
    clonharr = []
    sazaarr = []
    bearazarr = []
    sozaarr = []
    solaziarr = []
    tmbrarr = []

    satcode=uniqsats[n]
    indcs=np.where(DUMsaid==satcode)[0]
    print('satcode,indcs=',satcode,indcs)
    satlite = code2sat_name[satcode]
    print('satlite_name=',satlite)
    satlite_lower = satlite.lower
    sat_lower = "".join(c for c in satlite_lower())
    satlite_nopunct = "".join(c for c in satlite_lower() if c not in punct)
    satlite_spacepunct = "".join(c for c in satlite if c not in punct)
    satlite_nopunctnospace = "".join(c for c in satlite_nopunct if c not in space)
    print('lower,nopunc,spacepunc,nopuncnospace=',sat_lower,satlite_nopunct,satlite_spacepunct,satlite_nopunctnospace)

    if len(satlite_lower())>=6 and satlite_lower()[0:6]=='metop-':
        diag_satname=satlite_lower()[0:6]
        if satlite_lower()[6]=='2':
            diag_satname=diag_satname + 'a'
        if satlite_lower()[6]=='1':
            diag_satname=diag_satname + 'b'
        if satlite_lower()[6]=='3':
            diag_satname=diag_satname + 'c'

    if len(satlite_nopunctnospace)>=4 and satlite_nopunctnospace[0:4]=='noaa' :
        diag_satname='n'
        diag_satname=diag_satname + satlite_nopunctnospace[4:]

    if len(satlite_nopunctnospace)==4 and satlite_nopunctnospace[0:4]=='aqua' :
        diag_satname='aqua'
    
    print('diag_satname=', diag_satname)

    numindcs=len(indcs)
    print('nrec_sat=',numindcs)


    fovnarr=[DUMfovn[i] for i in indcs] 
    yeararr=[DUMyear[i] for i in indcs] 
    mntharr=[DUMmnth[i] for i in indcs] 
    daysarr=[DUMdays[i] for i in indcs] 
    hourarr=[DUMhour[i] for i in indcs] 
    minuarr=[DUMminu[i] for i in indcs] 
    secoarr=[DUMseco[i] for i in indcs] 
    clatarr=[DUMclat[i] for i in indcs]
    clonarr=[DUMclon[i] for i in indcs]
    sazaarr=[DUMsaza[i] for i in indcs]
    bearazarr=[DUMbearaz[i] for i in indcs]
    sozaarr=[DUMsoza[i] for i in indcs]
    solaziarr=[DUMsolazi[i] for i in indcs]
    holsarr=[DUMhols[i] for i in indcs]
    tmbrarr=[DUMtmbr[i] for i in indcs]
    
    diagtime=filetime[0:8]+'_'+filetime[8:10]
    netcdf_filename = netcdf_filePATH+'diag_amsua_'+diag_satname+'_anl.'+diagtime+'z.nc4'
    print('netcdffile=',netcdf_filename)
    nc =netCDF4.Dataset(netcdf_filename,'w',format='NETCDF4')
    nchans_dim = nc.createDimension('nchans',None)
    nobs_bufr_dim = nc.createDimension('nobs_bufr',None)
    nobs_diag_dim = nc.createDimension('nobs',None) 
    Observation_Class_maxstrlen_dim = nc.createDimension('Observation_Class_maxstrlen',7)
    BC_angord_arr_dim = nc.createDimension('BC_angord_arr_dim',5)
    
    chaninfoidx_nc =        nc.createVariable('chaninfoidx','i','nchans') ;
    
    freq_nc =               nc.createVariable('frequency',np.double,'nchans') ;
    
    polariz_nc =            nc.createVariable('polarization','i','nchans') ;
    
    wavenum_nc =            nc.createVariable('wavenumber',np.double,'nchans') ;
    errvar_nc =             nc.createVariable('error_variance',np.double,'nchans') ;
    lapseavg_nc =           nc.createVariable('mean_lapse_rate',np.double,'nchans') ;
    
    useflag_nc =            nc.createVariable('use_flag','i','nchans') ;
    sensorchan_nc =         nc.createVariable('sensor_chan','i','nchans') ;
    satinfochan_nc =        nc.createVariable('satinfo_chan','i','nchans') ;
    chanindx_nc =           nc.createVariable('Channel_Index','i','nobs') ;
    obclass_nc  =           nc.createVariable('Observation_Class','S1', ('nobs','Observation_Class_maxstrlen')) ;    ###### will likelyneed netCDF4.stringtochar

# VARIABLES FROM THE INPUT BUFR FILE (dim = nobs_bufr)
    fovn_bufr_nc =               nc.createVariable('FOVN',   np.double,'nobs_bufr') ;    
    year_bufr_nc =               nc.createVariable('YEAR',   'i','nobs_bufr') ; 
    mnth_bufr_nc =               nc.createVariable('MNTH',   'i','nobs_bufr') ; 
    days_bufr_nc =               nc.createVariable('DAYS',   'i','nobs_bufr') ; 
    hour_bufr_nc =               nc.createVariable('HOUR',   'i','nobs_bufr') ; 
    minu_bufr_nc =               nc.createVariable('MINU',   'i','nobs_bufr') ; 
    seco_bufr_nc =               nc.createVariable('SECO',   'i','nobs_bufr') ; 
    clat_bufr_nc =               nc.createVariable('CLAT',   np.double,'nobs_bufr') ;
    clon_bufr_nc =               nc.createVariable('CLON',   np.double,'nobs_bufr') ;
    #clath_bufr_nc =              nc.createVariable('CLATH',   np.double,'nobs_bufr') ;
    #clonh_bufr_nc =              nc.createVariable('CLONH',   np.double,'nobs_bufr') ;
    hols_bufr_nc =               nc.createVariable('HOLS',   np.double,'nobs_bufr') ;

    satzen_bufr_nc =             nc.createVariable('SAZA',   np.double,'nobs_bufr') ;
    solzen_bufr_nc  =            nc.createVariable('SOZA',   np.double,'nobs_bufr') ;
    bearaz_bufr_nc   =           nc.createVariable('BEARAZ',   np.double,'nobs_bufr') ;
    solaz_bufr_nc   =            nc.createVariable('SOLAZI',   np.double,'nobs_bufr') ;

    tmbr_bufr_nc=                nc.createVariable('TMBR',   np.double,('nobs_bufr','nchans')) ;

# VARIABLES FROM THE OBS DIAG FILE (dim = nobs)

 
    lat_diag_nc =                nc.createVariable('Latitude',   np.double,'nobs') ;
    lon_diag_nc =                nc.createVariable('Longitude',   np.double,'nobs') ;
    elev_diag_nc =               nc.createVariable('Elevation',   np.double,'nobs') ;
    time_diag_nc =               nc.createVariable('Obs_Time',  np.double,'nobs') ; 
    scanpos_diag_nc =            nc.createVariable('Scan_Position',   np.double,'nobs') ;
    satzen_diag_nc  =             nc.createVariable('Sat_Zenith_Angle',   np.double,'nobs') ;
    satazi_diag_nc  =             nc.createVariable('Sat_Azimuth_Angle',   np.double,'nobs') ;
    solzen_diag_nc  =             nc.createVariable('Sol_Zenith_Angle',   np.double,'nobs') ;
    solazi_diag_nc  =             nc.createVariable('Sol_Azimuth_Angle',   np.double,'nobs') ;
    glintang_diag_nc =           nc.createVariable('Sun_Glint_Angle',   np.double,'nobs') ;
    waterfrac_diag_nc=           nc.createVariable('Water_Fraction',   np.double,'nobs') ;
    landfrac_diag_nc =           nc.createVariable('Land_Fraction',   np.double,'nobs') ;
    icefrac_diag_nc =            nc.createVariable('Ice_Fraction',   np.double,'nobs') ;
    snowfrac_diag_nc =           nc.createVariable('Snow_Fraction',   np.double,'nobs') ;
    waterT_diag_nc =             nc.createVariable('Water_Temperature',   np.double,'nobs') ;
    landT_diag_nc =              nc.createVariable('Land_Temperature',   np.double,'nobs') ;
    iceT_diag_nc =              nc.createVariable('Ice_Temperature',   np.double,'nobs') ;
    snowT_diag_nc =              nc.createVariable('Snow_Temperature',   np.double,'nobs') ;
    soilT_diag_nc =              nc.createVariable('Soil_Temperature',   np.double,'nobs') ;
    soilM_diag_nc =              nc.createVariable('Soil_Moisture',   np.double,'nobs') ;
    #### INT ####         
    landTYPE_diag_nc =           nc.createVariable('Land_Type_Index','i','nobs') ;
    #### INT ####
    tsavg5_diag_nc=              nc.createVariable('tsavg5',   np.double,'nobs') ;
    sstcu_diag_nc=               nc.createVariable('sstcu',   np.double,'nobs') ;
    sstph_diag_nc=               nc.createVariable('sstph',   np.double,'nobs') ;
    sstnv_diag_nc=               nc.createVariable('sstnv',   np.double,'nobs') ;
    dta_diag_nc=                 nc.createVariable('dta',   np.double,'nobs') ;
    dqa_diag_nc=                 nc.createVariable('dqa',   np.double,'nobs') ;
    dtp_avh_diag_nc=             nc.createVariable('dtp_avh',   np.double,'nobs') ;
    vegfrac_diag_nc=             nc.createVariable('Vegetation_Fraction',   np.double,'nobs') ;
    snodep_diag_nc=              nc.createVariable('Snow_Depth',   np.double,'nobs') ;
    tpwc_diag_nc=                nc.createVariable('tpwc_amsua',   np.double,'nobs') ;
    clwges_diag_nc=              nc.createVariable('clw_guess_retrieval',   np.double,'nobs') ;
    sfcwind_diag_nc=             nc.createVariable('Sfc_Wind_Speed',   np.double,'nobs') ;
    cloudfrac_diag_nc=           nc.createVariable('Cloud_Frac',   np.double,'nobs') ;
    CTP_diag_nc=                 nc.createVariable('CTP',   np.double,'nobs') ;
    CLW_diag_nc=                 nc.createVariable('CLW',   np.double,'nobs') ;
    TPWC_diag_nc=                nc.createVariable('TPWC',   np.double,'nobs') ;
    clwobs_diag_nc=              nc.createVariable('clw_obs',   np.double,'nobs') ;
    clwges_diag_nc=              nc.createVariable('clw_guess',   np.double,'nobs') ;
    foundT_diag_nc=              nc.createVariable('Foundation_Temperature',   np.double,'nobs') ;
    sst_warmdt_diag_nc=          nc.createVariable('SST_Warm_layer_dt',   np.double,'nobs') ;
    sst_cooldt_diag_nc=          nc.createVariable('SST_Cool_layer_tdrop',   np.double,'nobs') ;
    sst_dtzdtf_diag_nc=          nc.createVariable('SST_dTz_dTfound',   np.double,'nobs') ;
    obs_diag_nc=                 nc.createVariable('Observation',   np.double,'nobs') ;
    oMf_adj_diag_nc=             nc.createVariable('Obs_Minus_Forecast_adjusted',   np.double,'nobs') ;
    oMf_unadj_diag_nc=           nc.createVariable('Obs_Minus_Forecast_unadjusted',   np.double,'nobs') ;
    invOBSerr_diag_nc=           nc.createVariable('Inverse_Observation_Error',   np.double,'nobs') ;
    qcflag_diag_nc=              nc.createVariable('QC_Flag',   np.double,'nobs') ;
    emiss_diag_nc=               nc.createVariable('Emissivity',   np.double,'nobs') ;
    wtd_lapse_diag_nc=           nc.createVariable('Weighted_Lapse_Rate',   np.double,'nobs') ;
    dtb_dts_diag_nc=             nc.createVariable('dTb_dTs',   np.double,'nobs') ;
    bc_const_diag_nc=            nc.createVariable('BC_Constant',   np.double,'nobs') ;
    bc_scanangle_diag_nc=        nc.createVariable('BC_Scan_Angle',   np.double,'nobs') ;
    bc_clw_diag_nc=              nc.createVariable('BC_Cloud_Liquid_Water',   np.double,'nobs') ;
    bc_lapsesqd_diag_nc=         nc.createVariable('BC_Lapse_Rate_Squared',   np.double,'nobs') ;
    bc_lapse_diag_nc=            nc.createVariable('BC_Lapse_Rate',   np.double,'nobs') ;
    bc_cosinelatxnode_diag_nc=   nc.createVariable('BC_Cosine_Latitude_times_Node',   np.double,'nobs') ;
    bc_sine_diag_nc=             nc.createVariable('BC_Sine_Latitude',   np.double,'nobs') ;
    bc_emiss_diag_nc=            nc.createVariable('BC_Emissivity',   np.double,'nobs') ;
    bc_fixedscanpos_diag_nc=     nc.createVariable('BC_Fixed_Scan_Position',   np.double,'nobs') ;
    bc_angord_diag_nc=           nc.createVariable('BC_angord',   np.double,('nobs','BC_angord_arr_dim')) ;
    
    #// global attributes:
    ##nc.description='blahblah'
    nc.Satellite_Sensor = 'amsua_'+diag_satname ;
    nc.Satellite = diag_satname ;
    nc.Observation_type = "amsua" ;
# should not be hard coded
    nc.Outer_Loop_Iteration = -1;
    nc.Number_of_channels = -1 ;
    nc.Number_of_Predictors = -1 ;
    nc.date_time = -1 ;
    nc.ireal_radiag = -1 ;
    nc.ipchan_radiag = -1 ;
    nc.iextra = -1 ;
    nc.jextra = -1 ;
    nc.idiag = -1 ;
    nc.angord = -1 ;
    nc.iversion_radiag = -1 ;
    nc.New_pc4pred = -1 ;
    nc.ioff0 = -1  ;



    #########################################################################################################
    fovnarr = np.array(fovnarr)
    clatarr   = np.array(clatarr)
    clonarr = np.array(clonarr)
    latarr   = -9999*np.ones(clatarr.shape)
    lonarr   = -9999*np.ones(clonarr.shape)
    clatharr = -9999*np.ones(clatarr.shape)
    clonharr = -9999*np.ones(clonarr.shape)
    holsarr = np.array(holsarr)

    sazaarr  = np.array(sazaarr)
    bearazarr  = np.array(bearazarr)
    sozaarr  = np.array(sozaarr)
    solaziarr = np.array(solaziarr)
    tmbrarr = np.array(tmbrarr)
    #print('clatarray=',clatarr)

    nc['FOVN'][:] = fovnarr    
    nc['YEAR'][:] = yeararr
    nc['MNTH'][:] = mntharr
    nc['DAYS'][:] = daysarr
    nc['HOUR'][:] = hourarr
    nc['MINU'][:] = minuarr
    nc['SECO'][:] = secoarr
    nc['CLAT'][:] = clatarr
    nc['CLON'][:] = clonarr
    nc['HOLS'][:] = holsarr
    #nc['CLATH'][:] = clatharr
    #nc['CLONH'][:] = clonharr
    nc['SAZA'][:]  = sazaarr
    nc['SOZA'][:]  = sozaarr
    nc['BEARAZ'][:] = bearazarr
    nc['SOLAZI'][:] = solaziarr

    nc['TMBR'][:] = tmbrarr
    nc.sync() # dump data to disk.
    
    
    nc.close()
    del nc
    del indcs
    del fovnarr
    del latarr
    del lonarr
    del clatarr
    del clonarr
    del clatharr
    del clonharr
    del sazaarr
    del bearazarr
    del sozaarr
    del solaziarr
    del holsarr
    del tmbrarr








