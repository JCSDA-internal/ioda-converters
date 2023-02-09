#!/usr/bin/env python3
"""convert prepbufr file to netCDF
cdraper, Nov, 2017"""

from __future__ import print_function
import ncepbufr
import numpy as np
import netCDF4
import numbers
import sys
import re
from netCDF4 import Dataset

# INPUT ARGS 

#if len(sys.argv) < 4:
#    raise SystemExit('prepbufr2nc <input prepbufr> <output netcdf> <VARIABLE>')

#prepbufr_filename = sys.argv[1]
#netcdf_filename = sys.argv[2]
#VARIABLE = sys.argv[3]

prepbufr_filename = 'prepbufr.gdas.2016030406'
netcdf_filename = 'test.nc' 
##################################
# functions 

# create variables, append handles
def append_nc_createvar(min_ind, bufr_str,dim_flag): 
    for n in range(len(bufr_str)):
        if (dim_flag==0):
           nc_varh.append(nc.createVariable(bufr_str[n],np.double,'nobs',zlib=True))    
        elif (dim_flag==1):
           nc_varh.append(nc.createVariable(bufr_str[n]+'1',np.double,'nobs',zlib=True))    
        elif (dim_flag==2): 
           nc_varh.append(nc.createVariable(bufr_str[n]+'2',np.double,('nobs','max_plev'),zlib=True))    
        elif (dim_flag==3): # bufr events 
           nc_varh.append(nc.createVariable(bufr_str[n]+'_bevn3',np.double,('nobs','max_plev','bevn_layer'),zlib=True))    
        elif (dim_flag==4): # bufr events 
           nc_varh.append(nc.createVariable(bufr_str[n]+'_bevn2',np.double,('nobs','bevn_layer'),zlib=True))    
 
    next_ind=min_ind+len(bufr_str)
    return next_ind

# read bufr, write to nc (single record)
def bufr2nc_record(min_ind, bufr_str,n_subset):
    for n in range(len(bufr_str)):
        tmp = bufr.read_subset(bufr_str[n]).data
        #if (tmp.size > 0): # if hdr is present and data is not missing
        if ((tmp.size >0) & (tmp < 10**9)):
          nc_varh[n+min_ind][n_subset] = tmp

# read bufr, write to nc (single record, 2D)
def bufr2nc_record2D(min_ind1, min_ind2, bufr_str,n_subset):
    maxnlev=0 

    for n in range(len(bufr_str)):
        tmp = bufr.read_subset(bufr_str[n]).data
        nlev=tmp.size # includes missing data 
        maxnlev=max(maxnlev,nlev)

        if (nlev>1):   
          for z in range(nlev): 
            if (tmp[0,z] < 10**9): # skip missing
              nc_varh[n+min_ind2][n_subset,z] = tmp[0,z]
        elif (nlev==1):
              nc_varh[n+min_ind1][n_subset] = tmp

    return maxnlev

def bufr2nc_record_bevn(min_ind2, min_ind3, bufr_str,n_subset):
    maxnlev=0 

    for n in range(len(bufr_str)):
        tmp = bufr.read_subset(bufr_str[n],events=True).data
        nlev=tmp.shape[1]  # includes missing data 
        maxnlev=max(maxnlev,nlev)

        if (nlev>1):   
          for z in range(nlev): 
            for e in range(20): 
              if (tmp[0,z,e] < 10**9): # skip missing
                nc_varh[n+min_ind3][n_subset,z,e] = tmp[0,z,e]
        elif (nlev==1):
            for e in range(20): 
              if (tmp[0,0,e] < 10**9): # skip missing
                nc_varh[n+min_ind2][n_subset,e] = tmp[0,0,e]

    return maxnlev


##################################
# from read_prepbufr.f90

hdstr = np.array(['SID','XOB','YOB','DHR','TYP','ELV','SAID','T29']) 
misc = np.array(['TSB','PRVSTG','SPRVSTG','ACID'])
obstr  = np.array(['POB','QOB','TOB','ZOB','UOB','VOB','PWO','MXGS','HOVI','CAT','PRSS','TDO','PMO' ])
satqcstr  = np.array(['QIFN'])
drift  = np.array(['XDR','YDR','HRDR']) 
fcststr  = np.array(['UFC','VFC','TFC']) 
qcstr  = np.array(['PQM','QQM','TQM','ZQM','WQM','NUL','PWQ','PMQ'])
oestr  = np.array(['POE','QOE','TOE','NUL','WOE','NUL','PWE']) 
aircraftstr = np.array(['POAF','IALR'])  
bevnstr = np.array(['TPC','TOB','TQM']) 

sststr = ['MSST','DBSS','SST1','SSTQM','SSTOE']
prvstr = ['PRVSTG']
sprvstr = ['SPRVSTG']
levstr  = ['POB']
cld2seqstr = ['TOCC','HBLCS']      
cldseqstr = ['VSSO','CLAM','HOCB']   
metarcldstr = ['CLAM','HOCB']      
metarwthstr = ['PRWE']           
metarvisstr = ['HOVI','TDO']       
goescldstr = ['CDTP','TOCC','GCDTT','CDTP_QM']   
maxtmintstr  = ['MXTM','MITM']
owavestr  = ['HOWV']
cldceilhstr = ['CEILING']

##################################
# INDEXES TO BE READ IN FROM EACH STRING ABOVE

# ALL
hdr_ind_out = np.arange(8) 
msc_ind_out = np.arange(4)

obs_ind_out = np.arange(13) 
sqc_ind_out = np.array([0])
drf_ind_out = np.arange(3)
fcs_ind_out = np.arange(3)
qcs_ind_out = np.array([0,1,2,3,4,6,7]) 
oes_ind_out = np.array([0,1,2,4,6]) 
bev_ind_out = np.array([0]) 
air_ind_out = np.arange(2)


##################################################################
# SET UP THE NC FILE

nc =Dataset(netcdf_filename,'w',format='NETCDF4')

# dimensions
nobs_dim = nc.createDimension('nobs',None)
nobs_diag_dim = nc.createDimension('nobs_diag',None)
plev_diag_dim = nc.createDimension('max_plev',None)
bevn_layr_dim = nc.createDimension('bevn_layer',None)
mtyp_string_dim = nc.createDimension('mtyp_string_len',10) # string not supported by nc4, write as array of char

# initialize variables handles
nc_varh=[]
nc_varh.append(nc.createVariable('idate','i','nobs'))
nc_varh.append(nc.createVariable('msg_type','S1',('nobs','mtyp_string_len')))


# doubles
next_ind=2 # index 0 is date, index 1 is msg_header
min_ind_hdr = next_ind  ; next_ind = append_nc_createvar(next_ind,hdstr[hdr_ind_out],0)
min_ind_msc = next_ind  ; next_ind = append_nc_createvar(next_ind,misc[msc_ind_out],0)
min_ind_sqc = next_ind  ; next_ind = append_nc_createvar(next_ind,satqcstr[sqc_ind_out],0)

# variables with 2 dimensions
min_ind_obs2 = next_ind  ; next_ind = append_nc_createvar(next_ind,obstr[obs_ind_out],2)
min_ind_drf2 = next_ind  ; next_ind = append_nc_createvar(next_ind,drift[drf_ind_out],2)
min_ind_fcs2 = next_ind  ; next_ind = append_nc_createvar(next_ind,fcststr[fcs_ind_out],2)
min_ind_qcs2 = next_ind  ; next_ind = append_nc_createvar(next_ind,qcstr[qcs_ind_out],2)
min_ind_oes2 = next_ind  ; next_ind = append_nc_createvar(next_ind,oestr[oes_ind_out],2)
min_ind_air2 = next_ind  ; next_ind = append_nc_createvar(next_ind,aircraftstr[air_ind_out],2)

# 1D versions
min_ind_obs1 = next_ind  ; next_ind = append_nc_createvar(next_ind,obstr[obs_ind_out],1)
min_ind_drf1 = next_ind  ; next_ind = append_nc_createvar(next_ind,drift[drf_ind_out],1)
min_ind_fcs1 = next_ind  ; next_ind = append_nc_createvar(next_ind,fcststr[fcs_ind_out],1)
min_ind_qcs1 = next_ind  ; next_ind = append_nc_createvar(next_ind,qcstr[qcs_ind_out],1)
min_ind_oes1 = next_ind  ; next_ind = append_nc_createvar(next_ind,oestr[oes_ind_out],1)
min_ind_air1 = next_ind  ; next_ind = append_nc_createvar(next_ind,aircraftstr[air_ind_out],1)

# variables with 3 dimensions
min_ind_bev3 = next_ind  ; next_ind = append_nc_createvar(next_ind,bevnstr[bev_ind_out],3)

# 2D versions
min_ind_bev2 = next_ind  ; next_ind = append_nc_createvar(next_ind,bevnstr[bev_ind_out],4)

bufr = ncepbufr.open(prepbufr_filename )

# read prepbufr data

n_subset=0
n_msg_excl=0 


nlev_msg=[]
type_msg=[]

#while (bufr.advance() == 0) & (n_subset<1000): # loop over messages.
while (bufr.advance() == 0):
    # msg type
    tmp_str = bufr.msg_type
    if ( (tmp_str == 'AIRCFT') | (tmp_str == 'AIRCAR') | (tmp_str == 'SATWND') ): 
        n_msg_excl+=1
        continue
 
    while bufr.load_subset() == 0: # loop over subsets in message.

        # date
        nc_varh[0][n_subset] = bufr.msg_date        
     
        # msg type
        #tmp_str = bufr.msg_type
        if (len(tmp_str) >  10 ):
            print ('msg_type len too long)') 
        else: 
            tmp_str=tmp_str.ljust(10,' ')
 
        #for s in range(len(tmp_str)):
        #  nc_varh[1][n_subset,s] = tmp_str[s] 

        #tmp = bufr.read_subset('ZOB').data
        #nlev=tmp.size # includes missing data 
   
        #for z in range(nlev): 
        #    if (tmp[0,z] < 10**9): # skip missing
        #      nc_varh[min_ind_obs][n_subset,z] = tmp[0,z]

        bufr2nc_record(min_ind_hdr,hdstr[hdr_ind_out],n_subset)
        bufr2nc_record(min_ind_msc, misc[msc_ind_out],n_subset)
        bufr2nc_record(min_ind_sqc, satqcstr[sqc_ind_out],n_subset)
        nlev = bufr2nc_record2D(min_ind_obs1, min_ind_obs2, obstr[obs_ind_out],n_subset)
        nlev_msg.append(nlev) 
        type_msg.append(tmp_str) 

        nlev = bufr2nc_record2D(min_ind_drf1, min_ind_drf2, drift[drf_ind_out],n_subset)
        nlev = bufr2nc_record2D(min_ind_fcs1, min_ind_fcs2, fcststr[fcs_ind_out],n_subset)
        nlev = bufr2nc_record2D(min_ind_qcs1, min_ind_qcs2, qcstr[qcs_ind_out],n_subset)
        nlev = bufr2nc_record2D(min_ind_oes1, min_ind_oes2, oestr[oes_ind_out],n_subset)
        nlev = bufr2nc_record2D(min_ind_air1, min_ind_air2, aircraftstr[air_ind_out],n_subset)
        nlev = bufr2nc_record_bevn(min_ind_bev2, min_ind_bev3, bevnstr[bev_ind_out],n_subset)
            
        n_subset+=1
        print(n_subset) 

nc.virtmp_code = bufr.get_program_code('VIRTMP') 


print(n_subset)         
nc.sync()
nc.close()

bufr.close()


