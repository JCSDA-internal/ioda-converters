#!/usr/bin/env python

import numpy as np
import datetime,os,sys
from fit_meteorwinds_comparison29 import fit_meteorwinds_comparison
from eccodes import *
from eccodes import CodesInternalError, codes_is_missing
os.environ["ECCODES_DEFINITION_PATH"]='/mnt/md0/anaconda3/share/eccodes/definitions'

def read_radar_bufr(bufr_file_path: str, bufr_tables_path: str):
    """
    Reads a BUFR file containing interferometric radar data using eccodes,
    pointing to a specific directory of BUFR tables.

    Args:
        bufr_file_path (str): The full path to your BUFR data file.
        bufr_tables_path (str): The full path to the root directory of your
                                downloaded BUFR tables (e.g., '.../BUFR4-master').
    """
    if not os.path.exists(bufr_file_path):
        print(f"Error: BUFR file not found at '{bufr_file_path}'")
        return

    if not os.path.exists(bufr_tables_path):
        print(f"Error: BUFR tables directory not found at '{bufr_tables_path}'")
        return

    # --- Step 1: Set the custom BUFR table path ---
    # This is the most critical step for reading custom BUFR formats.
    # It tells eccodes where to find your definitions.
    # We get the original path first to append our path, not overwrite it.
    original_defs_path = os.environ.get("ECCODES_DEFINITION_PATH", "")
    os.environ["ECCODES_DEFINITION_PATH"] = f"{bufr_tables_path}:{original_defs_path}"
    #print(f"Using ECCODES_DEFINITION_PATH: {os.environ['ECCODES_DEFINITION_PATH']}")

    try:
        with open(bufr_file_path, 'rb') as f:
            msg_count = 1
            # A BUFR file can contain multiple messages
            while True:
                # codes_bufr_new_from_file returns a handle to the message
                bufr_handle = codes_bufr_new_from_file(f)

                if bufr_handle is None:
                    # No more messages in the file
                    break

                #print(f"\n--- Processing Message {msg_count} ---")

                # --- Step 2: Unpack the message to access data values ---
                # This is mandatory to read the data section. Without it,
                # you can only access header-level keys.
                codes_set(bufr_handle, 'unpack', 1)

                # --- Step 3: Discover available keys (for the first message) ---
                # Since you have a new format, you might not know the keys.
                # This code block will print all available keys for the first message.
                """
                kkey=[]
                if msg_count == 1:
                    #print("\n--- Discovering all available keys from first message ---")
                    key_iterator = codes_keys_iterator_new(bufr_handle)
                    while codes_keys_iterator_next(key_iterator):
                        key_name = codes_keys_iterator_get_name(key_iterator)
                        print(f"  - {key_name}")
                        kkey.append(key_name)
                    codes_keys_iterator_delete(key_iterator)
                    #print("--- End of key discovery ---\n")
                """
                # --- Step 4: Access specific data ---
                # Use the keys you discovered above!
                try:
                    #for ii in range(len(kkey)):
                    #    if ii<=31:
                    #        tmp = codes_get(bufr_handle,kkey[ii])
                    #        print(ii,kkey[ii],tmp)
                    #    elif ii<=55:
                    #        tmp = codes_get_array(bufr_handle,kkey[ii])
                    #        print(ii,kkey[ii],len(tmp),tmp[:2])
                    #    elif (ii==56)|(ii==60):
                    #        continue
                    #    else:
                    #        tmp = codes_get_array(bufr_handle,kkey[ii])
                    #        print(ii,kkey[ii],len(tmp),tmp[:2])
                    #exit()
                    if not codes_is_missing(bufr_handle, 'year'):
                        year = codes_get(bufr_handle, 'typicalYear')
                        month = codes_get(bufr_handle, 'typicalMonth')
                        day = codes_get(bufr_handle, 'typicalDay')
                        hour = codes_get(bufr_handle, 'typicalHour')
                    else:
                        print("YEAR is marked as MISSING in this message.")
                    
                    # For single values, check if they are missing before getting them
                    if not codes_is_missing(bufr_handle, 'dopplerMeanVelocityRadial'):
                        minute = codes_get_array(bufr_handle, 'minute')
                        second = codes_get_array(bufr_handle, 'second')
                        velocities = codes_get_array(bufr_handle, 'dopplerMeanVelocityRadial')
                        azimuth = codes_get_array(bufr_handle, 'bearingOrAzimuth')
                        zenith = codes_get_array(bufr_handle, 'viewingZenithAngle')
                        snrdb = codes_get_array(bufr_handle, 'signalToNoiseRatio')
                        tau = codes_get_array(bufr_handle, 'decayTime')
                        amp = codes_get_array(bufr_handle, 'amplitudeOfEcho')
                        distance = codes_get_array(bufr_handle, 'distance')
                        ambig= codes_get_array(bufr_handle, 'numberOfVectorAmbiguities')
                        delph = codes_get_array(bufr_handle, 'differentialPhase')
                        #distance = codes_get(bufr_handle, 'distance')
                    else:
                        print("Distance is marked as MISSING in this message.")

                    if not codes_is_missing(bufr_handle, 'height'):
                        height = codes_get_array(bufr_handle, 'height')
                        distance = codes_get_array(bufr_handle, 'distance')
                    else:
                        print("height array is marked as MISSING.")
               
                    dt=[] 
                    for jj in range(len(tau)):
                        dt.append(datetime.datetime(year,month,day,hour,minute[jj],0)+datetime.timedelta(seconds=second[jj]))
                    """
                    for j in range(14):
                        print('%s %6.1f%5.1f%7.2f%6.2f%6.1f%7.1f%2i%5.1f%5i%6.3f%5.1f' \
                            %(dt[j],distance[j]/1000.,height[j]/1000.,velocities[j],velocities[j+len(height)],zenith[j], \
                            90.-azimuth[j] if azimuth[j]<90. else 450.-azimuth[j],ambig[j]+1,delph[j], \
                            amp[j],tau[j],snrdb[j]))
                    """
                except CodesInternalError as e:
                    # This block will now only catch unexpected errors, not "Key/value not found"
                    print(f"An unexpected Eccodes error occurred: {e}", file=sys.stderr)

                # --- Step 5: Release the handle for the current message ---
                codes_release(bufr_handle)
                msg_count += 1
    
    except Exception as e:
        print(f"An error occurred: {e}", file=sys.stderr)

    finally:
        # It's good practice to reset the environment variable if the script continues
        if original_defs_path:
            os.environ["ECCODES_DEFINITION_PATH"] = original_defs_path
        else:
            del os.environ["ECCODES_DEFINITION_PATH"]
        #print("\nScript finished.")
        return [dt,distance,height,velocities,zenith,azimuth,ambig,delph,amp,tau,snrdb]

def filtering(data,ThetaMax):
    # Data entries
    #     0     1  2    3    4    5     6     7       8     9   10   11 
    # DateTime,Rge,Ht,Vrad,delVr,Theta,Phi0,Ambig,Delphase,amax,Tau,snrdb
    total = len(data)
    valid1, valid2, vrad_med = 0,0,0.
    dat,dat2 = [],[]
    for a in data:
        ####    test test test
        #if (a[7]==2)&(dat!=[]):
        #    try:
        #        d=dat[-1]
        #        if d[0]==a[0]:
        #            #print(dat[-1])
        #            dat.pop()
        #            valid1-=1
        #            vrad_med-=d[3]
        #            #print(dat[-1])
        #    except IndexError:
        #        print(a[0],'<--')
        #        exit()
        ####
        if (a[7]!=1)|(a[1]>400.)|(a[5]>ThetaMax)|(a[11]<5.)|(np.abs(a[3])>110.): continue
        #dd = (np.cos(np.radians(a[7]))*np.sin(np.radians(a[6])))**2.+ \
        #    (np.sin(np.radians(a[7]))*np.cos(np.radians(a[6])))**2.
        #if dd>1.: continue
        dat.append(a)
        valid1+=1
        vrad_med+=a[3]
  
    if valid1==0: return dat2
    vrad_med=vrad_med/float(valid1)
    vrad_thres=110.0
    if np.abs(vrad_med)<=5.:
        vrad_thres=110.0
    elif (np.abs(vrad_med)>5.)&(np.abs(vrad_med)<10.):
        vrad_thres=90.0
    else:
        vrad_thres=80.0

    #print(total,len(dat))

    jump = 0
    #print(dat[0][7],dat[1][7],dat[2][7],dat[3][7],dat[4][7],dat[5][7],dat[6][7])
    ###  Removing multple counts from a (potential) single meteor echo ###
    for j,a in enumerate(dat):
        if j==valid1-1:
            dat2.append(dat[j])
        else:
            #if jump<=j: jump=j
            if jump==j:
                i = 1
                while (np.abs((dat[j][0]-dat[j+i][0]).total_seconds())<=0.008)&  \
                      (np.abs(dat[j][6]-dat[j+i][6])<=2.)& \
                      (np.abs(dat[j][5]-dat[j+i][5])<=2.):
                    i+=1
                    if (i+j)>=len(dat): break
                jmin,jmax=j,j+i
                d=jmin
                ivalue=j
                if i>0:
                    for kk in range(jmin,jmax):
                        if dat[d][9]<dat[kk][9]:
                            d=kk
                            ivalue=kk
                jump=jmax
                #if j<=11: print(j,jmin,jmax)
                valid2+=1
                dat2.append(dat[ivalue])
   
    print('Out of ',total,' there are ',len(dat),' valid echoes, and pure ones ',len(dat2))
    return dat2

dt0=datetime.datetime(2026,2,2,0)
dt1=datetime.datetime(2026,2,3,0)

dt=dt0
bmin = 69 #km
bmax = 109 #km
binsize = 2 #km
ThetaMin = 0. #minimum zenith angle to consider
ThetaMax = 65. #maximum zenith angle to consider
# remove times during which wind determination might not be trusted by using fit_meteorwinds_comparison.
# In that code, no wind is determined when too many meteors are determined outside the 70 to 100 km range.
begin_day = 0.
#sta = 'esrange'
sta = 'riogrande'
#sta = 'trondheim'
#sta = 'culebra'
#sta = 'sodankyla'
#sta = 'halley'
#sta = 'leipzig'
#sta = 'brazil'
#sta = 'rothera'
#sta = 'cariri'
#sta = 'pokerflat'
#sta = 'bearlake'
#sta = 'trelew'
#sta = 'santamaria'
bufr_tables = "/mnt/md0/Meteor/metechodefs-nobm"

while dt<=dt1:
    """
    if sta=='rothera':
        data1 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+(dt-datetime.timedelta(days=1)).strftime("%Y%m%d")+ \
                '.'+sta+'-sk.mpd',verbose=0)
        data2 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+dt.strftime("%Y%m%d")+ \
                '.'+sta+'-sk.mpd',verbose=0)
        data3 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+(dt+datetime.timedelta(days=1)).strftime("%Y%m%d")+ \
                '.'+sta+'-sk.mpd',verbose=0)
    elif sta=='leipzig':
        data1 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+(dt-datetime.timedelta(days=1)).strftime("%Y%m%d")+ \
                '.'+sta+'2.mpd',verbose=0)
        data2 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+dt.strftime("%Y%m%d")+ \
                '.'+sta+'2.mpd',verbose=0)
        data3 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+(dt+datetime.timedelta(days=1)).strftime("%Y%m%d")+ \
                '.'+sta+'2.mpd',verbose=0)
    else:
        data1 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+(dt-datetime.timedelta(days=1)).strftime("%Y%m%d")+ \
                '.'+sta+'.mpd',verbose=0)
        data2 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+dt.strftime("%Y%m%d")+ \
                '.'+sta+'.mpd',verbose=0)
        data3 = read_meteorradar_mpd('../data/skiymet/'+sta+'/mp'+(dt+datetime.timedelta(days=1)).strftime("%Y%m%d")+ \
                '.'+sta+'.mpd',verbose=0)
    """
    data1 = []
    radar_file = "/mnt/md0/data/skiymet/riogrande/bufr/metecho_riogrande_"+ \
        (dt-datetime.timedelta(days=1)).strftime("%Y%m%d")+"_23.bufr"
    dat = read_radar_bufr(radar_file, bufr_tables)
    if dat!=None:
        ll=len(dat[0])
        for j in range(ll):
            data1.append([dat[0][j],dat[1][j]/1000.,dat[2][j]/1000.,dat[3][j],dat[3][ll+j],dat[4][j], \
                90.-dat[5][j] if dat[5][j]<90. else 450.-dat[5][j],dat[6][j],dat[7][j],dat[8][j],dat[9][j],dat[10][j]])

    data2 = []
    for i in range(24):
        radar_file = "/mnt/md0/data/skiymet/riogrande/bufr/metecho_riogrande_"+dt.strftime("%Y%m%d") \
            +"_"+str(i).zfill(2)+".bufr"
        dat=read_radar_bufr(radar_file, bufr_tables)
        if dat!=None:
            ll=len(dat[0])
            for j in range(ll):
                data2.append([dat[0][j],dat[1][j]/1000.,dat[2][j]/1000.,dat[3][j],dat[3][ll+j],dat[4][j], \
                    90.-dat[5][j] if dat[5][j]<90. else 450.-dat[5][j],dat[6][j],dat[7][j],dat[8][j],dat[9][j],dat[10][j]])
    """
    print('       Datetime              Rge   Ht    Vrad  delVr Theta  Phi0  Ambig Delphase amax  Tau  snrdb Ambig2')
    for k in range(24):
        print('%s %6.1f%5.1f%7.2f%6.2f%6.1f%7.1f%5i%9.1f%7i%6.3f%5.1f'%(data2[k][0],data2[k][1],data2[k][2],\
                data2[k][3],data2[k][4],data2[k][5],data2[k][6],data2[k][7],data2[k][8], \
                data2[k][9],data2[k][10],data2[k][11]))
    """
    print('Total counts: ',len(data2))

    data3 = []
    radar_file = "/mnt/md0/data/skiymet/riogrande/bufr/metecho_riogrande_"+ \
        (dt+datetime.timedelta(days=1)).strftime("%Y%m%d")+"_00.bufr"
    dat = read_radar_bufr(radar_file, bufr_tables)
    if dat!=None:
        ll=len(dat[0])
        for j in range(ll):
            data3.append([dat[0][j],dat[1][j]/1000.,dat[2][j]/1000.,dat[3][j],dat[3][ll+j],dat[4][j], \
                90.-dat[5][j] if dat[5][j]<90. else 450.-dat[5][j],dat[6][j],dat[7][j],dat[8][j],dat[9][j],dat[10][j]])
 
    dat1 = filtering(data1,ThetaMax)
    dat2 = filtering(data2,ThetaMax)
    dat3 = filtering(data3,ThetaMax)

    #u, v, w, pts = fit_meteorwinds_comparison(dat1,dat2,dat3,begin_day,bmin,bmax,binsize,dt)
    u, v, w, u_err, v_err, w_err, pts = fit_meteorwinds_comparison(dat1,dat2,dat3,begin_day,bmin,bmax,binsize,dt)
    if u is not None:
        print('Saving data for '+dt.strftime("%Y%m%d"))
        #np.save('../data/meteor/'+sta+'/'+sta[:3]+dt.strftime("%Y%m%d"),(u,v,u_err,v_err,pts))
        np.save('/data/meteor/'+sta+'/'+sta[:3]+dt.strftime("%Y%m%d"),(u,v,pts))
    dt+=datetime.timedelta(days=1)
