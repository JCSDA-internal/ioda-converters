#!/usr/bin/env python
 
import numpy as np
import array,datetime,copy
from svdfit_wind2 import svdfit_wind

def fit_meteorwinds_comparison(data1, data2, data3, begin_day, bmin, bmax, binsize,ddt):

    # Data entries
    #     0     1    2  3    4    5     6    7      8     9       10      11   12  13  14    15 
    # DateTime,File,Rge,Ht,Vrad,delVr,Theta,Phi0,Ambig,Delphase,ant_pair,IREX,amax,Tau,vmet,snrdb
    # Data entries
    #     0     1  2    3    4    5     6     7       8     9   10   11
    # DateTime,Rge,Ht,Vrad,delVr,Theta,Phi0,Ambig,Delphase,amax,Tau,snrdb# Data entries

    tdmin = 0.015 #minimum trail decay time we want to consider in the calculation
    tdmax = 0.5   #maximum trail decay time we want to consider in the calculation

    ntimes  = 24 # n_elements(htime.hist) One hour time cadence 
    nheights = int((bmax - bmin)/binsize+1)

    allzon = np.full([ntimes, nheights], 9999.9, dtype=float)
    allmer = np.full([ntimes, nheights], 9999.9, dtype=float)
    allver = np.full([ntimes, nheights], 9999.9, dtype=float)
    zonerr = np.full([ntimes, nheights], 9999.9, dtype=float)
    mererr = np.full([ntimes, nheights], 9999.9, dtype=float)
    vererr = np.full([ntimes, nheights], 9999.9, dtype=float)
    pts = np.full([ntimes, nheights], np.nan)

    newd = []
    yr,mo,da=ddt.year,ddt.month,ddt.day
    for line in data1:
        #print(len(line))
        if line[0].hour!=23: continue
        if line[0].minute<30: continue
        if (line[4]>15.)|(line[4]<=0.): continue
        newd.append(line)
    for line in data2:
        if (line[4]>15.)|(line[4]<=0.): continue
        newd.append(line)
    for line in data3:
        if line[0].hour!=0: continue
        if line[0].minute>30: continue
        if (line[4]>15.)|(line[4]<=0.): continue
        newd.append(line)
    print(' Valid data entries: ',len(newd))
    if len(newd)==0:
        return None, None, None, None, None, None, None
    data=[]
    #u_reg,v_reg,w_reg=allzon.copy(),allmer.copy(),allver.copy()
    for nn in range(8):
        u_reg = copy.deepcopy(allzon)
        v_reg = copy.deepcopy(allmer)
        w_reg = copy.deepcopy(allver)
        if nn==0:
            reg=[9999.9,9999.9,9999.9]
        else:
            reg = [[[9999.9, 9999.9, 9999.9] for alt in range(21)] for j in range(24)]
            for alt in range(21):
                for j in range(24):
                    sum_u,sum_v,sum_w,itt=0,0,0,0
                    neighbors = [(j,alt-1),(j,alt+1),(j-1,alt),(j+1,alt)]
                    for t,a in neighbors:
                        if -1<a<21 and -1<t<24:
                            if abs(u_reg[t,a]) <= 500.0:
                                sum_u = sum_u + u_reg[t][a]
                                sum_v = sum_v + v_reg[t][a]
                                sum_w = sum_w + w_reg[t][a]
                                itt += 1
                    if itt>0:
                        reg[j][alt][0] = sum_u/itt
                        reg[j][alt][1] = sum_v/itt
                        reg[j][alt][2] = sum_w/itt

        #print('***',allzon[1,14],allzon[1,16])
        #if nn>2: continue 
        for i in range(ntimes):
            #thishour = [x for x in newd if x[0].hour==i ]
            thishour = [x for x in newd if (x[0]>=(datetime.datetime(yr,mo,da,i)-datetime.timedelta(minutes=30))) & \
                (x[0]<=(datetime.datetime(yr,mo,da,i)+datetime.timedelta(minutes=30))) ]

            count_high, count_low, count_good, count_ratio = np.nan, np.nan, np.nan, np.nan
            index_high = len([x for x in thishour if x[2] > 110.])
            index_low = len([x for x in thishour if x[2] < 60.])
            index_good = len(thishour) - index_high - index_low
            #print(index_high,index_low,index_good,len(thishour))
            if index_good==0.: continue

            for j in range(nheights):
                cell = [x for x in thishour if (x[2]>=bmin+j*binsize)&(x[2]<=bmin+(j+1)*binsize)]
                if len(cell)<=3: continue

                cell = np.array(cell)
                phi1 = np.radians(cell[:,6].astype(np.float64))
                theta1 = np.radians(cell[:,5].astype(np.float64))
                ht1 = cell[:,2].astype(np.float64)
                sigalt = cell[:,4].astype(np.float64)
                tshift = (cell[:,0]-datetime.datetime(yr,mo,da,i)).astype('timedelta64[s]').astype('float64')/3600./24.

                vrad1 = cell[:,3].astype(np.float64)
                alt_tw = np.mean(cell[:,2].astype(np.float64))
                phi_err, theta_err = 2., 2.

                if len(reg)==3:
                    wind0=reg.copy()
                else:
                    wind0=reg[i][j].copy()

                coef3= np.array([1.,1.,0.])
                ia=np.array([1,1,0])

                wind_err=np.array([10.,10.,10.],dtype=float)
                #sig_wind=np.abs(np.cos(phi1)*np.sin(theta1)).reshape(-1,1)*wind_err.reshape(1,-1)
                # Statistical error due to wind fitting
                sig_wind=np.array((np.abs(np.cos(phi1)*np.sin(theta1)), \
                               np.abs(np.sin(phi1)*np.sin(theta1)), \
                               np.abs(np.cos(theta1)))).transpose()*wind_err
                # Statistical error due to interferometric errors
                sig_phi=np.abs(-allzon[i,j]*np.sin(phi1)*np.sin(theta1)+allmer[i,j]*np.cos(phi1)*np.sin(theta1))*np.deg2rad(phi_err)
                sig_theta=np.abs(allzon[i,j]*np.cos(phi1)*np.cos(theta1)+allmer[i,j]*np.sin(phi1)*np.cos(theta1)- \
                        allver[i,j]*np.sin(theta1))*np.deg2rad(theta_err)

                # Weighting kernel by local shear
                if 0<j<(nheights-1):
                    if np.abs(allzon[i,j-1]<900.)&(np.abs(allzon[i,j+1]<900.)):
                        vshear=np.sqrt((allzon[i,j+1]-allzon[i,j-1])**2.+ \
                            (allmer[i,j+1]-allmer[i,j-1])**2.)/2.
                    else:
                        vshear=5.
                else:
                    vshear=5.
                #print('----------------',nn,i,j,allzon[i,j-1],allzon[i,j+1],vshear)

                if (0<i<23)&(allzon[j-1,j]<900.):
                    if allzon[j+1,j]<900.:
                        tshear = np.sqrt((allzon[i+1,j]-allzon[i-1,j])**2.+ \
                            (allmer[i+1,j]-allmer[i-1,j])**2.)/2.
                    else:
                        tshear = 5.
                else:
                    tshear = 5.

                sigh=vshear-vshear*np.exp(-(ht1-bmin-j*binsize-binsize/2.)**2)
                #print((cell[:,0]-datetime.datetime(yr,mo,da,i)).astype('timedelta64[s]').astype(float))
                #sigt=tshear-tshear*np.exp(-(tshift/(1./24./2.0))**2) # Need some work
                sigt=tshear+tshift*0.
                if (allzon[i,j]>900.)|(allmer[i,j]>900.):
                    sig2=sigalt*10.0+np.abs(vrad1/np.cos(theta1+np.deg2rad(2.))-vrad1/np.cos(theta1-np.deg2rad(2.)))
                else:
                    sig2=sigalt+sigh+sigt+sig_wind[:,0]+sig_wind[:,1]+sig_wind[:,2]+sig_phi+sig_theta

                #if (i==4)&(j==8): print(allzon[0,3],allzon[0,4],allzon[0,5],wind0)
                #if (nn==0)&(i==1)&(j==9): print(wind0,sig2[:3],len(sig2),'?')
                #if (i==1)&(j==9): print(':',sig_wind[:,2])
                #if (i==1)&(j==15): print('!!!',vshear,allzon[i,j-1],allzon[i,j+1])
                wind, chisq, covar = svdfit_wind(phi1, theta1, vrad1, sig2, len(phi1), 3, 3, wind0)
                #if (i==1)&(j==9): print('----->',nn,len(phi1),wind,allzon[i,j],wind0[0],'-- 0')
                allzon[i,j]=wind[0]
                allmer[i,j]=wind[1]
                allver[i,j]=wind[2]
                zonerr[i,j]=np.sqrt(np.abs(covar[0,0]))
                mererr[i,j]=np.sqrt(np.abs(covar[1,1]))
                vererr[i,j]=np.sqrt(np.abs(covar[2,2]))
                #if (i==1)&(j==15): print('---->',nn,len(phi1),wind,zonerr[i,j],mererr[i,j],vererr[i,j],'-- 0')
                #print('----->',nn,i,j,len(phi1),wind,allzon[i,j],wind[0],'-- 0')
                #if nn==2: exit()

                #print('-> ',i,j,len(phi1),wind,reg)
                pts[i,j] = len(vrad1)
                if np.isfinite(wind[0]):
                    totu = np.sqrt(wind[0]**2+wind[1]**2+wind[2]**2)
                    phiquer = np.rad2deg(np.arctan2(wind[1],wind[0]))
                    #if (i==4)&(j==8): print(phiquer,'<----')
                    # Remove echoes coming from near right angle of calculated mean wind and redo the mean
                    test = np.where((np.abs(phiquer-np.rad2deg(phi1))>85)&(np.abs(phiquer-np.rad2deg(phi1))<95),False,True) \
                        &np.where((np.abs(phiquer-np.rad2deg(phi1))>265)&(np.abs(phiquer-np.rad2deg(phi1))<275),False,True) \
                        &np.where(np.abs(totu*np.cos(np.deg2rad(phiquer))-vrad1/np.sin(theta1))>15000.0,False,True)
                    phi2=phi1[test]
                    if len(phi2)<=3:
                        allzon[i,j]=9999.9
                        allmer[i,j]=9999.9
                        allver[i,j]=9999.9
                        continue
                    while len(phi2)<len(phi1):
                        phi1=phi2
                        vrad1=vrad1[test]
                        theta1=theta1[test]
                        ht1=ht1[test]
                        sigalt=sigalt[test]
                        tshift=tshift[test]
                        #coef2=wind

                        sig_wind=np.array((np.abs(np.cos(phi1)*np.sin(theta1)), \
                               np.abs(np.sin(phi1)*np.sin(theta1)), \
                               np.abs(np.cos(theta1)))).transpose()*wind_err
                        sig_phi=np.abs(-wind[0]*np.sin(phi1)*np.sin(theta1)+wind[1]*np.cos(phi1)*np.sin(theta1))*np.deg2rad(phi_err)
                        sig_theta=np.abs(wind[0]*np.cos(phi1)*np.cos(theta1)+wind[1]*np.sin(phi1)*np.cos(theta1)- \
                            wind[2]*np.sin(theta1))*np.deg2rad(theta_err)
                        #if 0<j<(nheights-1):
                        #    if np.abs(allzon[i,j-1]<900.)&(np.abs(allzon[i,j+1]<900.)):
                        #        vshear=np.sqrt((allzon[i,j+1]-allzon[i,j-1])**2.+ \
                        #            (allmer[i,j+1]-allmer[i,j-1])**2.)/2.
                        #    else:
                        #        vshear=5.
                        #else:
                        #    vshear=5.
                        sigh=vshear-vshear*np.exp(-(ht1-bmin-j*binsize-binsize/2.)**2)
                        #sigt=tshear-tshear*np.exp(-(tshift/(1./24./2.0))**2)
                        sigt=tshear+tshift*0.
                        if (wind[0]>900.)|(wind[1]>900.):
                            sig2=sigalt*10.0+np.abs(vrad1/np.cos(theta1+np.deg2rad(2.))-vrad1/np.cos(theta1-np.deg2rad(2.)))
                        else:
                            sig2=sigalt+sigh+sigt+sig_wind[:,0]+sig_wind[:,1]+sig_wind[:,2]+sig_phi+sig_theta
                        #wind01=copy.deepcopy(reg0)
                        wind, chisq, covar = svdfit_wind(phi1, theta1, vrad1, sig2, len(phi1), 3, 3, wind0)
                        #if (i==1)&(j==9): print('---->',nn,len(phi1),wind,allzon[i,j],'-- 1')
                        try:
                            allzon[i,j]=wind[0]
                            allmer[i,j]=wind[1]
                            allver[i,j]=wind[2]
                            zonerr[i,j]=np.sqrt(np.abs(covar[0,0]))
                            mererr[i,j]=np.sqrt(np.abs(covar[1,1]))
                            vererr[i,j]=np.sqrt(np.abs(covar[2,2]))
                        except TypeError:
                            continue

                        #if (i==1)&(j==15):
                        #    print('---->',nn,len(phi1),wind,zonerr[i,j],mererr[i,j],vererr[i,j],'-- 1')

                        totu = np.sqrt(wind[0]**2+wind[1]**2+wind[2]**2)
                        phiquer = np.rad2deg(np.arctan2(wind[1],wind[0]))

                        test = np.where((np.abs(phiquer-np.rad2deg(phi1))>85)&(np.abs(phiquer-np.rad2deg(phi1))<95),False,True) \
                          &np.where((np.abs(phiquer-np.rad2deg(phi1))>265)&(np.abs(phiquer-np.rad2deg(phi1))<275),False,True) \
                          &np.where(np.abs(totu*np.cos(np.deg2rad(phiquer))-vrad1/np.sin(theta1))>15000.0,False,True)
                        phi2=phi1[test]
                        pts[i,j] = len(phi2)

                #allzon[i,j],allmer[i,j],allver[i,j]=u_reg[i,j].copy(),v_reg[i,j].copy(),w_reg[i,j].copy()

    # Remove all winds larger than 200 m/s, because these are considered unphysical?
    allzon[np.where(np.abs(allzon)>250.)]=np.nan
    allmer[np.where(np.abs(allmer)>250.)]=np.nan
    allver[np.where(np.abs(allver)>100.)]=np.nan 
    #exit()
    #print('---->',allzon[1,15],zonerr[1,15],np.shape(zonerr))
    return allzon, allmer, allver, zonerr, mererr, vererr, pts


