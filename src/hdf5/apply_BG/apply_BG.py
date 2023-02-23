"""
J. Zhou, H. Yang and R. Iacovazzi,
"Improving ATMS Remapping Accuracy Using Adaptive Window and Noise-Tuning Method in Backus–Gilbert Inversion,"
in IEEE Transactions on Geoscience and Remote Sensing,
vol. 60, pp. 1-12, 2022, Art no. 5304412, doi: 10.1109/TGRS.2022.3182630.

Original source code written by Jun Zhou @UMD can be found at:
https://github.com/JunUMD/BG_AdaptiveWindow_v1
"""
from pathlib import Path
import os, glob, time
import pickle, sys
import h5py
import numpy as np
import statistics
from scipy.interpolate import griddata


class apply_BG_class:
    def __init__(self,
                 input_files,
                 orb, orbit_num,
                 src_ch, coef_dir,
                 nfov,
                 ta_vmin, ta_vmax,
                 dif_vmin, dif_vmax):

        self.input_files = input_files
        self.orb = orb,
        self.orbit_num = orbit_num
        self.select_chs = src_ch
        self.select_nch = len(self.select_chs)
        self.coef_dir = coef_dir
        self.nfov = nfov

        self.ta_vmin = ta_vmin
        self.ta_vmax = ta_vmax
        self.dif_vmin = dif_vmin
        self.dif_vmax = dif_vmax

    def ingest(self):

        self.do_ingest()

    def prepcoef(self):

        self.alpha_all = []
        self.windowsize_all = []
        self.windowindx_all = []
        self.idx_src_all = []

        for ich in [0, 1]:
            self.alpha_all.append([])
            self.windowsize_all.append([])
            self.windowindx_all.append([])
            self.idx_src_all.append([])

            #Read in coefficients form a single NetCDF file. Ch1 and Ch2 use the same coefficients.
            f = h5py.File('/glade/scratch/jban/pandac/obs_220127/atms_nasa/tgt_ifr.nc','r')
            for ifv in range(self.nfov):
                windowsize = np.array(f['windowSize'][ifv])
                alpha_bst = np.array(f['alpha'][ifv, 0:windowsize])
                windowindx = np.array(f['windowIndex'][ifv, 0:windowsize, :])
                idx_src = np.array(f['indexSrc'][ifv])

                self.alpha_all[ich].append(alpha_bst)
                self.windowsize_all[ich].append(windowsize)
                self.windowindx_all[ich].append(windowindx)
                self.idx_src_all[ich].append(idx_src)

    def apply(self):
        for ich in [0, 1]:
            missing_value = 9.96921e+36
            ta_rmp = np.full_like(self.ta[ich], missing_value)
            for ifr in range(self.nfov):
                windowsize = self.windowsize_all[ich][ifr]
                windowindx = self.windowindx_all[ich][ifr]
                idx_src = self.idx_src_all[ich][ifr]
                alpha = self.alpha_all[ich][ifr]
                isc_cen = windowindx[idx_src-1][0]
                ifr_cen = windowindx[idx_src-1][1]

                for i in range(windowsize):
                    windowindx[i][0] = windowindx[i][0]-isc_cen
                    windowindx[i][1] = windowindx[i][1]-ifr_cen

                alpha = alpha/np.sum(alpha)
                for isc in range(self.nscan):
                    temp = 0.0
                    flag = 0
                    for iwin in range(windowsize):
                        [i, j] = np.array(windowindx[iwin])+np.array([isc, ifr])
                        if i >= 0 and i < self.nscan and j >= 0 and j < self.nfov:
                            temp += alpha[iwin]*self.ta[ich, i, j]
                        else:
                            flag = 1
                            break
                    if flag == 0:
                        ta_rmp[isc, ifr] = temp
            self.taAllCh[:, :, ich] = ta_rmp

        return [self.viewang, self.taAllCh, self.lat[ich, :, :], self.lon[ich, :, :], self.satzen, self.satazi, self.solzen, self.solazi, self.tim]

    def do_ingest(self):

        self.ta = []
        self.lat = []
        self.lon = []
        self.satzen = []
        self.satazi = []
        self.satran = []
        self.solzen = []
        self.solazi = []
        self.viewang = []
        self.tim = []
        self.nscan = 0
        self.taAllCh = []
        self.taAllCh.append([])
        for ele in self.select_chs:
            self.ta.append([])
            self.lat.append([])
            self.lon.append([])
            self.satzen.append([])
            self.satazi.append([])
            self.satran.append([])
            self.solzen.append([])
            self.solazi.append([])
            self.viewang.append([])
            self.tim.append([])

        for ifile, filename in enumerate(self.input_files):
            [viewang, temAllCh, ta, lat, lon, satzen, satazi, satran, tim, vel, nscan, solzen, solazi] = self.read_granule(filename)
            for isc in range(nscan):
                if (vel[isc, -1] > 0 and self.orb == 'des') or (vel[isc, -1] < 0 and self.orb == 'asc'):
                    continue
                self.nscan += 1
                for i, ich in enumerate(self.select_chs):
                    self.ta[i].append(ta[isc, :, int(ich)-1])
                    self.lat[i].append(lat[isc, :])
                    self.lon[i].append(lon[isc, :])
                    self.satzen[i].append(satzen[isc, :])
                    self.satazi[i].append(satazi[isc, :])
                    self.satran[i].append(satran[isc, :])
                    self.solzen[i].append(solzen[isc, :])
                    self.solazi[i].append(solazi[isc, :])
                    self.viewang[i].append(viewang[isc, :])
                    self.tim[i].append(tim[isc, :])
                self.taAllCh.append(temAllCh[isc, :, :])
        self.ta = np.array(self.ta)
        self.lat = np.array(self.lat)
        self.lon = np.array(self.lon)
        self.nscan = np.array(self.nscan)
        self.satzen = np.array(self.satzen)
        self.satazi = np.array(self.satazi)
        self.satran = np.array(self.satran)
        self.solzen = np.array(self.solzen)
        self.solazi = np.array(self.solazi)
        self.viewang = np.array(self.viewang)
        self.tim = np.array(self.tim)
        del self.taAllCh[0]
        self.taAllCh = np.array(self.taAllCh, dtype=object)

    def read_granule(self, ffile):
        f = h5py.File(ffile, 'r')
        tem = np.array(f['antenna_temp'])
        temAllCh = np.array(f['antenna_temp'])
        # nasa: antenna_temp(atrack, xtrack, channel) 136,96,22
        # noaa: BrightnessTemperature(phony_dim_5, phony_dim_6, phony_dim_7) 180,96,22
        tim = np.array(f['obs_time_utc'], dtype='int64')

        lat = np.array(f['lat'], dtype='float32')
        lon = np.array(f['lon'], dtype='float32')
        vel = np.array(f['sat_vel'])

        satzen = np.array(f['sat_zen'])
        satazi = np.array(f['sol_azi'])
        satran = np.array(f['sat_range'])

        solzen = np.array(f['sol_zen'])
        solazi = np.array(f['sol_azi'])

        viewang = np.array(f['view_ang'])
        # f.close()
        nscan = tem.shape[0]

        return [viewang, temAllCh, tem, lat, lon, satzen, satazi, satran, tim, vel, nscan, solzen, solazi]
