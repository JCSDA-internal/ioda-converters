#import sss2ioda

import imp
foo = imp.load_source('preobs', '../src/marine/smap/sss2ioda.py')

#from /Users/Hamideh/Documents/mine/SOCA/ioda-converters/src/marine/smap/sss2ioda import preobs
filename='./testinput/SMAP_L2B_SSS_17104_20180415T010704_R16010_V4.2.h5'
zz=foo.preobs(filename, window_length=24.0, midwindow_date='2018041500')
zz.toioda(fname='smap_test.nc')
