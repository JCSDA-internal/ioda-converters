
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_csm.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/contributed.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/esmf/ESMF_regridding.ncl"
load "$WEBWXHOME/satellite/scripts/goes16_libs.ncl"
load "$WEBWXHOME/ncl_lib/functions.ncl"
load "$WEBWXHOME/ncl_lib/cmap_bw.ncl"
load "$WEBWXHOME/ncl_lib/cmap_sat.ncl"

;_________________________________________________________________________________________
;
; This NCL script will ingest all 16 channels of GOES-16 ABI (Advanced Baseline Imager) data.
; Post-processing of the data includes:
;   1) scale channels 1 to 6 into reflectance
;   2) scale channels 7+ into brightness temperatures (deg C)
;   3) re-map the incoming full disk or CONUS to either HRRR or Mercator projection using ESMF_regridder
;   4) plots of various channels
;
;  Notes:  Channels 1, 3, and 5 are 1km native spacing; ch2 is 0.5km, and all other channels are 2km spacing,
;          therefore, creation of 3 sets of lat/lon data are needed for re-projection.  This code will create
;          output netCDF files with lat/lon arrays for those fixed satellite grids on either full disk or CONUS
;          sector.  If those files already exist ($WEBWXHOME/satellite/params), then they are opened and read,
;          else they are generated upon running.  Also, since the satellite can move to a different location,
;          and its nadir longitude changes, then entire series of lat/lon related files need to be re-generated.
;
; Author:  G. Thompson based on code provided by D. Adriaansen and lat/lon code from A. Rugg.
;   Date:  2017 Nov 09
;
;  Sample source filename:  /var/autofs/mnt/goes-16/ABI/L1/RadC/C01/2017313/OR_ABI-L1b-RadC-M3C01_G16_s20173131607191_e20173131609563_c20173131610002.nc
;  but these are linked to somewhat simpler names like:  /home/ldm/data/goes16/ABI-L1_RadC_C01/20171109/g16_RadC_C01_20171109_160719.nc
;  Note the switch from day-of-year to month and day in subdirectory and linked file as compared to original/source file.
;
;_________________________________________________________________________________________
; RadF time 20171119 at 151540
; RadC time 20171120 at 163220


dir_prefix = "/home/ldm/data/goes16"
region     = "RadC"                        ; Either RadC or RadF for CONUS or full disk respectively.
subdir_ch  = "ABI-L1_" + region            ; the subdir also includes channel like so: ABI-L1_RadF_C01
yyyymmdd   = 20180102                      ; Year, month, date for subdir and linked file name.
hhmmss     = "162220"                      ; Hour, minute, second (seconds are rounded) from scan start time.
output_type = "png"                        ; Either png or ncgm (gmeta)
output_grid = "HRRR"                   ; Either HRRR or Mercator   (Mercator does not work as desired yet and do not use).

gmetafile = "g16_" + yyyymmdd + "_" + hhmmss + ".ncgm"
pngfile = "g16_" + yyyymmdd + "_" + hhmmss

g16_file_ch1 = dir_prefix + "/" + subdir_ch + "_C01/" + yyyymmdd + "/g16_" + region + "_C01_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch2 = dir_prefix + "/" + subdir_ch + "_C02/" + yyyymmdd + "/g16_" + region + "_C02_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch3 = dir_prefix + "/" + subdir_ch + "_C03/" + yyyymmdd + "/g16_" + region + "_C03_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch4 = dir_prefix + "/" + subdir_ch + "_C04/" + yyyymmdd + "/g16_" + region + "_C04_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch5 = dir_prefix + "/" + subdir_ch + "_C05/" + yyyymmdd + "/g16_" + region + "_C05_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch6 = dir_prefix + "/" + subdir_ch + "_C06/" + yyyymmdd + "/g16_" + region + "_C06_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch7 = dir_prefix + "/" + subdir_ch + "_C07/" + yyyymmdd + "/g16_" + region + "_C07_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch8 = dir_prefix + "/" + subdir_ch + "_C08/" + yyyymmdd + "/g16_" + region + "_C08_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch9 = dir_prefix + "/" + subdir_ch + "_C09/" + yyyymmdd + "/g16_" + region + "_C09_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch10 = dir_prefix + "/" + subdir_ch + "_C10/" + yyyymmdd + "/g16_" + region + "_C10_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch11 = dir_prefix + "/" + subdir_ch + "_C11/" + yyyymmdd + "/g16_" + region + "_C11_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch12 = dir_prefix + "/" + subdir_ch + "_C12/" + yyyymmdd + "/g16_" + region + "_C12_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch13 = dir_prefix + "/" + subdir_ch + "_C13/" + yyyymmdd + "/g16_" + region + "_C13_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch14 = dir_prefix + "/" + subdir_ch + "_C14/" + yyyymmdd + "/g16_" + region + "_C14_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch15 = dir_prefix + "/" + subdir_ch + "_C15/" + yyyymmdd + "/g16_" + region + "_C15_" + yyyymmdd + "_" + hhmmss + ".nc"
g16_file_ch16 = dir_prefix + "/" + subdir_ch + "_C16/" + yyyymmdd + "/g16_" + region + "_C16_" + yyyymmdd + "_" + hhmmss + ".nc"

;g16_file_ch1 = dir_prefix + "/g16_" + region + "_C01_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch2 = dir_prefix + "/g16_" + region + "_C02_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch3 = dir_prefix + "/g16_" + region + "_C03_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch4 = dir_prefix + "/g16_" + region + "_C04_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch5 = dir_prefix + "/g16_" + region + "_C05_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch6 = dir_prefix + "/g16_" + region + "_C06_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch7 = dir_prefix + "/g16_" + region + "_C07_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch8 = dir_prefix + "/g16_" + region + "_C08_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch9 = dir_prefix + "/g16_" + region + "_C09_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch10 = dir_prefix + "/g16_" + region + "_C10_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch11 = dir_prefix + "/g16_" + region + "_C11_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch12 = dir_prefix + "/g16_" + region + "_C12_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch13 = dir_prefix + "/g16_" + region + "_C13_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch14 = dir_prefix + "/g16_" + region + "_C14_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch15 = dir_prefix + "/g16_" + region + "_C15_" + yyyymmdd + "_" + hhmmss + ".nc"
;g16_file_ch16 = dir_prefix + "/g16_" + region + "_C16_" + yyyymmdd + "_" + hhmmss + ".nc"

months = (/"Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"/)
one_third = 1./3.

print ("Starting script for GOES-16 post-processing and plotting.  Example file name: " + g16_file_ch7)
if (output_type .eq. "png") then
    print ("Outputting graphics to file(s) with names like: " + pngfile + ".000001.png")
else
    print ("Outputting graphics to NCGM file with name: " + gmetafile)
end if

;-----------------------------------------------------------------------------------------
;..Open the various files corresponding to each channel, read the data, convert to more usable
;   variables (reflectance or brightness temp), and flip Y-dimension if needed.
;-----------------------------------------------------------------------------------------

file_g16_ch1 = addfile (g16_file_ch1, "r")
file_g16_ch2 = addfile (g16_file_ch2, "r")
file_g16_ch3 = addfile (g16_file_ch3, "r")
file_g16_ch4 = addfile (g16_file_ch4, "r")
file_g16_ch5 = addfile (g16_file_ch5, "r")
file_g16_ch6 = addfile (g16_file_ch6, "r")
file_g16_ch7 = addfile (g16_file_ch7, "r")
file_g16_ch8 = addfile (g16_file_ch8, "r")
file_g16_ch9 = addfile (g16_file_ch9, "r")
file_g16_ch10 = addfile (g16_file_ch10, "r")
file_g16_ch11 = addfile (g16_file_ch11, "r")
file_g16_ch12 = addfile (g16_file_ch12, "r")
file_g16_ch13 = addfile (g16_file_ch13, "r")
file_g16_ch14 = addfile (g16_file_ch14, "r")
file_g16_ch15 = addfile (g16_file_ch15, "r")
file_g16_ch16 = addfile (g16_file_ch16, "r")

;-----------------------------------------------------------------------------------------
;..Read in the raw radiance data, channels 1-6 and convert into reflectance while also setting missing/bad values.
;-----------------------------------------------------------------------------------------

g16_ch1 = convert_raw_2_reflectance(file_g16_ch1, 1)
g16_ch2 = convert_raw_2_reflectance(file_g16_ch2, 2)
g16_ch3 = convert_raw_2_reflectance(file_g16_ch3, 3)
g16_ch4 = convert_raw_2_reflectance(file_g16_ch4, 4)
g16_ch5 = convert_raw_2_reflectance(file_g16_ch5, 5)
g16_ch6 = convert_raw_2_reflectance(file_g16_ch6, 6)

;-----------------------------------------------------------------------------------------
;..Read in the raw radiance data, channels 7-16 and convert into brightness temperature while also setting missing/bad values.
;-----------------------------------------------------------------------------------------

g16_ch7 = convert_raw_2_brightnessTemp(file_g16_ch7, 7)
g16_ch8 = convert_raw_2_brightnessTemp(file_g16_ch8, 8)
g16_ch9 = convert_raw_2_brightnessTemp(file_g16_ch9, 9)
g16_ch10 = convert_raw_2_brightnessTemp(file_g16_ch10, 10)
g16_ch11 = convert_raw_2_brightnessTemp(file_g16_ch11, 11)
g16_ch12 = convert_raw_2_brightnessTemp(file_g16_ch12, 12)
g16_ch13 = convert_raw_2_brightnessTemp(file_g16_ch13, 13)
g16_ch14 = convert_raw_2_brightnessTemp(file_g16_ch14, 14)
g16_ch15 = convert_raw_2_brightnessTemp(file_g16_ch15, 15)
g16_ch16 = convert_raw_2_brightnessTemp(file_g16_ch16, 16)

;-----------------------------------------------------------------------------------------
;..Set up date and time variables.
;-----------------------------------------------------------------------------------------

g16_time = file_g16_ch1->t
dinfo = cd_calendar(g16_time,0)
year = tointeger(dinfo(0,0))
month = tointeger(dinfo(0,1))
day = tointeger(dinfo(0,2))
hour = tointeger(dinfo(0,3))
minute = tointeger(dinfo(0,4))
second = tointeger(dinfo(0,5))
day_number = day_of_year(year,month,day)
yyyy = sprinti("%.4i", year)
mm = sprinti("%.2i", month)
dd = sprinti("%.2i", day)
hh = sprinti("%.2i", hour)
mn = sprinti("%.2i", minute)
sc = sprinti("%.2i", second)
date_time_string = hh + ":" + mn + ":" + sc + " " + dd + " " + months(month-1) + " " + yyyy

image_time = new(1, integer)
image_time!0 = "time"
image_time@long_name = "Image start time"
image_time@calendar  = "gregorian"
image_time@units     = "seconds since 1970-01-01 00:00:0.0"
image_time = doubletointeger(ut_inv_calendar(year,month,day,hour,minute,second, image_time@units, 0))

print ("    DEBUG, time info:  " + image_time + " equates to " + date_time_string + ", which is day-of-year = " + day_number)


;-----------------------------------------------------------------------------------------
;..Get various projection info.  If exists, get GOES-16 lat/lon from external files that only need to change
;   when the nadir point of the satellite changes.  Different channels have different resolutions:
;   channels 1, 3, and 5 have 1km spacing; channel 2 is 0.5km while channels 4 and 6-16 are 2km spacing.
;-----------------------------------------------------------------------------------------

nadir_longitude = file_g16_ch1->nominal_satellite_subpoint_lon

geo_ll = file_g16_ch1->geospatial_lat_lon_extent
geo_ll_south_edge = todouble(geo_ll@geospatial_southbound_latitude)
geo_ll_west_edge  = todouble(geo_ll@geospatial_westbound_longitude)
geo_ll_north_edge = todouble(geo_ll@geospatial_northbound_latitude)
geo_ll_east_edge  = todouble(geo_ll@geospatial_eastbound_longitude)

new_left_edge = toint(geo_ll_west_edge + (nadir_longitude-geo_ll_west_edge)*0.25) + 360.0
new_right_edge = toint(geo_ll_east_edge - (geo_ll_east_edge-nadir_longitude)*0.25) + 360.0


spacing = 1000
file_goes16_latlon1 = "$WEBWXHOME/satellite/params/goes16_" + region + nadir_longitude + "latlon_" + spacing + "m.nc"

g16_lat_1000m = g16_ch1*0.0
g16_lon_1000m = g16_ch1*0.0
create_latlon(file_g16_ch1, file_goes16_latlon1, region, spacing, g16_lat_1000m, g16_lon_1000m)

spacing = 500
file_goes16_latlon2 = "$WEBWXHOME/satellite/params/goes16_" + region + nadir_longitude + "latlon_" + spacing + "m.nc"

g16_lat_500m = g16_ch2*0.0
g16_lon_500m = g16_ch2*0.0
create_latlon(file_g16_ch2, file_goes16_latlon2, region, spacing, g16_lat_500m, g16_lon_500m)

spacing = 2000
file_goes16_latlon4 = "$WEBWXHOME/satellite/params/goes16_" + region + nadir_longitude + "latlon_" + spacing + "m.nc"

g16_lat_2000m = g16_ch4*0.0
g16_lon_2000m = g16_ch4*0.0
create_latlon(file_g16_ch4, file_goes16_latlon4, region, spacing, g16_lat_2000m, g16_lon_2000m)

;-----------------------------------------------------------------------------------------
;..Try computing shortwave IR reflectance (ch7)
;-----------------------------------------------------------------------------------------

sun_zenith = compute_solarZenith(day_number, hour, minute, second, g16_lat_1000m, g16_lon_1000m)
day_or_night = new (1, logical)
day_or_night = isItDaytime(sun_zenith)
if (day_or_night .eq. 1) then
    print ("DEBUG:  it is daytime")
else
    print ("DEBUG:  it is night")
end if
g16_ch1 = g16_ch1 / sun_zenith
g16_ch3 = g16_ch3 / sun_zenith
delete(sun_zenith)
sun_zenith = compute_solarZenith(day_number, hour, minute, second, g16_lat_500m, g16_lon_500m)
g16_ch2 = g16_ch2 / sun_zenith
delete(sun_zenith)


sun_zenith = compute_solarZenith(day_number, hour, minute, second, g16_lat_2000m, g16_lon_2000m)

ch7_refl = compute_swir_reflect(file_g16_ch7, g16_ch14, sun_zenith, 7)

ch6_refl = compute_swir_reflect(file_g16_ch6, g16_ch14, sun_zenith, 6)


;-----------------------------------------------------------------------------------------
;..Re-project using ESMF-regridder
;   Source grid and destination grid files are typically pre-created once and re-used,
;   basically containing the weights for regridding.  If they do not already exist, this
;   code will create them and it can take a very long time.
;-----------------------------------------------------------------------------------------

srcGridName1 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_1000m_SCRIP.nc"
srcGridName2 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_500m_SCRIP.nc"
srcGridName4 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_2000m_SCRIP.nc"

;-----------------------------------------------------------------------------------------
;..Read in external file of lat, lon arrays for the destination grid (HRRR model in this case).
;   Or, create our own Mercator grid with high resolution (1x1km) within the center ~70% of view of the satellite.
;   Also convert destination grid to a SCRIP convention file for use by ESMF-regrid.
;-----------------------------------------------------------------------------------------

if (output_grid .eq. "HRRR") then
    hrrr_latlonfile = "$WEBWXHOME/satellite/params/HRRR_latitude.nc"
    dstGridName = "$WEBWXHOME/satellite/params/hrrrmap_SCRIP.nc"
    wgtFileName1 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_1000m_2_HRRRmap_weights.nc"
    wgtFileName2 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_500m_2_HRRRmap_weights.nc"
    wgtFileName4 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_2000m_2_HRRRmap_weights.nc"

    if (.not.(isfilepresent(hrrr_latlonfile))) then
        print ("WARNING, file not present, " + hrrr_latlonfile)
    end if

    HRRR_latlon = addfile (hrrr_latlonfile, "r")
    output_lat = HRRR_latlon->latitude(0,:,:)
    output_lon = HRRR_latlon->longitude(0,:,:)
    mp_projection = "LambertConformal"
    dims1 = dimsizes(output_lat)
    nx = dims1(1)
    ny = dims1(0)
    lat_LL = output_lat@La1
    lon_LL = output_lat@Lo1
    lat_UR = output_lat(ny-1,nx-1)
    lon_UR = output_lon(ny-1,nx-1)
    cen_long = output_lat@Lov
    true_lat1 = output_lat@Latin1
    true_lat2 = output_lat@Latin2

    if (.not.(isfilepresent(dstGridName))) then
        create_file_SCRIP(dstGridName, "HRRR", output_lat@Dx*1000, output_lat@Lov, output_lat, output_lon)
    end if

else

    if (output_grid .eq. "Mercator") then

        wgtFileName1 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_1000m_2_Mercator_weights.nc"
        wgtFileName2 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_500m_2_Mercator_weights.nc"
        wgtFileName4 = "/wx/data/satellite/goes16_" + region + nadir_longitude + "_2000m_2_Mercator_weights.nc"

        dstGridName = "/wx/data/satellite/goes16_conus_mercator_SCRIP.nc"
        lat_LL = 15.0
        lon_LL = 230.0
        lat_UR = 55.0
        lon_UR = 300.0
        true_lat1 = 40.0          ; These 3 variables are ignored for Mercator but need to set them anyway.
        true_lat2 = 40.0
        cen_long = -105.0 + 360
        mp_projection = "Mercator"
        output_lat = fspan(lat_LL, lat_UR, toint((lat_UR-lat_LL)/0.05 + 1.5))
        output_lon = fspan(lon_LL, lon_UR, toint((lon_UR-lon_LL)/0.05 + 1.5))

        if (.not.(isfilepresent(dstGridName))) then
            Opt                = True
            Opt@LLCorner       = (/ lat_LL, lon_LL/)
            Opt@URCorner       = (/ lat_UR, lon_UR/)
            Opt@ForceOverwrite = True
            Opt@PrintTimings   = True
            Opt@Debug          = True
            Opt@NetCDFType     = "netcdf4"
            Opt@Title          = "GOES-16 1km spaced Mercator grid"

            print ("  calling rectilinear_to_SCRIP for Mercator projection to create new file: " + dstGridName + " with edges:  (" + lat_LL + ", " + lon_LL + ")  to  (" + lat_UR + ", " + lon_UR + ")")
            rectilinear_to_SCRIP(dstGridName, output_lat, output_lon, Opt)

            ;---Clean up
            delete(Opt)
        end if


    else
        print ("ABORT, need to set the output_grid to HRRR or Mercator.")
    end if

end if

;----------------------------------------------------------------------
;..Convert original source grid to a SCRIP convention file and
;   generate the weights from the SCRIP file for source and destination grids.
;----------------------------------------------------------------------

create_file_SCRIP(srcGridName1, region, 1000, nadir_longitude, g16_lat_1000m, g16_lon_1000m)
create_file_SCRIP(srcGridName2, region, 500, nadir_longitude, g16_lat_500m, g16_lon_500m)
create_file_SCRIP(srcGridName4, region, 2000, nadir_longitude, g16_lat_2000m, g16_lon_2000m)

create_file_WGT(srcGridName1, dstGridName, wgtFileName1, region, 1000, nadir_longitude)
create_file_WGT(srcGridName2, dstGridName, wgtFileName2, region, 500, nadir_longitude)
create_file_WGT(srcGridName4, dstGridName, wgtFileName4, region, 2000, nadir_longitude)


;----------------------------------------------------------------------
;..Re-map all the channels to the HRRR or Mercator grid.
;----------------------------------------------------------------------

print ("    DEBUG,  regridding the data ...")
Opt                = True
Opt@PrintTimings   = True

ch1_regrid = ESMF_regrid_with_weights (g16_ch1, wgtFileName1, Opt)
ch2_regrid = ESMF_regrid_with_weights (g16_ch2, wgtFileName2, Opt)
ch3_regrid = ESMF_regrid_with_weights (g16_ch3, wgtFileName1, Opt)
ch4_regrid = ESMF_regrid_with_weights (g16_ch4, wgtFileName4, Opt)
ch5_regrid = ESMF_regrid_with_weights (g16_ch5, wgtFileName1, Opt)
ch6_regrid = ESMF_regrid_with_weights (g16_ch6, wgtFileName4, Opt)
ch7_regrid = ESMF_regrid_with_weights (g16_ch7, wgtFileName4, Opt)
ch8_regrid = ESMF_regrid_with_weights (g16_ch8, wgtFileName4, Opt)
ch9_regrid = ESMF_regrid_with_weights (g16_ch9, wgtFileName4, Opt)
ch10_regrid = ESMF_regrid_with_weights (g16_ch10, wgtFileName4, Opt)
ch11_regrid = ESMF_regrid_with_weights (g16_ch11, wgtFileName4, Opt)
ch12_regrid = ESMF_regrid_with_weights (g16_ch12, wgtFileName4, Opt)
ch13_regrid = ESMF_regrid_with_weights (g16_ch13, wgtFileName4, Opt)
ch14_regrid = ESMF_regrid_with_weights (g16_ch14, wgtFileName4, Opt)
ch15_regrid = ESMF_regrid_with_weights (g16_ch15, wgtFileName4, Opt)
ch16_regrid = ESMF_regrid_with_weights (g16_ch16, wgtFileName4, Opt)

output_mask = where(ismissing(ch14_regrid), 0, 1)

new_regrid = ESMF_regrid_with_weights (ch7_refl, wgtFileName4, Opt)
delete (ch7_refl)
ch7_refl = new_regrid

delete (new_regrid)
new_regrid = ESMF_regrid_with_weights (ch6_refl, wgtFileName4, Opt)
delete (ch6_refl)
ch6_refl = new_regrid

;----------------------------------------------------------------------
;..Restrict the data bounds of each channel.
;----------------------------------------------------------------------

ch1_regrid = where(ismissing(ch1_regrid), 0.01, ch1_regrid)
ch1_regrid = ch1_regrid > 0.01
ch1_regrid = ch1_regrid < 0.99

ch2_regrid = where(ismissing(ch2_regrid), 0.01, ch2_regrid)
ch2_regrid = ch2_regrid > 0.01
ch2_regrid = ch2_regrid < 0.99

ch3_regrid = where(ismissing(ch3_regrid), 0.01, ch3_regrid)
ch3_regrid = ch3_regrid > 0.01
ch3_regrid = ch3_regrid < 0.99

ch4_regrid = where(ismissing(ch4_regrid), 0.01, ch4_regrid)
ch4_regrid = ch4_regrid > 0.01
ch4_regrid = ch4_regrid < 0.99

ch8_regrid = where(ismissing(ch8_regrid), -99., ch8_regrid)
ch8_regrid = where(ch8_regrid.lt.-85, -99., ch8_regrid)
ch8_regrid = ch8_regrid < 55.

ch9_regrid = where(ismissing(ch9_regrid), -99., ch9_regrid)
ch9_regrid = where(ch9_regrid.lt.-85, -99., ch9_regrid)
ch9_regrid = ch9_regrid < 55.

ch10_regrid = where(ismissing(ch10_regrid), -99., ch10_regrid)
ch10_regrid = where(ch10_regrid.lt.-85, -99., ch10_regrid)
ch10_regrid = ch10_regrid < 55.

ch14_regrid = where(ismissing(ch14_regrid), -99., ch14_regrid)
ch14_regrid = where(ch14_regrid.lt.-85, -99., ch14_regrid)
ch14_regrid = ch14_regrid < 55.

ch15_regrid = where(ismissing(ch15_regrid), -99., ch15_regrid)
ch15_regrid = where(ch15_regrid.lt.-85, -99., ch15_regrid)
ch15_regrid = ch15_regrid < 55.


;-----------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------
;..All graphical generation below here.
;-----------------------------------------------------------------------------------------
;-----------------------------------------------------------------------------------------


    ;-------------------------------------------------------------------------------------
    ; Open a workstation, ncgm, or postscript file; utilize custom colormap.
    ;-------------------------------------------------------------------------------------

    if (output_type .eq. "png") then
        wks_type = "png"
        wks_type@wkWidth  = 1800
        wks_type@wkHeight = 1800
        wks = gsn_open_wks (wks_type, pngfile)
    else
        wks = xwk("ncgm", gmetafile)
    end if
    if (output_grid .eq. "Mercator") then
        setvalues NhlGetWorkspaceObjectId()
            "wsMaximumSize" : 500000000
        end setvalues
    end if

    cmap=gen_bwcolormap()
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    ;-------------------------------------------------------------------------------------
    ; Setup Map characteristics based on HRRR or Mercator grid
    ;-------------------------------------------------------------------------------------


    mp = create "mp" mapPlotClass wks end create

    setvalues mp
        "vpXF": 0.001
        "vpYF": 0.999
        "vpWidthF": 0.999
        "vpHeightF": 0.999
        "mpDataBaseVersion"     : "Ncarg4_1"
        "mpOutlineBoundarySets" : "NoBoundaries"
        "mpGridAndLimbOn"       : False
        "mpProjection"          : mp_projection
        "mpLimitMode"           : "corners"
        "mpLeftCornerLatF"      : lat_LL
        "mpLeftCornerLonF"      : lon_LL
        "mpRightCornerLatF"     : lat_UR
        "mpRightCornerLonF"     : lon_UR
        "mpLambertParallel1F"   : true_lat1       ; Is it ignored if the projection is Mercator
        "mpLambertParallel2F"   : true_lat2       ; Is it ignored if the projection is Mercator
        "mpLambertMeridianF"    : cen_long        ; Is it ignored if the projection is Mercator
        "mpFillOn"              : True
        "mpLandFillColor"       : 46
        "mpOceanFillColor"      : 47
        "mpInlandWaterFillColor": 47
        "mpPerimOn"             : False
    end setvalues

    ;-------------------------------------------------------------------------------------
    ; Actual plotted window differs from requested because of map aspect ratio.
    ;-------------------------------------------------------------------------------------

    getvalues mp
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
    end getvalues

    mp2 = create "mp2" mapPlotClass wks end create
    setvalues mp2
        "vpXF": vpx
        "vpYF": vpy
        "vpWidthF": vpw
        "vpHeightF": vph
        "mpDataBaseVersion"     : "Ncarg4_1"
        "mpOutlineBoundarySets" : "National"
        "mpOutlineSpecifiers"   : (/"United States: States", "Mexico: States", "Canada: Provinces"/)
        "mpProjection"          : mp_projection
        "mpLimitMode"           : "corners"
        "mpLeftCornerLatF"      : lat_LL
        "mpLeftCornerLonF"      : lon_LL
        "mpRightCornerLatF"     : lat_UR
        "mpRightCornerLonF"     : lon_UR
        "mpLambertParallel1F"   : true_lat1       ; Is it ignored if the projection is Mercator
        "mpLambertParallel2F"   : true_lat2       ; Is it ignored if the projection is Mercator
        "mpLambertMeridianF"    : cen_long        ; Is it ignored if the projection is Mercator
        "mpFillOn"              : False
        "mpGridAndLimbOn"       : True
        "mpGridSpacingF"        : 5.0
        "mpGridMaskMode"        : "MaskLand"
        "mpGridLineDashPattern" : 2
        ; "mpNationalLineThicknessF" : 2.0
        "mpLabelsOn"            : False
        "mpPerimOn"             : True
        "mpGeophysicalLineColor": 2
        "mpUSStateLineColor"    : 1
        "mpNationalLineColor"   : 2
        "mpGridLineColor"       : 0
    end setvalues

    ;-------------------------------------------------------------------------------------
    ; Make a text annotation string for the data and time info.
    ;-------------------------------------------------------------------------------------

    date_time = create "date_time" textItemClass wks
        "txPosXF" : vpx + 0.005
        "txPosYF" : vpy + 0.005
        "txString" : date_time_string
        "txFont" : "courier"
        "txFontColor" : 1
        "txFuncCode" : "~"
        "txJust" : "BOTTOMLEFT"
        "txFontHeightF" : 0.010
    end create

    ;-------------------------------------------------------------------------------------
    ; Combine channels 1, 2, and 3 for a grayscale visible albedo image.
    ;-------------------------------------------------------------------------------------

    vis_combined = ch1_regrid*one_third + ch2_regrid*one_third + ch3_regrid*one_third

    cn_1 = contour(wks,MakeData(sqrt(vis_combined), "null"),"null")

    setvalues cn_1
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ExplicitLevels"
        "cnLevels"             : fspan(0.01, 0.96, 96)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "cnFillOn"             : True
        "cnFillColors"         : ispan(3,99,1)
        "tiMainString"         : "GOES-16 combined (ch1, 2, 3) visible albedo"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.011
        "lbLabelStride"        : 3
        "lbTitleOn"            : True
        "lbTitleString"        : " "
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    print ("    DEBUG,  drawing ...")

    draw ((/mp, cn_1, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now use channel 1 as blue, channel 2 as red, and channel 3 as green in a false-color visible image.
    ;-------------------------------------------------------------------------------------

    ramp   = fspan(0., 1., 255)
    reds   = new((/255, 4/), float)
    greens = new((/255, 4/), float)
    blues  = new((/255, 4/), float)

    reds   = 0
    greens = 0
    blues  = 0

    reds(:,0)   = ramp
    greens(:,1) = ramp
    blues(:,2)  = ramp

    ;----------------------------------------------------------------------
    ; The red contour map is plotted fully opaque; the green and blue
    ; are plotted completely transparent. When overlain, the colors 
    ; combine (rather magically).
    ;----------------------------------------------------------------------

    reds(:,3)   = 1.
    greens(:,3) = 0 
    blues(:,3)  = 0

    cn_blue = contour(wks,MakeData(sqrt(ch1_regrid), "null"), "null")
    setvalues cn_blue
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "EqualSpacedLevels"
        "cnMaxLevelCount"      : 254
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "cnFillOn"             : True
        "cnFillColors"         : blues
        "tiMainString"         : "GOES-16 RGB (ch1, 2, 3) visible image"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarDisplayMode" : "NEVER"
    end setvalues

    cn_red = contour(wks,MakeData(sqrt(ch2_regrid), "null"), "null")
    setvalues cn_red
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "EqualSpacedLevels"
        "cnMaxLevelCount"      : 254
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "cnFillOn"             : True
        "cnFillColors"         : reds
        "pmLabelBarDisplayMode" : "NEVER"
    end setvalues

    ;   green_data = green_data * 0.06038137 + blue_data * 0.45706946 + red_data * 0.48358168
    cn_green = contour(wks,MakeData(sqrt(ch3_regrid*0.06038137 + ch1_regrid*0.45706946 + ch2_regrid*0.48358168), "null"), "null")
    setvalues cn_green
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "EqualSpacedLevels"
        "cnMaxLevelCount"      : 254
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "cnFillOn"             : True
        "cnFillColors"         : greens
        "pmLabelBarDisplayMode" : "NEVER"
    end setvalues

    overlay(cn_red, cn_green)
    overlay(cn_red, cn_blue)

    draw ((/mp, cn_red, mp2, date_time/))
    frame(wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the usual longwave InfraRed (IR) [channel 14 near 11 microns] color image.
    ;-------------------------------------------------------------------------------------

    delete (cmap)
    cmap = gen_colorsat_ch4()
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -78.0
    cnMaxVal = 47.0
    cnSpacing =  (cnMaxVal-cnMinVal)/250.
    cnColorVals = ispan(5, 254, 250)
    delete (ch14_regrid@_FillValue)

    cn_14 = contour(wks,MakeData(ch14_regrid, "null"),"null")

    setvalues cn_14
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ 1, 1, 1, 1, 1, cnColorVals /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16 channel 14 infrared (11 micron)"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 4
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_14, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the usual water vapor IR [channel 9 near 6.95 microns] color image.
    ;-------------------------------------------------------------------------------------

    delete (cmap)
    cmap = gen_colorsat_ch3()
    temp = cmap
    cmap(0, :) = (/1.0,1.0,1.0/)
    cmap(1, :) = (/0.0,0.0,0.0/)
    cmap(2, :) = (/0.0,0.0,0.0/)
    cmap(3:254,:) = temp(3:254:-1,:)
    delete(temp)
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -75.0
    cnMaxVal = 0.0
    cnSpacing =  (cnMaxVal-cnMinVal)/250.
    cnColorVals = ispan(2, 254, 253)
    delete (ch9_regrid@_FillValue)

    cn_9 = contour(wks,MakeData(ch9_regrid, "null"),"null")

    setvalues cn_9
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ cnColorVals, 1 /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16 channel 9 (7 micron) mid-level water vapor"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 9
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_9, mp2, date_time/))
    frame (wks)

    ;-------------------------------------------------------------------------------------
    ; Now plot the water vapor IR [channel 8 near 6.2 microns] color image.
    ;-------------------------------------------------------------------------------------

    delete (ch8_regrid@_FillValue)

    cn_8 = contour(wks,MakeData(ch8_regrid, "null"),"null")

    setvalues cn_8
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ cnColorVals, 1 /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16 channel 8 (6.2 micron) high-level water vapor"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 9
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_8, mp2, date_time/))
    frame (wks)

    ;-------------------------------------------------------------------------------------
    ; Now plot the water vapor IR [channel 10 near 7.3 microns] color image.
    ;-------------------------------------------------------------------------------------

    delete (ch10_regrid@_FillValue)

    cn_10 = contour(wks,MakeData(ch10_regrid, "null"),"null")

    setvalues cn_10
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ cnColorVals, 1 /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16 channel 10 (7.3 micron) low-level water vapor"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 9
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_10, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the brightness temp difference (BTD) shortwave-longwave (3.9 minus 11 microns)
    ;-------------------------------------------------------------------------------------

    delete (cmap)
    cmap = gen_colorsat_ch1()
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -6.0
    cnMaxVal = 43.0
    cnSpacing =  (cnMaxVal-cnMinVal)/98.
    cnColorVals = ispan(2, 99, 98)

    btd_ch07m14 = ch7_regrid-ch14_regrid
    delete (btd_ch07m14@_FillValue)

    cn_btd_ch7m14 = contour(wks,MakeData((btd_ch07m14), "null"),"null")

    setvalues cn_btd_ch7m14
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ 1, cnColorVals /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16  BTD shortwave-longwave IR"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 5
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_btd_ch7m14, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the brightness temp difference (BTD) (11 minus 12 microns), possible volcanic ash or thin cirrus
    ;-------------------------------------------------------------------------------------


    delete (cmap)
    cmap = gen_colorsat_ch1()
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -3.0
    cnMaxVal = 7.0
    cnSpacing =  (cnMaxVal-cnMinVal)/100.
    cnColorVals = ispan(2, 98, 97)

    btd_ch14m15 = ch14_regrid-ch15_regrid
    delete (btd_ch14m15@_FillValue)

    cn_btd_ch14m15 = contour(wks,MakeData((btd_ch14m15), "null"),"null")

    setvalues cn_btd_ch14m15
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ 1, cnColorVals, 1, 1 /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16  BTD 11 minus 12 micron (volcanic ash, thin cirrus)"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 10
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_btd_ch14m15, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the brightness temp difference (BTD) for mid minus low WV (6.9 minus 7.4 microns)
    ;-------------------------------------------------------------------------------------


    delete (cmap)
    cmap = gen_colorsat_ch3()
    temp = cmap
    cmap(0, :) = (/1.0,1.0,1.0/)
    cmap(1, :) = (/0.0,0.0,0.0/)
    cmap(2, :) = (/0.0,0.0,0.0/)
    cmap(3:254,:) = temp(3:254:-1,:)
    delete(temp)
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -24.0
    cnMaxVal = 1.0
    cnSpacing =  (cnMaxVal-cnMinVal)/250.
    cnColorVals = ispan(2, 254, 253)

    btd_ch09m10 = ch9_regrid-ch10_regrid
    btd_ch09m10@_FillValue = -99
    btd_ch09m10 = where(output_mask.eq.0, btd_ch09m10@_FillValue, btd_ch09m10)
    delete(btd_ch09m10@_FillValue)

    cn_btd_ch09m10 = contour(wks,MakeData((btd_ch09m10), "null"),"null")

    setvalues cn_btd_ch09m10
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ 1, cnColorVals, 1, 1 /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16  BTD  mid minus low-level WV channels (ch9 - ch10)"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 10
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_btd_ch09m10, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the brightness temp difference (BTD) for low WV minus longwave IR (7.4 minus 11 microns)
    ;-------------------------------------------------------------------------------------


    delete (cmap)
    cmap = gen_colorsat_ch3()
    temp = cmap
    cmap(0, :) = (/1.0,1.0,1.0/)
    cmap(1, :) = (/0.0,0.0,0.0/)
    cmap(2, :) = (/0.0,0.0,0.0/)
    cmap(3:254,:) = temp(3:254:-1,:)
    delete(temp)
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -24.0
    cnMaxVal = 1.0
    cnSpacing =  (cnMaxVal-cnMinVal)/250.
    cnColorVals = ispan(3, 252, 250)

    btd_ch10m14 = ch10_regrid-ch14_regrid
    btd_ch10m14@_FillValue = 99
    btd_ch10m14 = where(output_mask.eq.0, btd_ch10m14@_FillValue, btd_ch10m14)
    delete(btd_ch10m14@_FillValue)

    cn_btd_ch10m14 = contour(wks,MakeData((btd_ch10m14), "null"),"null")

    setvalues cn_btd_ch10m14
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ cnColorVals, 1 /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16  BTD  low-level WV - longwave IR channels (ch10 - ch14)"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 10
        "lbTitleOn"            : True
        "lbTitleString"        : ":S:o:N:C"
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_btd_ch10m14, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now use channel 14 as blue, channel 2 as red, and channel 7 as green for daytime Microphysics recipe.
    ;-------------------------------------------------------------------------------------

;   reds(249:254,0)   = 0.0
;   greens(249:254,1) = 0.0
;   blues(249:254,2)  = 0.0

    ch14_regrid@_FillValue = 99
    ch14_regrid = where(output_mask.eq.0, ch14_regrid@_FillValue, ch14_regrid)
    delete(ch14_regrid@_FillValue)

    delete(cn_blue)
    cn_blue = contour(wks,MakeData(ch14_regrid, "null"), "null")
    setvalues cn_blue
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : 0.5
        "cnMinLevelValF"       : -74.5
        "cnMaxLevelValF"       : 49.5
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "cnFillOn"             : True
        "cnFillColors"         : blues
        "tiMainString"         : "GOES-16 Daytime Microphysics RGB (ch3, 7, 14) image"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarDisplayMode" : "NEVER"
    end setvalues

    ch3_regrid@_FillValue = -1
    ch3_regrid = where(output_mask.eq.0, ch3_regrid@_FillValue, ch3_regrid)
    delete(ch3_regrid@_FillValue)

    delete(cn_red)
    cn_red = contour(wks,MakeData(sqrt(sqrt(ch3_regrid)), "null"), "null")
    setvalues cn_red
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : 0.00392              ; ( 0.0 to 0.98 divided by 250)
        "cnMinLevelValF"       : 0.01
        "cnMaxLevelValF"       : 0.98
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "cnFillOn"             : True
        "cnFillColors"         : reds
        "pmLabelBarDisplayMode" : "NEVER"
    end setvalues

    ch7_refl@_FillValue = -1
    ch7_refl = where(output_mask.eq.0, ch7_refl@_FillValue, ch7_refl)
    delete(ch7_refl@_FillValue)

    delete(cn_green)
    cn_green = contour(wks,MakeData(sqrt(sqrt(ch7_refl)), "null"), "null")
    setvalues cn_green
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : 0.0028               ; ( 0.0 to 0.7 divided by 250)
        "cnMinLevelValF"       : 0.01
        "cnMaxLevelValF"       : 0.7
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "cnFillOn"             : True
        "cnFillColors"         : greens
        "pmLabelBarDisplayMode" : "NEVER"
    end setvalues

    overlay(cn_red, cn_green)
    overlay(cn_red, cn_blue)

    draw ((/mp, cn_red, mp2, date_time/))
    frame(wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the shortwave IR reflectance at 3.9 microns (ch7)
    ;-------------------------------------------------------------------------------------


    delete (cmap)
    cmap = gen_colorsat_ch1()
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -0.02
    cnMaxVal = 0.98
    cnSpacing =  (cnMaxVal-cnMinVal)/100.
    cnColorVals = ispan(2, 97, 96)

    cn_ch7_refl = contour(wks,MakeData(ch7_refl, "null"),"null")

    setvalues cn_ch7_refl
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ 1, 1, cnColorVals /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16  Shortwave IR (ch7) reflectance"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 10
        "lbTitleOn"            : True
        "lbTitleString"        : " "
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_ch7_refl, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the shortwave IR reflectance at 2.25 microns (ch6)
    ;-------------------------------------------------------------------------------------


    cnMinVal = -0.02
    cnMaxVal = 0.98
    cnSpacing =  (cnMaxVal-cnMinVal)/100.
    cnColorVals = ispan(2, 97, 96)
    ch6_refl@_FillValue = -1
    ch6_refl = where(output_mask.eq.0, ch6_refl@_FillValue, ch6_refl)

    cn_ch6_refl = contour(wks,MakeData(ch6_refl, "null"),"null")

    setvalues cn_ch6_refl
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : (/ 1, 1, cnColorVals /)
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16  Shortwave IR (ch6) reflectance"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 10
        "lbTitleOn"            : True
        "lbTitleString"        : " "
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_ch6_refl, mp2, date_time/))
    frame (wks)


    ;-------------------------------------------------------------------------------------
    ; Now plot the brightness temp difference (BTD) for 1.6 microns (ch5) minus 2.25 microns (ch6)
    ;-------------------------------------------------------------------------------------


    delete (cmap)
    cmap = gen_colorsat_ch3()
    temp = cmap
    cmap(0, :) = (/1.0,1.0,1.0/)
    cmap(1, :) = (/0.0,0.0,0.0/)
    cmap(2, :) = (/0.0,0.0,0.0/)

    cmap(3:74,:) = temp(178:249:-1,:)
    cmap(75:82,:) = (/ temp(74,:), temp(77,:), temp(80,:), temp(83,:), temp(86,:), temp(89,:), temp(92,:), temp(95,:) /)
    cmap(83:103,:) = temp(31:51,:)
    cmap(104:127,:) = temp(6:29,:)
    cmap(128, :) = (/0.0,0.0,0.0/)

    delete(temp)
    setvalues wks
        "wkColorMap" : cmap
    end setvalues

    cnMinVal = -0.15
    cnMaxVal = 0.1
    cnSpacing =  (cnMaxVal-cnMinVal)/125.
    cnColorVals = ispan(2, 128, 127)

    btd_ch05m06 = ch5_regrid-ch6_regrid
    btd_ch05m06@_FillValue = 99
    btd_ch05m06 = where(output_mask.eq.0, btd_ch05m06@_FillValue, btd_ch05m06)
    delete(btd_ch05m06@_FillValue)

    cn_btd_ch05m06 = contour(wks,MakeData((btd_ch05m06), "null"),"null")

    setvalues cn_btd_ch05m06
        "vpXF" : vpx
        "vpYF" : vpy
        "vpWidthF" : vpw
        "vpHeightF" : vph
        "cnRasterModeOn"       : True
        "cnRasterSmoothingOn"  : False
        "cnLevelSelectionMode" : "ManualLevels"
        "cnLevelSpacingF"      : cnSpacing
        "cnMinLevelValF"       : cnMinVal
        "cnMaxLevelValF"       : cnMaxVal
        "cnFillColors"         : cnColorVals
        "cnLineLabelsOn"       : False
        "cnLinesOn"            : False
        "tiMainString"         : "GOES-16  BTD  1.6 - 2.2 microns (ch5 - ch6)"
        "tiMainFontHeightF"    : 0.012
        "pmLabelBarWidthF"     : vpw*.95
        "lbLabelsOn"           : True
        "lbLabelFontHeightF"   : 0.010
        "lbLabelStride"        : 5
        "lbTitleOn"            : True
        "lbTitleString"        : " "
        "lbTitleFontHeightF"   : 0.010
    end setvalues

    draw ((/mp, cn_btd_ch05m06, mp2, date_time/))
    frame (wks)


