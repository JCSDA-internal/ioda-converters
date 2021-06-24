;----------------------------------------------------------------------------------------------------
;..This function takes the input file from one channel of GOES-16 data and computes reflectance from
;   raw radiance data and the kappa variable.  It will also check the data quality flags array (DQF)
;   and set all the points not marked valid to a FillValue.
;   DQF values:  (good_pixel conditionally_usable_pixel out_of_range_pixel no_value_pixel)
;----------------------------------------------------------------------------------------------------

function convert_raw_2_reflectance(infile:file, channel:numeric)
local raw_data, kap0, g16_output_data, dqf_data, is_south_up, temp
begin

    raw_data = short2flt(infile->Rad)
    kap0 = infile->kappa0
    g16_output_data = raw_data * kap0
    dqf_data = toint(byte2flt(infile->DQF))

    g16_output_data = where(dqf_data.eq.3, -1, g16_output_data)
;   g16_output_data = where(dqf_data.eq.2, -1, g16_output_data)
    g16_output_data@_FillValue = -1

    delete(raw_data)
    delete(dqf_data)

    is_south_up = toint(byte2flt(infile->yaw_flip_flag))

    if (is_south_up .eq. 0) then
        temp = g16_output_data
        g16_output_data = temp(::-1,:)
        delete (temp)
    end if

    return (g16_output_data)

end

;----------------------------------------------------------------------------------------------------
;..This function takes the input file from one channel of GOES-16 data and computes brightness
;   temperature from raw radiance data and the planck variables.  It will also check the data quality
;   flags array (DQF) and set all the points not marked valid to a FillValue.
;   DQF values:  (good_pixel conditionally_usable_pixel out_of_range_pixel no_value_pixel)
;----------------------------------------------------------------------------------------------------

function convert_raw_2_brightnessTemp(infile:file, channel:numeric)
local raw_data, g16_output_data, dqf_data, fk1, fk2, bc1, bc2, is_south_up, temp
begin

    raw_data = short2flt(infile->Rad)
    dqf_data = toint(byte2flt(infile->DQF))

    fk1 = infile->planck_fk1
    fk2 = infile->planck_fk2
    bc1 = infile->planck_bc1
    bc2 = infile->planck_bc2
    g16_output_data = (fk2/(log((fk1/(raw_data))+1))-bc1)/bc2  - 273.15

    g16_output_data = where(dqf_data.eq.3, -99, g16_output_data)
    g16_output_data = where(dqf_data.eq.2, -99, g16_output_data)
    g16_output_data@_FillValue = -99

    delete(raw_data)
    delete(dqf_data)
    delete(fk1)
    delete(fk2)
    delete(bc1)
    delete(bc2)

    is_south_up = toint(byte2flt(infile->yaw_flip_flag))

    if (is_south_up .eq. 0) then
        temp = g16_output_data
        g16_output_data = temp(::-1,:)
        delete (temp)
    end if

    return (g16_output_data)

end


;----------------------------------------------------------------------------------------------------
;..This function computes the cosine of the solar zenith angle.  From day-of-year, compute declination
;   angle and equation-of-time, then the time correction given longitude, then local solar time and
;   finally the hour angle.  The solar zenith angle is a function of latitude, declination, and hour
;   angle.  Output is actually the cosine of solar zenith angle.
;----------------------------------------------------------------------------------------------------

function compute_solarZenith(day_number:integer, hour:numeric, mins:numeric, secs:numeric, lat[*][*]:float, lon[*][*]:float)
local PI, d2r, declin, eqnOfTime, localStdTimeMeridian, timeCorrection, localSolarTime, omega, cos_theta
begin

    PI = acos(-1.0D0)
    d2r = PI/180.0D0

    ; From day-of-year, compute declination angle (degrees) and equation of time (mins).

    declin = 23.45 * sin(d2r*(360./365.*(day_number-81)))
    eqnOfTime = 9.87*sin(2*declin*d2r) - 7.53*cos(declin*d2r) -1.5*sin(declin*d2r);

    ; Compute local solar time (including time correction factor) and solar hour angle (omega).

    localStdTimeMeridian = 0.0
    timeCorrection = 4.0*(lon - localStdTimeMeridian) + tofloat(eqnOfTime)
    localSolarTime = (hour*60.0 + mins + secs/60.0 + timeCorrection)/60.0
    omega = 15.0 * (localSolarTime - 12.0)
    omega = where (omega.gt.180., omega-360., omega)

    ; Compute cosine of solar zenith angle.

    cos_theta = sin(declin*d2r)*sin(lat*d2r) + cos(declin*d2r)*cos(lat*d2r)*cos(omega*d2r)

    return (tofloat(cos_theta))

end

;----------------------------------------------------------------------------------------------------
;..Determine if the sun is above horizon in center of the domain.
;----------------------------------------------------------------------------------------------------

function isItDaytime(solar_zenith[*][*]:float)
local true_or_false, d1, nx, ny, ii, jj
begin
    true_or_false = 0
    d1 = dimsizes(solar_zenith)
    nx = d1(1)
    ny = d1(0)
    ii = toint(nx*0.5)
    jj = toint(ny*0.5)

    if (solar_zenith(jj,ii).gt.-0.001) then
        true_or_false = 1
    end if

    return (true_or_false)

end


;----------------------------------------------------------------------------------------------------
;..Create the fixed Lat/Lon grid for satellite projection.  This only needs to be run when the
;   satellite moves to a new position, which is found using the nadir_longitude.  This procedure
;   will write the final lat/lon arrays to a new netCDF file for saving time later.
;----------------------------------------------------------------------------------------------------

procedure create_latlon(infile:file, outfilename:string, region:string, spacing:numeric, latitude[*][*]:float, longitude[*][*]:float)
local PI, dims1, nx, ny, proj, req, rpol, H, l0, geo_ll, g16_nadir_lon, x, y, a, b, c, rs, sx, sy, sz, lat16, lon16, is_south_up, temp, title, fAtt, dimNames, dimSizes, dimUnlim, file_latlon
begin

    if (fileexists(outfilename)) then
        print ("        DEBUG, reading in lat/lon from file " + outfilename)
        file_latlon = addfile(outfilename, "r")
        latitude = file_latlon->latitude
        longitude = file_latlon->longitude
        delete (file_latlon)
    else

    PI = acos(-1.0d0)

    dims1 = dimsizes(latitude)
    nx = dims1(1)
    ny = dims1(0)

    proj = infile->goes_imager_projection
    req  = proj@semi_major_axis
    rpol = proj@semi_minor_axis
    H    = proj@perspective_point_height+proj@semi_major_axis
    l0   = proj@longitude_of_projection_origin

    geo_ll = infile->geospatial_lat_lon_extent
    g16_nadir_lon = geo_ll@geospatial_lon_nadir

    ;..Get (x,y) dimension info and create lat/lon arrays.

    x := short2flt(infile->x)
    y := short2flt(infile->y)

    x := conform_dims(dimsizes(longitude),x,1)
    y := conform_dims(dimsizes(latitude),y,0)

    ;..Trigonometry to get the projection info.

    a = (sin(x))^2+(cos(x))^2*((cos(y))^2+((req^2)/(rpol^2))*((sin(y))^2))
    b = -2*H*cos(x)*cos(y)
    c = (H^2)-(req^2)
    rs = (-b-sqrt((b^2)-(4*a*c)))/(2*a)
    sx = rs*cos(x)*cos(y)
    sy = -rs*sin(x)
    sz = rs*cos(x)*sin(y)

    ;..Compute 2D lat/lon and convert from radians to degrees.

    lat16 = atan(((req^2)/(rpol^2))*(sz/(sqrt(((H-sx)^2)+(sy^2)))))
    lon16 = atan(sy/(H-sx))

    latitude = tofloat(lat16 * 180.0d0/PI)
    longitude = -1.0 * (tofloat(lon16 * 180.0d0/PI) - tofloat(l0))

    if (any(isnan_ieee(latitude))) then
        latitude@_FillValue = 91.0
        replace_ieeenan (latitude, latitude@_FillValue, 0)
    end if

    if (any(isnan_ieee(longitude))) then
        longitude@_FillValue = 361.0
        replace_ieeenan (longitude, longitude@_FillValue, 0)
    end if

    is_south_up = 0
    is_south_up = toint(byte2flt(infile->yaw_flip_flag))

    if (is_south_up .eq. 0) then
        temp = latitude
        latitude = temp(::-1,:)
        delete (temp)
        temp = longitude
        longitude = temp(::-1,:)
        delete (temp)
    end if

    ;..Since if we came here, the file of lat/lon does not exist, we shall create a netCDF file to store the result.

    title = "GOES-16 fixed grid Lat/Lon arrays with spacing " + spacing + "m, for region " + region + " and nadir longitude of " + g16_nadir_lon
    print ("        DEBUG, now creating a new file with title: " + title)
    outfile = addfile (outfilename, "c")
    setfileoption("nc", "Format", "NetCDF4Classic")
    setfileoption("nc", "CompressionLevel", 4)
    setfileoption(outfile, "DefineMode", True)
    fAtt               = True            
    fAtt@title         = title
    fAtt@satellite     = "GOES-16"
    fAtt@nadir_lon     = g16_nadir_lon
    fAtt@region        = region
    fAtt@spacing       = spacing
    fAtt@Conventions   = "None"
    fAtt@creation_date = systemfunc ("date")
    fAtt@creator       = "G. Thompson"
    fAtt@history       = "Created " + fAtt@creation_date
    fAtt@institution   = "NCAR-RAL"
    fileattdef(outfile, fAtt)              
    dimNames = (/"south_north", "west_east"/)
    dimSizes = (/ ny ,   nx /)
    dimUnlim = (/False, False/)
    filedimdef(outfile, dimNames, dimSizes, dimUnlim)

    filevardef(outfile, "latitude",  typeof(latitude), (/"south_north","west_east"/))
    filevardef(outfile, "longitude", typeof(longitude), (/"south_north","west_east"/))

    filevarattdef(outfile, "latitude",  latitude)
    filevarattdef(outfile, "longitude", longitude)

    outfile->latitude  = (/latitude/)
    outfile->longitude = (/longitude/)

    delete (outfile)

    end if

end


;----------------------------------------------------------------------------------------------------
;..Convert original source grid to a SCRIP convention file for ESMF-regridder usage later.
;----------------------------------------------------------------------------------------------------

procedure create_file_SCRIP(srcGridName1:string, region:string, spacing:numeric, nadir_longitude:numeric, input_lat[*][*]:float, input_lon[*][*]:float)
local Opt, local_lat, local_lon
begin

    if (.not.(isfilepresent(srcGridName1))) then
        local_lat = input_lat
        local_lon = input_lon + 360.0

        Opt                = True
        Opt@Mask2D         = where(.not.ismissing(input_lat).and..not.ismissing(input_lon),1,0)
        ; Opt@Mask2D         = where((input_lat.gt.-89.999.and.input_lat.lt.89.999).and.(input_lon.gt.-179.999.and.input_lon.lt.179.999),1,0)
        Opt@ForceOverwrite = True
        Opt@PrintTimings   = True
        Opt@Title          = "NCAR-RAL GOES-16 (real-time) Grid for region " + region + " and nadir longitude " + nadir_longitude + " and " + spacing + "m spacing"
        Opt@NetCDFType     = "netcdf4"

        print ("   running curvilinear_to_SCRIP to create the file: " + srcGridName1)
        curvilinear_to_SCRIP(srcGridName1, local_lat, local_lon, Opt)

        ;---Clean up
        delete(Opt)
        delete(local_lat)
        delete(local_lon)
    end if

end

;----------------------------------------------------------------------------------------------------
;..Compute the weights to go between the source SCRIP file to destination SCRIP file.
;----------------------------------------------------------------------------------------------------

procedure create_file_WGT(srcGridName:string, dstGridName:string, wgtFileName:string, region:string, spacing:numeric, nadir_longitude:numeric)
local Opt
begin

    if (.not.(isfilepresent(wgtFileName))) then
        Opt                = True
        Opt@Debug          = True
        Opt@InterpMethod   = "bilinear"     ; default
        Opt@missing_value  = -99.
        Opt@ForceOverwrite = True
        Opt@PrintTimings   = True
        Opt@SrcRegional    = True
        Opt@DstRegional    = True
        Opt@Title          = "NCAR-RAL GOES-16 (real-time) ESMF weights for region " + region + " and nadir longitude " + nadir_longitude + " and " + spacing + "m spacing"
        Opt@NetCDFType     = "netcdf4"

        print ("   running ESMF_regrid_gen_weights to create the file: " + wgtFileName + " using source/destination SCRIP files: " + srcGridName + " and " + dstGridName)
        ESMF_regrid_gen_weights(srcGridName, dstGridName, wgtFileName, Opt)

        ;---Clean up
        delete(Opt)
    end if

end


;----------------------------------------------------------------------------------------------------
;..Compute the shortwave IR reflectance from Channel 7.
;   Origins: Kaufman and Nakajima (1993JAM, eq.3); Turk et al (1998JAM); Lindsey et al (2006MWR); Lensky and Rosenfeld (2008ACP).
;----------------------------------------------------------------------------------------------------

function compute_swir_reflect(infile:file, g16_ch14[*][*]:float, sun_zenith[*][*]:float, channel:numeric)
local PI, arf, radiance, reflect, c1, c2, solar_f0, nu, btemp, emission, top, bottom, esun, d, kap0, temp, is_south_up
begin

    PI = acos(-1.0D0)
    arf = 1.0D0

    radiance = short2flt(infile->Rad)
    reflect = radiance*0.0

    is_south_up = toint(byte2flt(infile->yaw_flip_flag))

    if (is_south_up .eq. 0) then
        temp = radiance
        radiance = temp(::-1,:)
        delete (temp)
    end if

    ; Planck function constants

    c1 = 1.191066D-5                     ; mW m-2 sr-1 (cm-1)-4
    c2 = 1.438833D0                      ; (cm-1)-1

    if (ismissing(infile->esun)) then    ; Solar constant at central wavelength in W m^-2 um^-1  (eqv. mW m-2 cm-1)
        if (channel .eq. 7) then         ;  According to:  http://rredc.nrel.gov/solar/spectra/am0/ASTM2000.html
            esun = 9.6                   ;   for a central wavelength of 3.9 microns, this is the Solar flux.
        else
            if (channel .eq. 6) then
                esun = 79.2              ;   for a central wavelength of 2.25 microns, this is the Solar flux.
            end if
        end if
    else
        esun = infile->esun
    end if
    solar_f0 = esun / PI                 ; Convert solar flux to proper units mW m-2 sr-1 cm-1

    if (channel .eq. 7) then
        nu = 2570.373D0                  ; Central wavenumber of channel 7 (3.9 micron).
    else
        if (channel .eq. 6) then
            nu = 4459.441D0              ; Central wavenumber of channel 6 (2.25 micron).
        else
            print ("Currently only channels 6 and 7 are supported.")
        end if
    end if

    btemp = g16_ch14 + 273.15            ; Calculate Planck emission from longwave IR brightness temp.
    emission = c1*nu*nu*nu/(exp(c2*nu/btemp) - 1.0)

    top = tofloat(radiance - emission)
    bottom = tofloat(solar_f0*sun_zenith*arf - emission)

    top = where( (sun_zenith.lt.1.E-3 .or. g16_ch14.lt.-90.0 .or. top.le.0), 0.01, top)
    bottom = where( (sun_zenith.lt.1.E-3 .or. g16_ch14.lt.-90.0 .or. bottom.le.0), 0.01, bottom)

    reflect = where( (top.gt.0.01 .and. bottom.gt.0.01), top/bottom, 1.E-3)
    reflect = where(reflect.gt.0.95, 0.95, reflect)

    if (ismissing(infile->kappa0)) then
        d = infile->earth_sun_distance_anomaly_in_AU
        kap0 = PI * d*d / esun
    else
        kap0 = infile->kappa0
    end if
;   reflect = radiance * kap0

    return (reflect)

end
