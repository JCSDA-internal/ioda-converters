program Goes_ReBroadcast_converter
!
! Purpose: Convert GOES ReBroadcast netCDF files to ioda-v1 format.
!          Currently only processes bands 7-16.
!
! input files:
!    (1) flist.txt: contains a list of nc files (exclude path) to be processed
!                     GoesReBroadcast file
!                     (optional) Clear Sky Mask output of cspp-geo-aitf package
!    (2) namelist.goes_abi_converter
!        &data_nml
!          nc_list_file = 'flist.txt'
!          data_dir = '/data/goes',         ! path of the GRB nc files
!          data_id = 'OR_ABI-L1b-RadC-M3'   ! prefix of the downloaded GRB nc files
!          sat_id = 'G16'
!          n_subsample = 1
!        /

   use netcdf_mod, only: open_netcdf_for_write, close_netcdf, &
      def_netcdf_dims, def_netcdf_var, def_netcdf_end, &
      put_netcdf_var, missing_r

   implicit none
   include 'netcdf.inc'

   integer, parameter  :: r_single = selected_real_kind(6)  ! single precision
   integer, parameter  :: r_double = selected_real_kind(15) ! double precision
   integer, parameter  :: i_byte   = selected_int_kind(1)   ! byte integer
   integer, parameter  :: i_short  = selected_int_kind(4)   ! short integer
   integer, parameter  :: i_long   = selected_int_kind(8)   ! long integer
   integer, parameter  :: i_kind   = i_long                 ! default integer
   integer, parameter  :: r_kind   = r_single               ! default real

   ! prefix of Clear Sky Mask (Binary Cloud Mask) output of cspp-geo-aitf package
   character(len=14), parameter :: BCM_id = 'CG_ABI-L2-ACMC'

   integer(i_kind), parameter :: nband      = 10  ! IR bands 7-16
   integer(i_kind) :: band_start = 7
   integer(i_kind) :: band_end   = 16

   real(r_kind) :: pi, deg2rad, rad2deg

   logical, allocatable :: got_latlon(:,:)
   real(r_kind), allocatable :: glat(:,:)    ! grid latitude (nx,ny)
   real(r_kind), allocatable :: glon(:,:)    ! grid longitude (nx,ny)
   real(r_kind), allocatable :: gzen(:,:)    ! satellite zenith angle (nx,ny)
   real(r_kind), allocatable :: solzen(:,:)  ! solar zenith angle (nx,ny)

   real(r_kind),    allocatable :: rad_2d(:,:)  ! radiance(nx,ny)
   real(r_kind),    allocatable :: bt_2d(:,:)   ! brightness temperature(nx,ny)
   integer(i_kind), allocatable :: qf_2d(:,:)   ! quality flag(nx,ny)
            ! qf (DQF, Data Quality Flag)
            ! 0:good, 1:conditionally_usable, 2:out_of_range, 3:no_value
   integer(i_kind), allocatable :: cm_2d(:,:)   ! cloud_mask(nx,ny)

   type rad_type
      real(r_kind),    allocatable :: rad(:,:,:)  ! radiance(nband,nx,ny)
      real(r_kind),    allocatable :: bt(:,:,:)   ! brightness temperature(nband,nx,ny)
      integer(i_kind), allocatable :: qf(:,:,:)   ! quality flag(nband,nx,ny)
      real(r_kind),    allocatable :: sd(:)       ! std_dev(nband)
      integer(i_kind), allocatable :: cm(:,:)     ! cloud mask(nx,ny)
   end type rad_type
   type(rad_type), allocatable  :: rdata(:)  ! (ntime)

   character(len=22), allocatable :: time_start(:)  ! (ntime) 2017-10-01T18:02:19.6Z

   integer(i_kind) :: ncid, nf_status
   integer(i_kind) :: nx, ny
   integer(i_kind) :: it, ib, ii, i, j
   integer(i_kind) :: ntime
   integer(i_kind) :: t_index
   integer(i_kind) :: band_id

   integer(i_kind)      :: nml_unit = 81
   integer(i_kind)      :: iunit    = 87

   character(len=256)              :: nc_list_file  ! the text file that contains a list of netcdf files to process
   character(len=256)              :: data_dir
   character(len=18)               :: data_id
   character(len=3)                :: sat_id
   integer(i_kind)                 :: n_subsample
   logical                         :: write_iodav1

   namelist /data_nml/ nc_list_file, data_dir, data_id, sat_id, n_subsample

   real(r_kind)                    :: sdtb ! to be done
   integer(i_kind)                 :: istat
   integer(i_kind)                 :: nfile, ifile, nlen
   logical                         :: isfile
   logical                         :: found_time
   logical                         :: got_grid_info
   logical, allocatable            :: valid(:), is_BCM(:)
   character(len=256), allocatable :: nc_fnames(:)
   character(len=256)              :: fname
   character(len=256)              :: out_fname
   character(len=256)              :: txtbuf
   character(len=18)               :: finfo
   character(len=2)                :: mode_id, scan_mode
   character(len=3)                :: fsat_id
   character(len=22), allocatable  :: scan_time(:) ! 2017-10-01T18:02:19.6Z
   integer(i_kind),   allocatable  :: fband_id(:)
   integer(i_kind),   allocatable  :: ftime_id(:)
   integer(i_kind),   allocatable  :: julianday(:)

   continue

   pi = acos(-1.0)
   deg2rad = pi/180.0
   rad2deg = 1.0/deg2rad
   !
   ! initialize namelist variables
   !
   nc_list_file   = 'flist.txt'
   data_dir       = '.'
   data_id        = 'OR_ABI-L1b-RadC-M3'
   sat_id         = 'G16'
   n_subsample    = 1
   write_iodav1   = .true.
   !
   ! read namelist
   !
   open(unit=nml_unit, file='namelist.goes_abi_converter', status='old', form='formatted')
   read(unit=nml_unit, nml=data_nml, iostat=istat)
   write(0,nml=data_nml)
   if ( istat /= 0 ) then
      write(0,*) 'Error reading namelist data_nml'
      stop
   end if

   ! get file names from nc_list_file
   nfile  = 0  ! initialize the number of netcdf files to read
   inquire(file=trim(nc_list_file), exist=isfile)
   if ( .not. isfile ) then
      write(0,*) 'File not found: nc_list_file '//trim(nc_list_file)
      stop
   else
      open(unit=iunit, file=trim(nc_list_file), status='old', form='formatted')
      !first find out the number of netcdf files to read
      istat = 0
      do while ( istat == 0 )
         read(unit=iunit, fmt='(a)', iostat=istat) txtbuf
         if ( istat /= 0 ) then
            exit
         else
            nfile = nfile + 1
         end if
      end do
      if ( nfile > 0 ) then
         allocate (nc_fnames(nfile))
         !read the nc_list_file again to get the netcdf file names
         rewind(iunit)
         do ifile = 1, nfile
            read(unit=iunit, fmt='(a)', iostat=istat) nc_fnames(ifile)
         end do
      else
         write(0,*) 'File not found from nc_list_file '//trim(nc_list_file)
         stop
      end if
      close(iunit)
   end if !nc_list_file

   allocate (ftime_id(nfile))
   allocate (scan_time(nfile))
   allocate (julianday(nfile))
   allocate (fband_id(nfile))
   allocate (valid(nfile))
   allocate (is_BCM(nfile))
   valid( :) = .false.
   is_BCM(:) = .false.

   nlen = len_trim(data_id)
   mode_id = data_id(nlen-1:nlen)

   ! parse the file list
   t_index = 0
   file_loop1: do ifile = 1, nfile

      fname = trim(data_dir)//'/'//trim(nc_fnames(ifile))
      inquire(file=trim(fname), exist=isfile)
      if ( .not. isfile ) then
         write(0,*) 'File not found: '//trim(fname)
         cycle file_loop1
      end if

      ! OR_ABI-L1b-RadC-M3C16_G16_s20172741802196_e20172741804580_c20172741805015.nc
      ! retrieve some basic info from the netcdf filename itself
      call decode_nc_fname(trim(nc_fnames(ifile)),finfo, scan_mode, is_BCM(ifile), &
         fband_id(ifile), fsat_id, scan_time(ifile), julianday(ifile))

      ! all files must be the same mode
      if ( scan_mode /= mode_id ) then
         cycle file_loop1
      end if

      if ( fsat_id /= sat_id ) then
         cycle file_loop1
      end if

      if ( .not. is_BCM(ifile) ) then
         ! id of the file name must match specified data_id
         if ( finfo /= data_id ) then
            cycle file_loop1
         else
            ! only process band 7-16
            if ( fband_id(ifile) < band_start .or. fband_id(ifile) > band_end ) then
               cycle file_loop1
            end if
         end if
      end if

      valid(ifile) = .true.

      ! group files of the same scan time
      if ( t_index == 0 ) then
         t_index = t_index + 1
         ftime_id(ifile) = t_index
      else
         found_time = .false.
         find_time_loop: do ii = ifile-1, 1, -1
            if ( valid(ii) ) then
               if ( scan_time(ifile) == scan_time(ii) ) then
                  ftime_id(ifile) = ftime_id(ii)
                  found_time = .true.
                  exit find_time_loop
               end if
            end if
         end do find_time_loop
         if ( .not. found_time ) then
            t_index = t_index + 1
            ftime_id(ifile) = t_index
         end if
      end if

      ntime = t_index

   end do file_loop1

   if ( ntime <= 0 ) then
      write(0,*) 'ntime = ', ntime
      write(0,*) 'No valid files found from nc_list_file '//trim(nc_list_file)
      stop
   end if

   allocate (time_start(ntime))
   allocate (rdata(ntime))

   got_grid_info = .false.
   file_loop2: do ifile = 1, nfile

      if ( valid(ifile) ) then

         fname = trim(data_dir)//'/'//trim(nc_fnames(ifile))
         nf_status = nf_OPEN(trim(fname), nf_NOWRITE, ncid)
         if ( nf_status == 0 ) then
            write(0,*) 'Reading '//trim(fname)
         else
            write(0,*) 'ERROR reading '//trim(fname)
            cycle file_loop2
         end if

         if ( .not. got_grid_info ) then
            call read_GRB_dims(ncid, nx, ny)
            allocate (glat(nx, ny))
            allocate (glon(nx, ny))
            allocate (gzen(nx, ny))
            allocate (solzen(nx, ny))
            allocate (got_latlon(nx, ny))
            glat(:,:) = missing_r
            glon(:,:) = missing_r
            gzen(:,:) = missing_r
            solzen(:,:) = missing_r
            write(0,*) 'Calculating lat/lon from fixed grid x/y...'
            call read_GRB_grid(ncid, nx, ny, glat, glon, gzen, got_latlon)
            call calc_solar_zenith_angle(nx, ny, glat, glon, scan_time(ifile), julianday(ifile), solzen, got_latlon)
            got_grid_info = .true.
            allocate (rad_2d(nx, ny))
            allocate (bt_2d(nx, ny))
            allocate (qf_2d(nx, ny))
            allocate (cm_2d(nx, ny))
         end if

         it = ftime_id(ifile)
         ib = fband_id(ifile)

         if ( .not. is_BCM(ifile) ) then

            call read_GRB(ncid, nx, ny, rad_2d, bt_2d, qf_2d, sdtb, band_id, time_start(it))

            if ( band_id /= ib ) then
               write(0,*) 'ERROR: band_id from the file name and the file content do not match.'
               cycle file_loop2
            end if

            if ( time_start(it) /= scan_time(ifile) ) then
               write(0,*) 'ERROR: scan start time from the file name and the file content do not match.'
               cycle file_loop2
            end if

            if ( .not. allocated(rdata(it)%rad) ) allocate (rdata(it)%rad(nband,nx,ny))
            if ( .not. allocated(rdata(it)%bt) )  allocate (rdata(it)%bt(nband,nx,ny))
            if ( .not. allocated(rdata(it)%qf) )  allocate (rdata(it)%qf(nband,nx,ny))
            if ( .not. allocated(rdata(it)%sd) )  allocate (rdata(it)%sd(nband))

            do j = 1, ny
               do i = 1, nx
                  ! convert band id 7-16 to array index 1-10
                  rdata(it)%rad(ib-band_start+1,i,j) = rad_2d(i,j)
                  rdata(it)%bt(ib-band_start+1,i,j)  = bt_2d(i,j)
                  rdata(it)%qf(ib-band_start+1,i,j)  = qf_2d(i,j)
                  rdata(it)%sd(ib-band_start+1)      = sdtb
               end do
            end do

         else

            call read_L2_BCM(ncid, nx, ny, cm_2d, time_start(it))

            if ( time_start(it) /= scan_time(ifile) ) then
               write(0,*) 'ERROR: scan start time from the file name and the file content do not match.'
               cycle file_loop2
            end if

            if ( .not. allocated(rdata(it)%cm) )  allocate (rdata(it)%cm(nx,ny))
            rdata(it)%cm(:,:) = cm_2d(:,:)

         end if

         nf_status = nf_CLOSE(ncid)

      end if

   end do file_loop2

   if ( allocated(rad_2d) ) deallocate(rad_2d)
   if ( allocated(bt_2d) )  deallocate(bt_2d)
   if ( allocated(qf_2d) )  deallocate(qf_2d)
   if ( allocated(cm_2d) )  deallocate(cm_2d)

   if ( write_iodav1 ) then
      do it = 1, ntime
         out_fname = trim(data_id)//'_'//sat_id//'_'//time_start(it)//'.nc4'
         write(0,*) 'Writing ', trim(out_fname)
         if ( allocated(rdata(it)%cm) ) then
            call output_iodav1(trim(out_fname), time_start(it), nx, ny, nband, got_latlon, &
               glat, glon, gzen, solzen, rdata(it)%bt, rdata(it)%qf, rdata(it)%sd, rdata(it)%cm)
         else
            call output_iodav1(trim(out_fname), time_start(it), nx, ny, nband, got_latlon, &
               glat, glon, gzen, solzen, rdata(it)%bt, rdata(it)%qf, rdata(it)%sd)
         end if
      end do
   end if

   if ( allocated(glat) )   deallocate(glat)
   if ( allocated(glon) )   deallocate(glon)
   if ( allocated(gzen) )   deallocate(gzen)
   if ( allocated(solzen) ) deallocate(solzen)

   do it = 1, ntime
      if ( allocated(rdata(it)%rad) ) deallocate (rdata(it)%rad)
      if ( allocated(rdata(it)%bt)  ) deallocate (rdata(it)%bt)
      if ( allocated(rdata(it)%qf)  ) deallocate (rdata(it)%qf)
      if ( allocated(rdata(it)%cm)  ) deallocate (rdata(it)%cm)
   end do
   deallocate(rdata)
   deallocate(time_start)

   deallocate(nc_fnames)
   deallocate(ftime_id)
   deallocate(scan_time)
   deallocate(julianday)
   deallocate(fband_id)
   deallocate(valid)
   deallocate(is_BCM)

contains

subroutine read_GRB_dims(ncid, nx, ny)
   implicit none
   integer(i_kind), intent(in)  :: ncid
   integer(i_kind), intent(out) :: nx, ny
   integer(i_kind)              :: dimid
   integer(i_kind)              :: nf_status(4)
   continue
   nf_status(1) = nf_INQ_DIMID(ncid, 'x', dimid)
   nf_status(2) = nf_INQ_DIMLEN(ncid, dimid, nx)
   nf_status(3) = nf_INQ_DIMID(ncid, 'y', dimid)
   nf_status(4) = nf_INQ_DIMLEN(ncid, dimid, ny)
   if ( any(nf_status /= 0) ) then
      write(0,*) 'Error reading dimensions'
      stop
   end if
   return
end subroutine read_GRB_dims

!NC_BYTE 8-bit signed integer
!NC_SHORT 16-bit signed integer
!NC_INT (or NC_LONG) 32-bit signed integer
!NC_FLOAT 32-bit floating point
!NC_DOUBLE 64-bit floating point

subroutine read_GRB_grid(ncid, nx, ny, glat, glon, gzen, got_latlon)
   implicit none
   integer(i_kind), intent(in)    :: ncid
   integer(i_kind), intent(in)    :: nx, ny
   real(r_kind),    intent(inout) :: glat(nx,ny)
   real(r_kind),    intent(inout) :: glon(nx,ny)
   real(r_kind),    intent(inout) :: gzen(nx,ny)
   logical,         intent(inout) :: got_latlon(nx,ny)
   integer(i_kind)                :: varid, i, j
   integer(i_kind)                :: nf_status
   integer(i_kind)                :: istart(1), icount(1)
   integer(i_short), allocatable  :: itmp_short_1d(:)
   real(r_kind),     allocatable  :: x(:)
   real(r_kind),     allocatable  :: y(:)
   real(r_single) :: scalef, offset
   real(r_double) :: dtmp
   real(r_double) :: r_eq    ! GRS80 semi-major axis of earth
   real(r_double) :: r_pol   ! GRS80 semi-minor axis of earth = (1-f)*r_eq
   real(r_double) :: lon_sat ! satellite longitude, longitude_of_projection_origin
   real(r_double) :: h_sat   ! satellite height
   real(r_double) :: a, b, c, rs, sx, sy, sz
   real(r_kind)   :: rlat, rlon, lon_diff, tmp1, theta1, theta2
   continue

!int goes_imager_projection ;
!  goes_imager_projection:long_name = "GOES-R ABI fixed grid projection" ;
!  goes_imager_projection:grid_mapping_name = "geostationary" ;
!  goes_imager_projection:perspective_point_height = 35786023. ;
!  goes_imager_projection:semi_major_axis = 6378137. ;
!  goes_imager_projection:semi_minor_axis = 6356752.31414 ;
!  goes_imager_projection:inverse_flattening = 298.2572221 ;
!  goes_imager_projection:latitude_of_projection_origin = 0. ;
!  goes_imager_projection:longitude_of_projection_origin = -89.5 ;
!  goes_imager_projection:sweep_angle_axis = "x" ;

   nf_status = nf_INQ_VARID(ncid, 'goes_imager_projection', varid)
   nf_status = nf_GET_ATT_DOUBLE(ncid, varid, 'semi_major_axis',  dtmp)
   r_eq = dtmp
   nf_status = nf_GET_ATT_DOUBLE(ncid, varid, 'semi_minor_axis',  dtmp)
   r_pol = dtmp
   nf_status = nf_GET_ATT_DOUBLE(ncid, varid, 'perspective_point_height',  dtmp)
   h_sat = dtmp + r_eq  ! perspective_point_height + semi_major_axis
   nf_status = nf_GET_ATT_DOUBLE(ncid, varid, 'longitude_of_projection_origin',  dtmp)
   lon_sat = dtmp * deg2rad

!short x(x) ;
!  x:scale_factor = 5.6e-05f ;
!  x:add_offset = -0.075012f ;
!  x:units = "rad" ;
!  x:axis = "X" ;
!  x:long_name = "GOES fixed grid projection x-coordinate" ;

   istart(1) = 1
   icount(1) = nx
   allocate(itmp_short_1d(nx))
   nf_status = nf_INQ_VARID(ncid, 'x', varid)
   nf_status = nf_GET_VARA_INT2(ncid, varid, istart(1:1), icount(1:1), itmp_short_1d(:))
   nf_status = nf_GET_ATT_REAL(ncid, varid, 'scale_factor', scalef)
   nf_status = nf_GET_ATT_REAL(ncid, varid, 'add_offset', offset)
   allocate(x(nx))
   do i = 1, nx
      x(i) = offset + itmp_short_1d(i) * scalef
   end do
   deallocate(itmp_short_1d)

!short y(y) ;
!  y:scale_factor = -5.6e-05f ;
!  y:add_offset = 0.126532f ;
!  y:units = "rad" ;
!  y:axis = "Y" ;
!  y:long_name = "GOES fixed grid projection y-coordinate" ;
!  y:standard_name = "projection_y_coordinate" ;

   istart(1) = 1
   icount(1) = ny
   allocate(itmp_short_1d(ny))
   nf_status = nf_INQ_VARID(ncid, 'y', varid)
   nf_status = nf_GET_VARA_INT2(ncid, varid, istart(1:1), icount(1:1), itmp_short_1d(:))
   nf_status = nf_GET_ATT_REAL(ncid, varid, 'scale_factor', scalef)
   nf_status = nf_GET_ATT_REAL(ncid, varid, 'add_offset', offset)
   allocate(y(ny))
   do i = 1, ny
      y(i) = offset + itmp_short_1d(i) * scalef
   end do
   deallocate(itmp_short_1d)
   ! Product Definition and User's Guide (PUG) Volume 3, pp. 19-21
   ! from fixed grid x/y to geodetic lat/lon
   got_latlon(1:nx,1:ny) = .true.
   do j = 1, ny
      do i = 1, nx
         a = sin(x(i))*sin(x(i)) + cos(x(i))*cos(x(i)) * &
             (cos(y(j))*cos(y(j))+(r_eq/r_pol)*(r_eq/r_pol)*sin(y(j))*sin(y(j)))
         b = -2.0*h_sat*cos(x(i))*cos(y(j))
         c = h_sat*h_sat - r_eq*r_eq
         if ( (b*b-4.0*a*c) < 0.0 ) then
            got_latlon(i,j) = .false.
            cycle
         end if
         rs = (-1.0*b - sqrt(b*b-4.0*a*c)) / (2.0*a)
         sx = rs * cos(x(i)) * cos(y(j))
         sy = -1.0 * rs * sin(x(i))
         sz = rs * cos(x(i)) * sin(y(j))
         !glat(i,j) = (atan((r_eq/r_pol)*(r_eq/r_pol)*(sz/sqrt((h_sat-sx)*(h_sat-sx)+sy*sy)))) * rad2deg
         !glon(i,j) = (lon_sat - atan(sy/(h_sat-sx))) * rad2deg
         glat(i,j) = atan((r_eq/r_pol)*(r_eq/r_pol)*(sz/sqrt((h_sat-sx)*(h_sat-sx)+sy*sy)))
         glon(i,j) = lon_sat - atan(sy/(h_sat-sx))
      end do
   end do

   deallocate(x)
   deallocate(y)

   ! calculate geostationary satellite zenith angle
   do j = 1, ny
      do i = 1, nx
         if ( .not. got_latlon(i,j) ) cycle
         rlat = glat(i,j) ! in radian
         rlon = glon(i,j) ! in radian
         lon_diff = abs(rlon-lon_sat)
!         tmp1 = sqrt((2.0*r_eq*sin(lon_diff/2.)-r_eq*(1.0-cos(rlat))*sin(lon_diff/2.))**2 &
!           +(2.0*r_eq*sin(rlat/2.))**2-(r_eq*(1.0-cos(rlat))*sin(lon_diff/2.))**2)
         tmp1 = (2.0*r_eq*sin(lon_diff/2.)-r_eq*(1.0-cos(rlat))*sin(lon_diff/2.))**2 &
           +(2.0*r_eq*sin(rlat/2.))**2-(r_eq*(1.0-cos(rlat))*sin(lon_diff/2.))**2
         if ( tmp1 < 0.0 ) cycle
         tmp1 = sqrt(tmp1)
         theta1 = 2.0*asin(tmp1/r_eq/2.)
         theta2 = atan(r_eq*sin(theta1)/((h_sat-r_eq)+r_eq*(1.0-sin(theta1))))
         gzen(i,j) = (theta1+theta2) * rad2deg
         !gzen(i,j) = 90.0 - atan((cos(lon_diff)*cos(rlat)-0.1512)/(sqrt(1.0-cos(lon_diff)*cos(lon_diff)*cos(rlat)*cos(rlat)))) * rad2deg
         glat(i,j) = glat(i,j) * rad2deg
         glon(i,j) = glon(i,j) * rad2deg
      end do
   end do

   return
end subroutine read_GRB_grid

subroutine read_GRB(ncid, nx, ny, rad, bt, qf, sd, band_id, time_start)
   implicit none
   integer(i_kind),   intent(in)    :: ncid
   integer(i_kind),   intent(in)    :: nx, ny
   integer(i_kind),   intent(out)   :: band_id
   real(r_kind),      intent(out)   :: sd
   real(r_kind),      intent(inout) :: rad(nx,ny)
   real(r_kind),      intent(inout) :: bt(nx,ny)
   integer(i_kind),   intent(inout) :: qf(nx,ny)
   character(len=22), intent(out)   :: time_start  ! 2017-10-01T18:02:19.6Z
   integer(i_byte),  allocatable    :: itmp_byte_1d(:)
   integer(i_byte),  allocatable    :: itmp_byte_2d(:,:)
   integer(i_short), allocatable    :: itmp_short_2d(:,:)
   integer(i_kind)                  :: nf_status
   integer(i_kind)                  :: istart(2), icount(2)
   integer(i_kind)                  :: varid, i, j
   integer(i_short)                 :: ifill
   real(r_single)                   :: rfill
   real(r_single)                   :: rtmp
   real(r_single)                   :: planck_fk1, planck_fk2
   real(r_single)                   :: planck_bc1, planck_bc2
   real(r_single)                   :: scalef, offset
   real(r_kind)                     :: rmiss = -999.0
   integer(i_kind)                  :: imiss = -999
   continue

   ! time_start is the same for all bands, but time_end is not
   nf_status = nf_GET_ATT_TEXT(ncid, nf_GLOBAL, 'time_coverage_start', time_start)
   !nf_status = nf_GET_ATT_TEXT(ncid, nf_GLOBAL, 'time_coverage_end',   time_end)

   istart(1) = 1
   icount(1) = 1
   allocate(itmp_byte_1d(1))
   nf_status = nf_INQ_VARID(ncid, 'band_id', varid)
   nf_status = nf_GET_VARA_INT1(ncid, varid, istart(1:1), icount(1:1), itmp_byte_1d(:))
   band_id = itmp_byte_1d(1)
   deallocate(itmp_byte_1d)

   nf_status = nf_INQ_VARID(ncid, 'std_dev_radiance_value_of_valid_pixels', varid)
   nf_status = nf_GET_VAR_REAL(ncid, varid, rtmp)
   sd = rtmp

   ! qf (DQF, Data Quality Flag)
   ! 0:good, 1:conditionally_usable, 2:out_of_range, 3:no_value
   istart(1) = 1
   icount(1) = nx
   istart(2) = 1
   icount(2) = ny
   allocate(itmp_byte_2d(nx,ny))
   nf_status = nf_INQ_VARID(ncid, 'DQF', varid)
   nf_status = nf_GET_VARA_INT1(ncid, varid, istart(1:2), icount(1:2), itmp_byte_2d(:,:))
   qf(:,:) = imiss
   do j = 1, ny
      do i = 1, nx
         qf(i,j) = itmp_byte_2d(i,j)
      end do
   end do
   deallocate(itmp_byte_2d)

   nf_status = nf_INQ_VARID(ncid, 'planck_fk1', varid)
   nf_status = nf_GET_VAR_REAL(ncid, varid, planck_fk1)
   nf_status = nf_GET_ATT_REAL(ncid, varid, '_FillValue',  rfill)

   nf_status = nf_INQ_VARID(ncid, 'planck_fk2', varid)
   nf_status = nf_GET_VAR_REAL(ncid, varid, planck_fk2)

   nf_status = nf_INQ_VARID(ncid, 'planck_bc1', varid)
   nf_status = nf_GET_VAR_REAL(ncid, varid, planck_bc1)

   nf_status = nf_INQ_VARID(ncid, 'planck_bc2', varid)
   nf_status = nf_GET_VAR_REAL(ncid, varid, planck_bc2)

   istart(1) = 1
   icount(1) = nx
   istart(2) = 1
   icount(2) = ny
   allocate(itmp_short_2d(nx, ny))
   nf_status = nf_INQ_VARID(ncid, 'Rad', varid)
   nf_status = nf_GET_VARA_INT2(ncid, varid, istart(1:2), icount(1:2), itmp_short_2d(:,:))
   nf_status = nf_GET_ATT_INT2(ncid, varid, '_FillValue',  ifill)
   nf_status = nf_GET_ATT_REAL(ncid, varid, 'scale_factor', scalef)
   nf_status = nf_GET_ATT_REAL(ncid, varid, 'add_offset', offset)
   rad(:,:) = rmiss
   bt(:,:)  = rmiss
   do j = 1, ny
      do i = 1, nx
         if ( itmp_short_2d(i,j) /= ifill ) then
            rad(i,j) = offset + itmp_short_2d(i,j) * scalef
            if ( planck_fk1 /= rfill .and. planck_fk2 /= rfill .and. &
                 planck_bc1 /= rfill .and. planck_bc2 /= rfill ) then
              if ( rad(i,j) > 0.0 ) then
               bt(i,j) = (planck_fk2/(log((planck_fk1/rad(i,j))+1.0))-planck_bc1)/planck_bc2
              end if
            end if
         end if
      end do
   end do
   deallocate(itmp_short_2d)

   return
end subroutine read_GRB

subroutine read_L2_BCM(ncid, nx, ny, cm, time_start)
   implicit none
   integer(i_kind),   intent(in)    :: ncid
   integer(i_kind),   intent(in)    :: nx, ny
   integer(i_kind),   intent(inout) :: cm(nx,ny)
   character(len=22), intent(out)   :: time_start  ! 2017-10-01T18:02:19.6Z
   integer(i_byte),  allocatable    :: itmp_byte_2d(:,:)
   integer(i_kind)                  :: nf_status
   integer(i_kind)                  :: istart(2), icount(2)
   integer(i_kind)                  :: varid, i, j
   integer(i_kind)                  :: imiss = -999
   integer(i_kind)                  :: qf(nx,ny)
   continue

   ! time_start is the same for all bands, but time_end is not
   nf_status = nf_GET_ATT_TEXT(ncid, nf_GLOBAL, 'time_coverage_start', time_start)
   !nf_status = nf_GET_ATT_TEXT(ncid, nf_GLOBAL, 'time_coverage_end',   time_end)

   istart(1) = 1
   icount(1) = nx
   istart(2) = 1
   icount(2) = ny
   allocate(itmp_byte_2d(nx,ny))
   nf_status = nf_INQ_VARID(ncid, 'DQF', varid)
   nf_status = nf_GET_VARA_INT1(ncid, varid, istart(1:2), icount(1:2), itmp_byte_2d(:,:))
   qf(:,:) = imiss
   do j = 1, ny
      do i = 1, nx
         qf(i,j) = itmp_byte_2d(i,j)
      end do
   end do
   deallocate(itmp_byte_2d)

   istart(1) = 1
   icount(1) = nx
   istart(2) = 1
   icount(2) = ny
   allocate(itmp_byte_2d(nx,ny))
   nf_status = nf_INQ_VARID(ncid, 'BCM', varid)
   nf_status = nf_GET_VARA_INT1(ncid, varid, istart(1:2), icount(1:2), itmp_byte_2d(:,:))
   cm(:,:) = imiss
   do j = 1, ny
      do i = 1, nx
         if ( qf(i,j) == 0 ) then ! good quality
            cm(i,j) = itmp_byte_2d(i,j)
         end if
      end do
   end do
   deallocate(itmp_byte_2d)

   return
end subroutine read_L2_BCM

subroutine decode_nc_fname(fname, finfo, scan_mode, is_BCM, band_id, sat_id, start_time, jday)
   implicit none
   character(len=*),  intent(in)  :: fname
   character(len=18), intent(out) :: finfo
   character(len=2),  intent(out) :: scan_mode
   logical,           intent(out) :: is_BCM
   integer(i_kind),   intent(out) :: band_id
   character(len=3),  intent(out) :: sat_id
   character(len=22), intent(out) :: start_time
   integer(i_kind),   intent(out) :: jday
   integer(i_kind) :: year, month, day, hour, minute, sec1, sec2

   if ( fname( 1:14) == BCM_id ) then
      is_BCM = .true.
      band_id = -99
   else
      is_BCM = .false.
   end if
   !CG_ABI-L2-ACMC-M3_G16_s20180351202275_e20180351205060_c20180351205106.nc
   !OR_ABI-L1b-RadC-M3C16_G16_s20172741802196_e20172741804580_c20172741805015.nc
   !1234567890123456789012345678901234567890123456789012345678901234567890123456
   if ( .not. is_BCM ) then
      read(fname( 1:18), '(a18)') finfo
      read(fname(17:18), '(a2)')  scan_mode
      read(fname(20:21), '(i2)')  band_id
      read(fname(23:25), '(a3)')  sat_id
      read(fname(28:31), '(i4)')  year
      read(fname(32:34), '(i3)')  jday
      read(fname(35:36), '(i2)')  hour
      read(fname(37:38), '(i2)')  minute
      read(fname(39:40), '(i2)')  sec1   ! integer part of second
      read(fname(41:41), '(i1)')  sec2   ! decimal part of second
      ! get month and day from julian day
      call get_date(year, jday, month, day)
      ! 2017-10-01T18:02:19.6Z
      write(start_time,'(i4.4,4(a,i2.2),a,i2.2,a,i1,a)') &
            year, '-', month, '-', day, 'T', hour, ':',  minute, ':', sec1, '.', sec2, 'Z'
   else
      read(fname( 1:17), '(a17)') finfo
      read(fname(16:17), '(a2)')  scan_mode
      read(fname(19:21), '(a3)')  sat_id
      read(fname(24:27), '(i4)')  year
      read(fname(28:30), '(i3)')  jday
      read(fname(31:32), '(i2)')  hour
      read(fname(33:34), '(i2)')  minute
      read(fname(35:36), '(i2)')  sec1   ! integer part of second
      read(fname(37:37), '(i1)')  sec2   ! decimal part of second
      ! get month and day from julian day
      call get_date(year, jday, month, day)
      ! 2017-10-01T18:02:19.6Z
      write(start_time,'(i4.4,4(a,i2.2),a,i2.2,a,i1,a)') &
            year, '-', month, '-', day, 'T', hour, ':',  minute, ':', sec1, '.', sec2, 'Z'
   end if
   return
end subroutine decode_nc_fname

subroutine get_date(ccyy, jday, month, day)
   implicit none
   integer(i_kind), intent(in)  :: ccyy, jday
   integer(i_kind), intent(out) :: month, day
   integer(i_kind) :: mmday(12) = (/31,28,31,30,31,30,31,31,30,31,30,31/)
   integer(i_kind) :: i, jdtmp
   continue

   if ( MOD(ccyy,4) == 0 ) then
      mmday(2) = 29
      if ( MOD(ccyy,100) == 0 ) then
         mmday(2) = 28
      end if
      if ( MOD(ccyy,400) == 0 ) then
         mmday(2) = 29
      end if
   end if

   jdtmp = 0
   do i = 1, 12
      jdtmp = jdtmp + mmday(i)
      if ( jday <= jdtmp ) then
         month = i
         day = jday - ( jdtmp - mmday(i) )
         exit
      end if
   end do

   return
end subroutine get_date

subroutine output_iodav1(fname, time_start, nx, ny, nband, got_latlon, lat, lon, sat_zen, sun_zen, bt, qf, sdtb, cloudmask)

   implicit none

   character(len=*),   intent(in) :: fname
   character(len=22),  intent(in) :: time_start
   integer(i_kind),    intent(in) :: nx, ny, nband
   logical,            intent(in) :: got_latlon(nx,ny)
   real(r_kind),       intent(in) :: lat(nx,ny)
   real(r_kind),       intent(in) :: lon(nx,ny)
   real(r_kind),       intent(in) :: sat_zen(nx,ny)
   real(r_kind),       intent(in) :: sun_zen(nx,ny)
   real(r_kind),       intent(in) :: bt(nband,nx,ny)
   integer(i_kind),    intent(in) :: qf(nband,nx,ny)
   real(r_kind),       intent(in) :: sdtb(nband)
   integer(i_kind),    intent(in), optional :: cloudmask(nx,ny)

   integer(i_kind), parameter :: nstring = 50
   integer(i_kind), parameter :: ndatetime = 20
   integer(i_kind) :: nvars
   integer(i_kind) :: nlocs

   character(len=ndatetime), allocatable  :: datetime(:)   ! ccyy-mm-ddThh:mm:ssZ
   real(r_kind), allocatable :: lat_out(:)
   real(r_kind), allocatable :: lon_out(:)
   real(r_kind), allocatable :: scan_pos_out(:)
   real(r_kind), allocatable :: sat_zen_out(:)
   real(r_kind), allocatable :: sun_zen_out(:)
   real(r_kind), allocatable :: sat_azi_out(:)
   real(r_kind), allocatable :: sun_azi_out(:)
   real(r_kind), allocatable :: bt_out(:,:)
   real(r_kind), allocatable :: err_out(:,:)
   real(r_kind), allocatable :: qf_out(:,:)

   integer(i_kind) :: ncid_nlocs
   integer(i_kind) :: ncid_nvars
   integer(i_kind) :: ncid_nstring
   integer(i_kind) :: ncid_ndatetime
   integer(i_kind) :: ncfileid
   character(len=nstring) :: ncname

   character(len=nstring), allocatable :: name_var_tb(:)
   character(len=4) :: c4

   integer(i_kind) :: iline, isample, iband
   integer(i_kind) :: iloc
   integer(i_kind) :: iyear, imonth, iday, ihour, imin, isec

   character(len=60), parameter :: var_tb = "brightness_temperature"

   nvars = nband

   nlocs = 0
   do iline = 1, ny, n_subsample
      do isample = 1, nx, n_subsample
         if ( .not. got_latlon(isample,iline) ) cycle
         if ( sat_zen(isample,iline) > 80.0 ) cycle
         ! qf (DQF, Data Quality Flag)
         ! 0:good, 1:conditionally_usable, 2:out_of_range, 3:no_value
         ! keep only qf=0,1 pixels
         if ( all(qf(:,isample,iline) > 1) ) cycle
         if ( all(bt(:,isample,iline)<0.0) ) cycle
         nlocs = nlocs + 1
      end do
   end do

   write(0,*) 'nlocs = ', nlocs
   if ( nlocs <= 0 ) then
      return
   end if

   allocate (name_var_tb(1:nband))
   allocate (datetime(nlocs))
   allocate (lat_out(nlocs))
   allocate (lon_out(nlocs))
   allocate (scan_pos_out(nlocs))
   allocate (sat_zen_out(nlocs))
   allocate (sat_azi_out(nlocs))
   allocate (sun_zen_out(nlocs))
   allocate (sun_azi_out(nlocs))
   allocate (bt_out(nband,nlocs))
   allocate (err_out(nband,nlocs))
   allocate (qf_out(nband,nlocs))

   read(time_start( 1: 4), '(i4)') iyear
   read(time_start( 6: 7), '(i2)') imonth
   read(time_start( 9:10), '(i2)') iday
   read(time_start(12:13), '(i2)') ihour
   read(time_start(15:16), '(i2)') imin
   read(time_start(18:19), '(i2)') isec

   iloc = 0
   do iline = 1, ny, n_subsample
      do isample = 1, nx, n_subsample
         if ( .not. got_latlon(isample,iline) ) cycle
         if ( sat_zen(isample,iline) > 80.0 ) cycle
         if ( all(qf(:,isample,iline) > 1) ) cycle
         if ( all(bt(:,isample,iline)<0.0) ) cycle
         iloc = iloc + 1
         write(unit=datetime(iloc), fmt='(i4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a)')  &
               iyear, '-', imonth, '-', iday, 'T', ihour, ':', imin, ':', isec, 'Z'
         lat_out(iloc) = lat(isample,iline)
         lon_out(iloc) = lon(isample,iline)
         sat_zen_out(iloc) = sat_zen(isample,iline)
         sun_zen_out(iloc) = sun_zen(isample,iline)
         bt_out(1:nband,iloc) = bt(1:nband,isample,iline)
         qf_out(1:nband,iloc) = qf(1:nband,isample,iline)
         scan_pos_out(iloc) = isample
         sat_azi_out(iloc) = missing_r
         sun_azi_out(iloc) = missing_r
         err_out(1:nband,iloc) = 1.0 !missing_r
      end do
   end do

   call open_netcdf_for_write(trim(fname),ncfileid)
   call def_netcdf_dims(ncfileid,'nvars',nvars,ncid_nvars)
   call def_netcdf_dims(ncfileid,'nlocs',nlocs,ncid_nlocs)
   call def_netcdf_dims(ncfileid,'nstring',nstring,ncid_nstring)
   call def_netcdf_dims(ncfileid,'ndatetime',ndatetime,ncid_ndatetime)
   do i = 1, nvars
      write(unit=c4, fmt='(i4)') i+6
      name_var_tb(i) = trim(var_tb)//'_'//trim(adjustl(c4))
      ncname = trim(name_var_tb(i))//'@ObsValue'
      call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT,'units','K')
      ncname = trim(name_var_tb(i))//'@ObsError'
      call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
      ncname = trim(name_var_tb(i))//'@PreQC'
      call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_INT)
   end do
   ncname = 'latitude@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'longitude@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'solar_azimuth_angle@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'scan_position@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'sensor_azimuth_angle@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'solar_zenith_angle@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'sensor_zenith_angle@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'sensor_view_angle@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nlocs/),NF_FLOAT)
   ncname = 'sensor_channel@VarMetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nvars/),NF_INT)
   ncname = 'datetime@MetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_ndatetime,ncid_nlocs/),NF_CHAR)
   ncname = 'variable_names@VarMetaData'
   call def_netcdf_var(ncfileid,ncname,(/ncid_nstring,ncid_nvars/),NF_CHAR)
   call def_netcdf_end(ncfileid)

   do i = 1, nvars
      ncname = trim(name_var_tb(i))//'@ObsValue'
      call put_netcdf_var(ncfileid,ncname,bt_out(i,:))
      ncname = trim(name_var_tb(i))//'@ObsError'
      call put_netcdf_var(ncfileid,ncname,err_out(i,:))
      ncname = trim(name_var_tb(i))//'@PreQC'
      call put_netcdf_var(ncfileid,ncname,qf_out(i,:))
   end do

   ncname = 'latitude@MetaData'
   call put_netcdf_var(ncfileid,ncname,lat_out)
   ncname = 'longitude@MetaData'
   call put_netcdf_var(ncfileid,ncname,lon_out)
   ncname = 'solar_azimuth_angle@MetaData'
   call put_netcdf_var(ncfileid,ncname,sun_azi_out)
   ncname = 'scan_position@MetaData'
   call put_netcdf_var(ncfileid,ncname,scan_pos_out)
   ncname = 'sensor_azimuth_angle@MetaData'
   call put_netcdf_var(ncfileid,ncname,sat_azi_out)
   ncname = 'solar_zenith_angle@MetaData'
   call put_netcdf_var(ncfileid,ncname,sun_zen_out)
   ncname = 'sensor_zenith_angle@MetaData'
   call put_netcdf_var(ncfileid,ncname,sat_zen_out)
   ncname = 'sensor_view_angle@MetaData'
   call put_netcdf_var(ncfileid,ncname,sat_zen_out)
   ncname = 'sensor_channel@VarMetaData'
   call put_netcdf_var(ncfileid,ncname,(/7,8,9,10,11,12,13,14,15,16/))
   ncname = 'datetime@MetaData'
   call put_netcdf_var(ncfileid,ncname,datetime)
   ncname = 'variable_names@VarMetaData'
   call put_netcdf_var(ncfileid,ncname,name_var_tb(1:nband))
   call close_netcdf(trim(fname),ncfileid)

   deallocate (name_var_tb)
   deallocate (datetime)
   deallocate (lat_out)
   deallocate (lon_out)
   deallocate (scan_pos_out)
   deallocate (sat_zen_out)
   deallocate (sat_azi_out)
   deallocate (sun_zen_out)
   deallocate (sun_azi_out)
   deallocate (bt_out)
   deallocate (err_out)
   deallocate (qf_out)

end subroutine output_iodav1

subroutine calc_solar_zenith_angle(nx, ny, xlat, xlon, xtime, julian, solzen, got_latlon)

! the calulcation is adapted from subroutines radconst and calc_coszen in
! WRF phys/module_radiation_driver.F

   implicit none

   integer(i_kind),   intent(in)    :: nx, ny, julian
   real(r_kind),      intent(in)    :: xlat(nx,ny), xlon(nx,ny)
   character(len=22), intent(in)    :: xtime
   real(r_kind),      intent(inout) :: solzen(nx,ny)
   logical,           intent(in)    :: got_latlon(nx,ny)

   real(r_kind) :: obliq = 23.5
   real(r_kind) :: deg_per_day = 360.0/365.0
   real(r_kind) :: slon   ! longitude of the sun
   real(r_kind) :: declin ! declination of the sun
   real(r_kind) :: hrang, da, eot, xt, tloctm, rlat
   integer(i_kind) :: gmt, minute, i, j

   ! calculate longitude of the sun from vernal equinox
   if ( julian >= 80 ) slon = (julian - 80 ) * deg_per_day
   if ( julian <  80 ) slon = (julian + 285) * deg_per_day

   declin = asin(sin(obliq*deg2rad)*sin(slon*deg2rad)) ! in radian

   read(xtime(12:13), '(i2)') gmt
   read(xtime(15:16), '(i2)') minute

   da = 6.2831853071795862*(julian-1)/365.
   eot = (0.000075+0.001868*cos(da)-0.032077*sin(da) &
          -0.014615*cos(2.0*da)-0.04089*sin(2.0*da))*(229.18)
   xt = gmt + (minute + eot)/60.0

   do j = 1, ny
      do i = 1, nx
         if ( .not. got_latlon(i,j) ) cycle
         tloctm = xt + xlon(i,j)/15.0
         hrang = 15.0*(tloctm-12.0) * deg2rad
         rlat = xlat(i,j) * deg2rad
         solzen(i,j) = acos( sin(rlat)*sin(declin) + &
                             cos(rlat)*cos(declin)*cos(hrang) )
         solzen(i,j) = solzen(i,j) * rad2deg
      enddo
   enddo

   return
end subroutine calc_solar_zenith_angle

end program Goes_ReBroadcast_converter
