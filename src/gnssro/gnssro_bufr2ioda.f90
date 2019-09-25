!!!--------  README --------------------------------------------------------------------
!  this is a temporary routine to generate netcdf file
!  for jedi/ufo/gnssro/ operator test
!  Copyright UCAR 2019
!  Authors: Hailing Zhang
!           Mark Olah

!!!---------  to run   -----------------------------------------------------------------
!  ./gnssro_bufr2ioda yyyymmddhh $bufrfile_input $netcdffile_output


program gnssro_bufr2ioda
use netcdf
implicit none

integer, parameter :: i_short = selected_int_kind(4)  !2
integer, parameter :: i_kind  = selected_int_kind(8)
integer, parameter :: r_double = selected_real_kind(15) !8
integer, parameter :: r_kind = selected_real_kind(15) !8

! output obs data stucture
integer   :: ncid
integer   :: nobs_dimid,nlocs_dimid,nvars_dimid,nrecs_dimid,ndatetime_dimid
integer   :: varid_lat,varid_lon,varid_time,varid_datetime,varid_said,varid_ptid,varid_sclf,varid_asce
integer   :: varid_recn
integer   :: varid_geoid, varid_rfict
integer   :: varid_ref,varid_msl,varid_refoe
integer   :: varid_bnd,varid_bndoe,varid_impp, varid_imph,varid_azim
integer   :: nlev_dimid
integer   :: varid_geo_temp,varid_geo_pres,varid_geo_shum,varid_geo_geop, varid_geo_geop_sfc

character(len=256)        :: infile, outfile
character,dimension(8)    :: subset
character(len=10)         :: anatime
integer(i_kind)           :: ndatetime = 20
character(len=20)         :: datetime
integer(i_kind)           :: i,k,m,ireadmg,ireadsb,said,ptid,sclf,asce
integer(i_kind)           :: lnbufr    = 10
integer(i_kind)           :: nread,ndata,nvars,nrec, ndata0
integer(i_kind)           :: anatime_i
integer(i_kind)           :: idate5(6), idate,iadate5(6)
integer(i_kind)           :: mincy,minobs
logical                   :: good,outside
integer                   :: refflag, bendflag
integer(i_kind),parameter :: mxib=31
integer(i_kind)           :: ibit(mxib),nib
integer(i_kind),parameter :: maxlevs=500
integer(i_kind),parameter :: n1ahdr=11
integer(i_kind)           :: maxobs
real(r_kind) :: timeo
type gpsro_type
      integer(i_kind), allocatable, dimension(:)    :: said
      integer(i_kind), allocatable, dimension(:)    :: sclf
      integer(i_kind), allocatable, dimension(:)    :: ptid
      integer(i_kind), allocatable, dimension(:)    :: recn
      integer(i_kind), allocatable, dimension(:)    :: asce
      real(r_double), allocatable, dimension(:)     :: time
      character(len=20), allocatable, dimension(:)  :: datetime
      real(r_double), allocatable, dimension(:)     :: lat
      real(r_double), allocatable, dimension(:)     :: lon
      real(r_double), allocatable, dimension(:)     :: rfict
      real(r_double), allocatable, dimension(:)     :: azim
      real(r_double), allocatable, dimension(:)     :: geoid
      real(r_double), allocatable, dimension(:)     :: msl_alt
      real(r_double), allocatable, dimension(:)     :: ref
      real(r_double), allocatable, dimension(:)     :: refoe_gsi
      real(r_double), allocatable, dimension(:)     :: bend_ang
      real(r_double), allocatable, dimension(:)     :: impact_para
      real(r_double), allocatable, dimension(:)     :: bndoe_gsi
end type gpsro_type

type(gpsro_type) :: gpsro_data

real(r_double),dimension(n1ahdr)     :: bfr1ahdr
real(r_double),dimension(50,maxlevs) :: data1b
real(r_double),dimension(50,maxlevs) :: data2a
real(r_double),dimension(maxlevs)  :: nreps_this_ROSEQ2
integer(i_kind)           :: iret,levs,levsr,nreps_ROSEQ1,nreps_ROSEQ2_int
real(r_double) pcc,qfro,usage,dlat,dlat_earth,dlon,dlon_earth,freq_chk,freq,azim
real(r_double) height,rlat,rlon,ref,bend,impact,roc,geoid,  bend_error,ref_error,bend_pccf,ref_pccf
real(r_double) obsErr 
real(r_double), parameter :: missingvalue=-9.9e15
logical,        parameter :: GlobalModel = .true. ! temporary

character(10) nemo
character(80) hdr1a

data hdr1a / 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID PTID GEODU SCLF' / 
data nemo /'QFRO'/ 
nrec =0
ndata=0
nvars=2
maxobs=0

call getarg(1,anatime)
call getarg(2,infile)
call getarg(3,outfile)

read(anatime,'(i10)') anatime_i
read(anatime(1:4),'(i4)')  iadate5(1) 
read(anatime(5:6),'(i4)')  iadate5(2)
read(anatime(7:8),'(i4)')  iadate5(3)
read(anatime(9:10),'(i4)') iadate5(4)
iadate5(5) = 0
call w3fs21(iadate5,mincy)

open(lnbufr,file=trim(infile),form='unformatted')
call openbf(lnbufr,'IN',lnbufr)
call datelen(10)
call readmg(lnbufr,subset,idate,iret)
if (iret/=0) then
   write(6,*)'READ_GNSSRO: can not open gnssro file!'
   stop
end if

! loop over all message to estimate maxobs
do while(ireadmg(lnbufr,subset,idate)==0)
   read_loop_countmaxobs:  do while(ireadsb(lnbufr)==0)
     call ufbint(lnbufr,bfr1ahdr,n1ahdr,1,iret,hdr1a)
     call ufbseq(lnbufr,data1b,50,maxlevs,levs,'ROSEQ1')
     maxobs= maxobs + levs
  enddo read_loop_countmaxobs
end do

allocate(gpsro_data%said(maxobs))
allocate(gpsro_data%sclf(maxobs))
allocate(gpsro_data%ptid(maxobs))
allocate(gpsro_data%recn(maxobs))
allocate(gpsro_data%asce(maxobs))
allocate(gpsro_data%time(maxobs))
allocate(gpsro_data%datetime(maxobs))
allocate(gpsro_data%lat(maxobs))
allocate(gpsro_data%lon(maxobs))
allocate(gpsro_data%rfict(maxobs))
allocate(gpsro_data%azim(maxobs))
allocate(gpsro_data%geoid(maxobs))
allocate(gpsro_data%msl_alt(maxobs))
allocate(gpsro_data%ref(maxobs))
allocate(gpsro_data%refoe_gsi(maxobs))
allocate(gpsro_data%bend_ang(maxobs))
allocate(gpsro_data%impact_para(maxobs))
allocate(gpsro_data%bndoe_gsi(maxobs))

!rewind lnbufr
call closbf(lnbufr)
open(lnbufr,file=trim(infile),form='unformatted')
call openbf(lnbufr,'IN',lnbufr)
call datelen(10)
call readmg(lnbufr,subset,idate,iret)

do while(ireadmg(lnbufr,subset,idate)==0)
   read_loop:  do while(ireadsb(lnbufr)==0)
!    Read/decode data in subset (profile)
     call ufbint(lnbufr,bfr1ahdr,n1ahdr,1,iret,hdr1a)
     call ufbint(lnbufr,qfro,1,1,iret,nemo)
!    observation time in minutes
     idate5(1) = bfr1ahdr(1) ! year
     idate5(2) = bfr1ahdr(2) ! month
     idate5(3) = bfr1ahdr(3) ! day
     idate5(4) = bfr1ahdr(4) ! hour
     idate5(5) = bfr1ahdr(5) ! minute
     idate5(6) = 0 ! seconds
     pcc  = bfr1ahdr(6)         ! profile per cent confidence
     roc  = bfr1ahdr(7)         ! Earth local radius of curvature
     said = bfr1ahdr(8)        ! Satellite identifier
     ptid = bfr1ahdr(9)        ! Platform transmitter ID number
     geoid= bfr1ahdr(10)      ! Geoid undulation
     sclf = bfr1ahdr(11) 

     call w3fs21(idate5,minobs)
     write(datetime,'(I4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")') idate5(1),idate5(2),idate5(3),idate5(4),idate5(5),idate5(6)
     timeo=real(minobs-mincy,r_kind)/60.0

     if( roc>6450000.0_r_kind .or. roc<6250000.0_r_kind  .or.       &
         geoid>200_r_kind .or. geoid<-200._r_kind ) then
         write(6,*)'READ_GNSSRO: profile fails georeality check, skip this report'
         cycle read_loop
     endif

!    profile check:  (1) CDAAC processing - cosmic-1, cosmic-2, sacc, cnofs, kompsat5
     if ( ((said >= 740).and.(said <=745)).or.((said >= 750).and.(said <= 755)) .or.(said == 820).or.(said == 786).or.(said == 825)) then  !CDAAC processing
       if(pcc == 0.0) then
          write(6,*)'READ_GNSSRO: bad profile 0.0% confidence said=',said,'ptid=',ptid, ' SKIP this report'
          cycle read_loop
        endif
     endif

     bendflag = 0
     refflag  = 0
!    profile check:  (2) GRAS SAF processing - metopa-c, oceansat2, megha-tropiques, sacd 
     if ( (said >= 3 .and.said <= 5).or.(said == 421).or.(said == 440).or. (said == 821) ) then 
          call upftbv(lnbufr,nemo,qfro,mxib,ibit,nib)
          if(nib > 0) then
            do i = 1, nib
               if(ibit(i)== 5) then  ! bending angle
                  bendflag = 1
                  write(6,*)'READ_GNSSRO: bad profile said=',said,'ptid=',ptid, ' SKIP this report'
                  cycle read_loop
               endif
               if(ibit(i)== 6) then  ! refractivity
                  refflag = 1
                  exit
               endif
           enddo
         endif 
     endif

     asce = 0 
     call upftbv(lnbufr,nemo,qfro,mxib,ibit,nib)
     if ( nib > 0) then
        do i = 1, nib
          if(ibit(i)== 3) then
             asce = 1
             exit
          endif
        enddo
     end if

     call ufbint(lnbufr,nreps_this_ROSEQ2,1,maxlevs,nreps_ROSEQ1,'{ROSEQ2}')
     call ufbseq(lnbufr,data1b,50,maxlevs,levs,'ROSEQ1') 
     call ufbseq(lnbufr,data2a,50,maxlevs,levsr,'ROSEQ3') ! refractivity

     nrec=nrec+1
     ndata0 = ndata

     do k = 1, levs
        rlat     = data1b(1,k)  ! earth relative latitude (degrees)
        rlon     = data1b(2,k)  ! earth relative longitude (degrees)
        azim     = data1b(3,k)
        height   = data2a(1,k)
        ref      = data2a(2,k)
        ref_error= data2a(4,k)
        ref_pccf = data2a(6,k)

!       Loop over number of replications of ROSEQ2 nested inside this particular replication of ROSEQ1
        nreps_ROSEQ2_int = nreps_this_ROSEQ2(k)
        do i = 1,nreps_ROSEQ2_int
           m = (6*i)-2
           freq_chk=data1b(m,k)      ! frequency (hertz)
           if(nint(freq_chk).ne.0) cycle ! do not want non-zero freq., go on to next replication of ROSEQ2
           freq       = data1b(m,k)
           impact     = data1b(m+1,k)      ! impact parameter (m)
           bend       = data1b(m+2,k)        ! bending angle (rad)
           bend_error = data1b(m+4,k)  ! RMSE in bending angle (rad)
        enddo

        bend_pccf=data1b((6*nreps_ROSEQ2_int)+4,k)  ! percent confidence for this ROSEQ1 replication
        good=.true. 

        if (  height<0._r_kind   .or. height>60000._r_kind .or.           &
           abs(azim)>360._r_kind .or.                                     &
           abs(rlat)>90._r_kind  .or. abs(rlon)>360._r_kind ) then
           good=.false.
           write(6,*)'READ_GNSSRO: obs fails georeality check, said=',said,'ptid=',ptid
        endif
        if ( bend>=1.e+9_r_kind .or. bend<=0._r_kind .or. impact>=1.e+9_r_kind .or. impact<roc .or. bendflag == 1 ) then
           good=.false.
           write(6,*)'READ_GNSSRO: obs bend/impact is invalid, said=',said,'ptid=',ptid
        endif

        if ( ref>=1.e+9_r_kind .or. ref<=0._r_kind .or. refflag == 1 ) then
             ref = missingvalue
        endif
    
    if(good) then
       ndata  = ndata +1 
       gpsro_data%recn(ndata)     = nrec
       gpsro_data%lat(ndata)      = rlat
       gpsro_data%lon(ndata)      = rlon
       gpsro_data%time(ndata)     = timeo
       gpsro_data%datetime(ndata) = datetime
       gpsro_data%said(ndata)     = said
       gpsro_data%sclf(ndata)     = sclf
       gpsro_data%asce(ndata)     = asce
       gpsro_data%ptid(ndata)     = ptid
       gpsro_data%ref(ndata)      = ref
       gpsro_data%msl_alt(ndata)  = height
       gpsro_data%bend_ang(ndata)     = bend
       gpsro_data%bndoe_gsi(ndata)    = bend_error
       gpsro_data%impact_para(ndata)  = impact! 
       gpsro_data%rfict(ndata) = roc
       gpsro_data%geoid(ndata) = geoid
       gpsro_data%azim(ndata)  = azim
       CALL bendingangle_err_gsi(rlat,impact-roc, obsErr)
       gpsro_data%bndoe_gsi(ndata) = obsErr
       if ( ref > missingvalue)  then
          CALL refractivity_err_gsi(rlat,height, GlobalModel, obsErr)
          gpsro_data%refoe_gsi(ndata) = obsErr
       else
          gpsro_data%refoe_gsi(ndata) = missingvalue
       end if
     end if

    end do ! end of k loop

    if (ndata == ndata0) nrec = nrec -1

  enddo read_loop
end do

call closbf(lnbufr)
if (nrec==0) then
    write(6,*) "Error. No valid observations found. Cannot create NetCDF ouput."
    stop 1
endif

call check( nf90_create(trim(outfile), NF90_NETCDF4, ncid))
call check( nf90_def_dim(ncid, 'nlocs', ndata,   nlocs_dimid) )
call check( nf90_def_dim(ncid, 'nrecs', nrec,    nrecs_dimid) )
call check( nf90_def_dim(ncid, 'nobs',  ndata*nvars,   nobs_dimid) )
call check( nf90_def_dim(ncid, 'nvars', nvars,   nvars_dimid) )
call check( nf90_def_dim(ncid, 'ndatetime', ndatetime,   ndatetime_dimid) )
call check( nf90_put_att(ncid, NF90_GLOBAL, 'date_time', anatime_i) )
call check( nf90_def_var(ncid, "latitude@MetaData",      NF90_FLOAT, nlocs_dimid, varid_lat) )
call check( nf90_def_var(ncid, "longitude@MetaData",    NF90_FLOAT, nlocs_dimid, varid_lon) )
call check( nf90_def_var(ncid, "time@MetaData",         NF90_FLOAT, nlocs_dimid, varid_time) )
call check( nf90_def_var(ncid, "datetime@MetaData",     NF90_CHAR, (/ ndatetime_dimid, nlocs_dimid /), varid_datetime) )
call check( nf90_def_var(ncid, "record_number@MetaData",   NF90_INT, nlocs_dimid, varid_recn))
call check( nf90_def_var(ncid, "gnss_sat_class@MetaData",  NF90_INT, nlocs_dimid, varid_sclf))
call check( nf90_def_var(ncid, "reference_sat_id@MetaData", NF90_INT, nlocs_dimid, varid_ptid))
call check( nf90_def_var(ncid, "occulting_sat_id@MetaData",  NF90_INT, nlocs_dimid, varid_said))
call check( nf90_def_var(ncid, "ascending_flag@MetaData",  NF90_INT, nlocs_dimid, varid_asce))
call check( nf90_def_var(ncid, "refractivity@ObsValue", NF90_FLOAT, nlocs_dimid, varid_ref) )
call check( nf90_def_var(ncid, "refractivity@ObsError", NF90_FLOAT, nlocs_dimid, varid_refoe))
call check( nf90_def_var(ncid, "altitude@MetaData", NF90_FLOAT, nlocs_dimid, varid_msl) )
call check( nf90_def_var(ncid, "bending_angle@ObsValue", NF90_FLOAT, nlocs_dimid, varid_bnd) )
call check( nf90_def_var(ncid, "bending_angle@ObsError", NF90_FLOAT, nlocs_dimid, varid_bndoe) )
call check( nf90_def_var(ncid, "impact_parameter@MetaData", NF90_FLOAT, nlocs_dimid, varid_impp))
call check( nf90_def_var(ncid, "impact_height@MetaData", NF90_FLOAT, nlocs_dimid, varid_imph))
call check( nf90_def_var(ncid, "sensor_azimuth_angle@MetaData",NF90_FLOAT, nlocs_dimid, varid_azim))
call check( nf90_def_var(ncid, "geoid_height_above_reference_ellipsoid@MetaData",NF90_FLOAT, nlocs_dimid, varid_geoid))
call check( nf90_def_var(ncid, "earth_radius_of_curvature@MetaData",NF90_FLOAT, nlocs_dimid, varid_rfict))

call check( nf90_enddef(ncid) )
call check( nf90_put_var(ncid, varid_lat, gpsro_data%lat(1:ndata)) )
call check( nf90_put_var(ncid, varid_lon, gpsro_data%lon(1:ndata)) )
call check( nf90_put_var(ncid, varid_time, gpsro_data%time(1:ndata)) )
call check( nf90_put_var(ncid, varid_datetime, gpsro_data%datetime(1:ndata)) )
call check( nf90_put_var(ncid, varid_recn, gpsro_data%recn(1:ndata)) )
call check( nf90_put_var(ncid, varid_said, gpsro_data%said(1:ndata)) )
call check( nf90_put_var(ncid, varid_ptid, gpsro_data%ptid(1:ndata)) )
call check( nf90_put_var(ncid, varid_sclf, gpsro_data%sclf(1:ndata)) )
call check( nf90_put_var(ncid, varid_asce, gpsro_data%asce(1:ndata)) )
call check( nf90_put_var(ncid, varid_ref, gpsro_data%ref(1:ndata)) )
call check( nf90_put_var(ncid, varid_refoe, gpsro_data%refoe_gsi(1:ndata)) )
call check( nf90_put_var(ncid, varid_msl, gpsro_data%msl_alt(1:ndata)) )
call check( nf90_put_var(ncid, varid_bnd, gpsro_data%bend_ang(1:ndata)) )
call check( nf90_put_var(ncid, varid_bndoe, gpsro_data%bndoe_gsi(1:ndata)) )
call check( nf90_put_var(ncid, varid_impp, gpsro_data%impact_para(1:ndata)) )
call check( nf90_put_var(ncid, varid_imph, gpsro_data%impact_para(1:ndata)-gpsro_data%rfict(1:ndata)-gpsro_data%geoid(1:ndata)  ) )
call check( nf90_put_var(ncid, varid_azim, gpsro_data%azim(1:ndata)) )
call check( nf90_put_var(ncid, varid_geoid, gpsro_data%geoid(1:ndata)) )
call check( nf90_put_var(ncid, varid_rfict, gpsro_data%rfict(1:ndata)) )
call check( nf90_close(ncid) ) 

deallocate(gpsro_data%said)
deallocate(gpsro_data%sclf)
deallocate(gpsro_data%ptid)
deallocate(gpsro_data%recn)
deallocate(gpsro_data%asce)
deallocate(gpsro_data%time)
deallocate(gpsro_data%datetime)
deallocate(gpsro_data%lat)
deallocate(gpsro_data%lon)
deallocate(gpsro_data%rfict)
deallocate(gpsro_data%azim)
deallocate(gpsro_data%geoid)
deallocate(gpsro_data%msl_alt)
deallocate(gpsro_data%ref)
deallocate(gpsro_data%refoe_gsi)
deallocate(gpsro_data%bend_ang)
deallocate(gpsro_data%impact_para)
deallocate(gpsro_data%bndoe_gsi)

contains
 subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
     print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

  end subroutine check  


subroutine  refractivity_err_gsi(obsLat, obsZ, GlobalModel, obsErr)
real(r_kind), intent(in)   :: obsLat,  obsZ
real(r_kind),  intent(out) :: obsErr
logical,         intent(in)   :: GlobalModel
real(r_kind)               :: obsZ_km

obsZ_km  = obsZ / 1000.0_r_kind

if( GlobalModel ) then ! for global

     if( obsLat>= 20.0 .or.obsLat<= -20.0 ) then
         obsErr=-1.321_r_kind+0.341_r_kind*obsZ_km-0.005_r_kind*obsZ_km**2
     else
       if(obsZ_km > 10.0) then
          obsErr=2.013_r_kind-0.060_r_kind*obsZ_km+0.0045_r_kind*obsZ_km**2
       else
          obsErr=-1.18_r_kind+0.058_r_kind*obsZ_km+0.025_r_kind*obsZ_km**2
       endif
     endif
     obsErr = 1.0_r_kind/abs(exp(obsErr))

else ! for regional 
     if( obsLat >= 20.0 .or.obsLat <= -20.0 ) then
         if (obsZ_km > 10.00) then
             obsErr =-1.321_r_kind+0.341_r_kind*obsZ_km-0.005_r_kind*obsZ_km**2
         else
             obsErr =-1.2_r_kind+0.065_r_kind*obsZ_km+0.021_r_kind*obsZ_km**2
         endif
     else
         if(obsZ_km > 10.00) then
            obsErr =2.013_r_kind-0.120_r_kind*obsZ_km+0.0065_r_kind*obsZ_km**2
         else
            obsErr =-1.19_r_kind+0.03_r_kind*obsZ_km+0.023_r_kind*obsZ_km**2
         endif
     endif
     obsErr = 1.0_r_kind/abs(exp(obsErr))
endif

end subroutine refractivity_err_gsi

subroutine  bendingangle_err_gsi(obsLat, obsZ,  obsErr)
real(r_kind), intent(in)   :: obsLat,  obsZ
real(r_kind),  intent(out) :: obsErr
real(r_kind)               :: obsZ_km

obsZ_km  = obsZ / 1000.0_r_kind
if((said==41).or.(said==722).or.(said==723).or.(said==4).or.(said==42).or.&
                 (said==3).or.(said==821.or.(said==421)).or.(said==440).or.(said==43)) then
     if( abs(obsLat)>= 40.00 ) then

        if(obsZ_km>12.) then
          obsErr=0.19032_r_kind+0.287535_r_kind*obsZ_km-0.00260813_r_kind*obsZ_km**2
        else
          obsErr=-3.20978_r_kind+1.26964_r_kind*obsZ_km-0.0622538_r_kind*obsZ_km**2
        endif
     else
        if(obsZ_km>18.) then
          obsErr=-1.87788_r_kind+0.354718_r_kind*obsZ_km-0.00313189_r_kind*obsZ_km**2
        else
          obsErr=-2.41024_r_kind+0.806594_r_kind*obsZ_km-0.027257_r_kind*obsZ_km**2
        endif
     endif
     obsErr = 0.001_r_kind/abs(exp(obsErr))

 else !!!! CDAAC processing
     if( abs(obsLat)>= 40.00 ) then
         if (obsZ_km > 12.00) then
            obsErr=-0.685627_r_kind+0.377174_r_kind*obsZ_km-0.00421934_r_kind*obsZ_km**2
         else
            obsErr=-3.27737_r_kind+1.20003_r_kind*obsZ_km-0.0558024_r_kind*obsZ_km**2
         endif
      else
         if(obsZ_km >18.00) then
            obsErr=-2.73867_r_kind+0.447663_r_kind*obsZ_km-0.00475603_r_kind*obsZ_km**2
         else
            obsErr=-3.45303_r_kind+0.908216_r_kind*obsZ_km-0.0293331_r_kind*obsZ_km**2
         endif
     endif
     obsErr = 0.001_r_kind/abs(exp(obsErr))

endif

end subroutine bendingangle_err_gsi
!!!!!!________________________________________________________  

!!!!!! SUBROUTINE W3FS21 was copied from GSI/src/libs/w3nco_v2.0.6/w3fs21.f and iw3jdn.f
SUBROUTINE W3FS21(IDATE, NMIN)
    INTEGER  IDATE(5)
    INTEGER  NMIN
    INTEGER  IYEAR, NDAYS, IJDN
    INTEGER  JDN78
    DATA  JDN78 / 2443510 /
    NMIN  = 0

    IYEAR = IDATE(1)

    IF (IYEAR.LE.99) THEN
        IF (IYEAR.LT.78) THEN
            IYEAR = IYEAR + 2000
        ELSE
            IYEAR = IYEAR + 1900
        ENDIF
    ENDIF

!   COMPUTE JULIAN DAY NUMBER FROM YEAR, MONTH, DAY
    IJDN  = IDATE(3) - 32075      &
             + 1461 * (IYEAR + 4800 + (IDATE(2) - 14) / 12) / 4  &
             + 367 * (IDATE(2)- 2 - (IDATE(2) -14) / 12 * 12) / 12   &
             - 3 * ((IYEAR + 4900 + (IDATE(2) - 14) / 12) / 100) / 4

!   SUBTRACT JULIAN DAY NUMBER OF JAN 1,1978 TO GET THE
!   NUMBER OF DAYS BETWEEN DATES
    NDAYS = IJDN - JDN78
    NMIN = NDAYS * 1440 + IDATE(4) * 60 + IDATE(5)
    RETURN
END SUBROUTINE W3FS21

end program
