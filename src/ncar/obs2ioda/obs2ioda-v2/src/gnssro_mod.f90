!!!--------  README --------------------------------------------------------------------
!  this is a temporary routine to generate netcdf4 file
!  for jedi/ufo/gnssro/ operator test
!  IODA VERSION 2
!  Copyright UCAR 2022
!  Author: Hailing Zhang

module gnssro_bufr2ioda
use netcdf
implicit none

integer, parameter :: i_kind  = selected_int_kind(8)    !4
integer, parameter :: i_64    = selected_int_kind(10)   !8
integer, parameter :: r_kind  = selected_real_kind(15)  !8

integer(i_kind) :: said
logical :: verbose = .false.

contains

subroutine read_write_gnssro(infile, outdir)

! output obs data stucture
integer   :: ncid
integer   :: nlocs_dimid,nvars_dimid
integer   :: varid_lat,varid_lon,varid_time,varid_epochtime
integer   :: varid_said,varid_siid,varid_ptid,varid_sclf,varid_asce,varid_ogce
integer   :: varid_recn
integer   :: varid_geoid, varid_rfict
integer   :: varid_ref,varid_msl,varid_refoe
integer   :: varid_bnd,varid_bndoe,varid_impp, varid_imph,varid_azim
integer   :: nlev_dimid
integer   :: varid_geo_temp,varid_geo_pres,varid_geo_shum,varid_geo_geop, varid_geo_geop_sfc
integer   :: grpid_metadata,grpid_obserror,grpid_obsvalue
character(len=*), intent(in) :: infile
character(len=*), intent(in) :: outdir
character(len=256)        :: outfile
character,dimension(8)    :: subset
character(len=10)         :: anatime
integer(i_kind)           :: i,k,m,ireadmg,ireadsb,said,siid,ptid,sclf,asce,ogce
integer(i_kind)           :: lnbufr    = 10
integer(i_kind)           :: nread,ndata,nvars,nrec, ndata0
integer(i_kind)           :: idate5(6), idate,iadate5(6)
integer(i_kind)           :: mincy,minobs
integer(i_64)             :: epochtime

logical                   :: good,outside
integer                   :: refflag, bendflag
integer(i_kind),parameter :: mxib=31
integer(i_kind)           :: ibit(mxib),nib
integer(i_kind),parameter :: maxlevs=500
integer(i_kind),parameter :: n1ahdr=13
integer(i_kind)           :: maxobs
real(r_kind) :: timeo
type gnssro_type
      integer(i_kind), allocatable, dimension(:)    :: said
      integer(i_kind), allocatable, dimension(:)    :: siid
      integer(i_kind), allocatable, dimension(:)    :: sclf
      integer(i_kind), allocatable, dimension(:)    :: ptid
      integer(i_kind), allocatable, dimension(:)    :: recn
      integer(i_kind), allocatable, dimension(:)    :: asce
      integer(i_kind), allocatable, dimension(:)    :: ogce
      real(r_kind), allocatable, dimension(:)       :: time
      integer(i_64),allocatable, dimension(:)     :: epochtime
      real(r_kind), allocatable, dimension(:)     :: lat
      real(r_kind), allocatable, dimension(:)     :: lon
      real(r_kind), allocatable, dimension(:)     :: rfict
      real(r_kind), allocatable, dimension(:)     :: azim
      real(r_kind), allocatable, dimension(:)     :: geoid
      real(r_kind), allocatable, dimension(:)     :: msl_alt
      real(r_kind), allocatable, dimension(:)     :: ref
      real(r_kind), allocatable, dimension(:)     :: refoe_gsi
      real(r_kind), allocatable, dimension(:)     :: bend_ang
      real(r_kind), allocatable, dimension(:)     :: impact_para
      real(r_kind), allocatable, dimension(:)     :: bndoe_gsi
end type gnssro_type

type(gnssro_type) :: gnssro_data

real(r_kind),dimension(n1ahdr)     :: bfr1ahdr
real(r_kind),dimension(50,maxlevs) :: data1b
real(r_kind),dimension(50,maxlevs) :: data2a
real(r_kind),dimension(maxlevs)    :: nreps_this_ROSEQ2
integer(i_kind)                    :: iret,levs,levsr,nreps_ROSEQ1,nreps_ROSEQ2_int
real(r_kind) :: pcc,qfro(1),usage,dlat,dlat_earth,dlon,dlon_earth,freq_chk,freq,azim
real(r_kind) :: height,rlat,rlon,ref,bend,impact,roc,geoid,  bend_error,ref_error,bend_pccf,ref_pccf
real(r_kind) :: obsErr
real(r_kind)    :: r_missing
integer(i_kind) :: i_missing
integer(i_64)   :: i64_missing

logical,        parameter :: GlobalModel = .true. ! temporary

character(10) nemo
character(80) hdr1a

data hdr1a / 'YEAR MNTH DAYS HOUR MINU PCCF ELRC SAID SIID PTID GEODU SCLF OGCE' / 
data nemo /'QFRO'/ 

i_missing=huge(i_missing)
i64_missing=huge(i64_missing)
r_missing=huge(i_missing)

nrec =0
ndata=0
nvars=2
maxobs=0

open(lnbufr,file=trim(infile),form='unformatted')
call openbf(lnbufr,'IN',lnbufr)
call datelen(10)
call readmg(lnbufr,subset,idate,iret)
if (iret/=0) then
   write(6,*)'READ_GNSSRO: can not open gnssro file!'
   stop
end if

write(unit=*,fmt='(a,i10)') trim(infile)//' file date is: ', idate
iadate5(1) = idate/1000000
iadate5(2) = (idate-iadate5(1)*1000000)/10000
iadate5(3) = (idate-iadate5(1)*1000000-iadate5(2)*10000)/100
iadate5(4) = idate-iadate5(1)*1000000-iadate5(2)*10000-iadate5(3)*100
iadate5(5) = 0
call w3fs21(iadate5,mincy)
write(anatime,'(i10)') idate

write(outfile,'(a,i10.10,a)') trim(outdir)//'gnssro_obs_',idate,'.h5'

! loop over all message to estimate maxobs
do while(ireadmg(lnbufr,subset,idate)==0)
   read_loop_countmaxobs:  do while(ireadsb(lnbufr)==0)
     call ufbint(lnbufr,bfr1ahdr,n1ahdr,1,iret,hdr1a)
     call ufbseq(lnbufr,data1b,50,maxlevs,levs,'ROSEQ1')
     maxobs= maxobs + levs
  enddo read_loop_countmaxobs
end do

allocate(gnssro_data%said(maxobs))
allocate(gnssro_data%siid(maxobs))
allocate(gnssro_data%sclf(maxobs))
allocate(gnssro_data%ptid(maxobs))
allocate(gnssro_data%recn(maxobs))
allocate(gnssro_data%asce(maxobs))
allocate(gnssro_data%ogce(maxobs))
allocate(gnssro_data%time(maxobs))
allocate(gnssro_data%epochtime(maxobs))
allocate(gnssro_data%lat(maxobs))
allocate(gnssro_data%lon(maxobs))
allocate(gnssro_data%rfict(maxobs))
allocate(gnssro_data%azim(maxobs))
allocate(gnssro_data%geoid(maxobs))
allocate(gnssro_data%msl_alt(maxobs))
allocate(gnssro_data%ref(maxobs))
allocate(gnssro_data%refoe_gsi(maxobs))
allocate(gnssro_data%bend_ang(maxobs))
allocate(gnssro_data%impact_para(maxobs))
allocate(gnssro_data%bndoe_gsi(maxobs))

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
     pcc  = bfr1ahdr(6)        ! profile per cent confidence
     roc  = bfr1ahdr(7)        ! Earth local radius of curvature
     said = bfr1ahdr(8)        ! Satellite identifier
     siid = bfr1ahdr(9)        ! Satellite instrument
     ptid = bfr1ahdr(10)       ! Platform transmitter ID number
     geoid= bfr1ahdr(11)       ! Geoid undulation
     sclf = bfr1ahdr(12)       ! Satellite classification
     ogce = bfr1ahdr(13)       ! Identification of originating/generating centre

     call w3fs21(idate5,minobs)
     timeo=real(minobs-mincy,r_kind)/60.0
     call epochtimecalculator(idate5, epochtime)  ! calculate epochtime since January 1 1970

     if( roc>6450000.0_r_kind .or. roc<6250000.0_r_kind  .or.       &
         geoid>200_r_kind .or. geoid<-200._r_kind ) then
         if (verbose) write(6,*)'READ_GNSSRO: profile fails georeality check, skip this report'
         cycle read_loop
     endif

!    profile check:  (1) CDAAC processing - cosmic-1, cosmic-2, sacc, cnofs, kompsat5
     if ( ((said >= 740).and.(said <=745)).or. &
          ((said >= 750).and.(said <= 755))    &
            .or.(said == 820).or.(said == 786).or.(said == 825) &
            .or. ogce == 60) then  !CDAAC processing
       if(pcc == 0.0) then
          if (verbose) write(6,*)'READ_GNSSRO: bad profile 0.0% confidence said=', &
            said,'ptid=',ptid, ' SKIP this report'
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
                  if (verbose) write(6,*)'READ_GNSSRO: bad profile said=',said,'ptid=', &
                     ptid, ' SKIP this report'
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

        if (  height<0._r_kind   .or. height>100000._r_kind .or.           &
           abs(rlat)>90._r_kind  .or. abs(rlon)>360._r_kind ) then
           good=.false.
           if (verbose) write(6,*)'READ_GNSSRO: obs fails georeality check, said=',said,'ptid=',ptid
        endif
        if ( bend>=1.e+9_r_kind .or. bend<=0._r_kind .or. impact>=1.e+9_r_kind .or. impact<roc .or. bendflag == 1 ) then
           good=.false.
           if (verbose) write(6,*)'READ_GNSSRO: obs bend/impact is invalid, said=',said,'ptid=',ptid
        endif

        if ( ref>=1.e+9_r_kind .or. ref<=0._r_kind .or. refflag == 1 ) then
             ref = r_missing
        endif
    
        if ( abs(azim)>360._r_kind  .or. azim<0._r_kind ) then
             azim = r_missing
        endif

    if(good) then
       ndata  = ndata +1 
       gnssro_data%recn(ndata)     = nrec
       gnssro_data%lat(ndata)      = rlat
       gnssro_data%lon(ndata)      = rlon
       gnssro_data%time(ndata)     = timeo
       gnssro_data%epochtime(ndata)= epochtime
       gnssro_data%said(ndata)     = said
       gnssro_data%siid(ndata)     = siid
       gnssro_data%sclf(ndata)     = sclf
       gnssro_data%asce(ndata)     = asce
       gnssro_data%ptid(ndata)     = ptid
       gnssro_data%ogce(ndata)     = ogce
       gnssro_data%ref(ndata)      = ref
       gnssro_data%msl_alt(ndata)  = height
       gnssro_data%bend_ang(ndata)     = bend
       gnssro_data%bndoe_gsi(ndata)    = bend_error
       gnssro_data%impact_para(ndata)  = impact
       gnssro_data%rfict(ndata) = roc
       gnssro_data%geoid(ndata) = geoid
       gnssro_data%azim(ndata)  = azim
       CALL bendingangle_err_gsi(rlat,impact-roc, obsErr, ogce)
       gnssro_data%bndoe_gsi(ndata) = obsErr
       if ( ref > r_missing)  then
          CALL refractivity_err_gsi(rlat,height, GlobalModel, obsErr)
          gnssro_data%refoe_gsi(ndata) = obsErr
       else
          gnssro_data%refoe_gsi(ndata) = r_missing
       end if
     end if

    end do ! end of k loop

    if (ndata == ndata0) nrec = nrec -1

  enddo read_loop
end do

call closbf(lnbufr)
if (nrec==0) then
    write(6,*) "Error. No valid observations found. Cannot create NetCDF ouput."
    stop 2
endif

call check( nf90_create(trim(outfile), NF90_NETCDF4, ncid))
call check( nf90_def_dim(ncid, 'nlocs', ndata,   nlocs_dimid) )
call check( nf90_put_att(ncid, NF90_GLOBAL, 'date_time', anatime) )
call check( nf90_put_att(ncid, NF90_GLOBAL, 'ioda_version', 'fortran generated ioda2 file') )
call check( nf90_def_grp(ncid, 'MetaData', grpid_metadata) )
call check( nf90_def_grp(ncid, 'ObsValue', grpid_obsvalue) )
call check( nf90_def_grp(ncid, 'ObsError', grpid_obserror) )

call check( nf90_def_var(grpid_metadata, "latitude",      NF90_FLOAT, nlocs_dimid, varid_lat) )
call check( nf90_put_att(grpid_metadata, varid_lat, "units",  "degree_north" ))
call check( nf90_def_var_fill(grpid_metadata, varid_lat, 0, real(r_missing)))

call check( nf90_def_var(grpid_metadata, "longitude",     NF90_FLOAT, nlocs_dimid, varid_lon) )
call check( nf90_put_att(grpid_metadata, varid_lon, "units",  "degree_east" ))
call check( nf90_def_var_fill(grpid_metadata, varid_lon, 0, real(r_missing)))

call check( nf90_def_var(grpid_metadata, "time",          NF90_FLOAT, nlocs_dimid, varid_time) )
call check( nf90_put_att(grpid_metadata, varid_time, "longname", "time offset to analysis time" ))
call check( nf90_put_att(grpid_metadata, varid_time, "units", "hour" ))
call check( nf90_def_var_fill(grpid_metadata, varid_time, 0, real(r_missing)))

call check( nf90_def_var(grpid_metadata, "dateTime",     NF90_INT64, nlocs_dimid, varid_epochtime) )
call check( nf90_put_att(grpid_metadata, varid_epochtime, "units","seconds since 1970-01-01T00:00:00Z" ))
call check( nf90_def_var_fill(grpid_metadata, varid_epochtime, 0, i64_missing ))

call check( nf90_def_var(grpid_metadata, "record_number",   NF90_INT, nlocs_dimid, varid_recn))
call check( nf90_put_att(grpid_metadata, varid_recn, "longname", "GNSS RO profile identifier" ))
call check( nf90_put_att(grpid_metadata, varid_recn, "units",  "1" ))
call check( nf90_def_var_fill(grpid_metadata, varid_recn, 0, i_missing ))

call check( nf90_def_var(grpid_metadata, "gnss_sat_class",  NF90_INT, nlocs_dimid, varid_sclf))
call check( nf90_put_att(grpid_metadata, varid_sclf, "longname", "GNSS satellite classification, &
                                                     &e.g., 401=GPS, 402=GLONASS" ))
call check( nf90_put_att(grpid_metadata, varid_sclf, "units",  "1" ))
call check( nf90_def_var_fill(grpid_metadata, varid_sclf, 0, i_missing ))

call check( nf90_def_var(grpid_metadata, "reference_sat_id", NF90_INT, nlocs_dimid, varid_ptid))
call check( nf90_put_att(grpid_metadata, varid_ptid, "longname", "GNSS satellite transmitter identifier (1-32)" ))
call check( nf90_put_att(grpid_metadata, varid_ptid, "units",  "1" ))
call check( nf90_def_var_fill(grpid_metadata, varid_ptid, 0, i_missing ))

call check( nf90_def_var(grpid_metadata, "occulting_sat_id", NF90_INT, nlocs_dimid, varid_said))
call check( nf90_put_att(grpid_metadata, varid_said, "longname", "Low Earth Orbit satellite identifier, e.g., COSMIC2=750-755" ))
call check( nf90_put_att(grpid_metadata, varid_said, "units",  "1" ))
call check( nf90_def_var_fill(grpid_metadata, varid_said, 0, i_missing ))

call check( nf90_def_var(grpid_metadata, "occulting_sat_is", NF90_INT, nlocs_dimid, varid_siid))
call check( nf90_put_att(grpid_metadata, varid_siid, "longname", "satellite instrument"))
call check( nf90_put_att(grpid_metadata, varid_siid, "units",  "1" ))
call check( nf90_def_var_fill(grpid_metadata, varid_siid, 0, i_missing ))

call check( nf90_def_var(grpid_metadata, "ascending_flag",  NF90_INT, nlocs_dimid, varid_asce))
call check( nf90_put_att(grpid_metadata, varid_asce, "longname", "the original occultation ascending/descending flag" ))
call check( nf90_put_att(grpid_metadata, varid_asce, "valid_range", int((/ 0, 1 /))) )
call check( nf90_put_att(grpid_metadata, varid_asce, "flag_values", int((/ 0, 1 /))) )
call check( nf90_put_att(grpid_metadata, varid_asce, "flag_meanings", "descending ascending") )
call check( nf90_put_att(grpid_metadata, varid_asce, "units",  "1" ))
call check( nf90_def_var_fill(grpid_metadata, varid_asce, 0, i_missing ))

call check( nf90_def_var(grpid_metadata, "process_center",NF90_INT,nlocs_dimid, varid_ogce))
call check( nf90_put_att(grpid_metadata, varid_ogce, "longname", "originally data processing_center, &
                                                     &e.g., 60 for UCAR, 94 for DMI, 78 for GFZ" ))
call check( nf90_put_att(grpid_metadata, varid_ogce, "units",  "1" ))
call check( nf90_def_var_fill(grpid_metadata, varid_ogce, 0, i_missing ))

call check( nf90_def_var(grpid_obsvalue, "refractivity", NF90_FLOAT, nlocs_dimid, varid_ref) )
call check( nf90_put_att(grpid_obsvalue, varid_ref, "longname", "Atmospheric refractivity" ))
call check( nf90_put_att(grpid_obsvalue, varid_ref, "units", "N" ))
call check( nf90_put_att(grpid_obsvalue, varid_ref, "valid_range", real((/ 0.0, 500.0 /))) )
call check( nf90_def_var_fill(grpid_obsvalue, varid_ref, 0, real(r_missing) ))

call check( nf90_def_var(grpid_obserror, "refractivity", NF90_FLOAT, nlocs_dimid, varid_refoe))
call check( nf90_put_att(grpid_obserror, varid_refoe, "longname", "Input error in atmospheric refractivity" ))
call check( nf90_put_att(grpid_obserror, varid_refoe, "units", "N" ))
call check( nf90_put_att(grpid_obserror, varid_refoe, "valid_range", real((/ 0.0, 10.0 /))) )
call check( nf90_def_var_fill(grpid_obserror, varid_refoe, 0, real(r_missing) ))

call check( nf90_def_var(grpid_metadata, "altitude", NF90_FLOAT, nlocs_dimid, varid_msl) )
call check( nf90_put_att(grpid_metadata, varid_msl, "longname", "Geometric altitude" ))
call check( nf90_put_att(grpid_metadata, varid_msl, "units", "m" ))
call check( nf90_def_var_fill(grpid_metadata, varid_msl, 0, real(r_missing) ))

call check( nf90_def_var(grpid_obsvalue, "bending_angle", NF90_FLOAT, nlocs_dimid, varid_bnd) )
call check( nf90_put_att(grpid_obsvalue, varid_bnd, "longname", "Bending Angle" ))
call check( nf90_put_att(grpid_obsvalue, varid_bnd, "units", "radian" ))
call check( nf90_put_att(grpid_obsvalue, varid_bnd, "valid_range", real((/ -0.001, 0.08 /))) )
call check( nf90_def_var_fill(grpid_obsvalue, varid_bnd, 0, real(r_missing) ))

call check( nf90_def_var(grpid_obserror, "bending_angle", NF90_FLOAT, nlocs_dimid, varid_bndoe) )
call check( nf90_put_att(grpid_obserror, varid_bndoe, "longname", "Input error in Bending Angle" ))
call check( nf90_put_att(grpid_obserror, varid_bndoe, "units", "radian" ))
call check( nf90_put_att(grpid_obserror, varid_bndoe, "valid_range", real((/ 0.0, 0.008 /))) )
call check( nf90_def_var_fill(grpid_obserror, varid_bndoe, 0, real(r_missing) ))

call check( nf90_def_var(grpid_metadata, "impact_parameter", NF90_FLOAT, nlocs_dimid, varid_impp))
call check( nf90_put_att(grpid_metadata, varid_impp, "longname", "distance from centre of curvature" ))
call check( nf90_put_att(grpid_metadata, varid_impp, "units", "m" ))
call check( nf90_put_att(grpid_metadata, varid_impp, "valid_range", real((/ 6200000.0, 6600000.0 /))) )
call check( nf90_def_var_fill(grpid_metadata, varid_impp, 0, real(r_missing) ))


call check( nf90_def_var(grpid_metadata, "impact_height", NF90_FLOAT, nlocs_dimid, varid_imph))
call check( nf90_put_att(grpid_metadata, varid_imph, "longname", "distance from mean sea level" ))
call check( nf90_put_att(grpid_metadata, varid_imph, "units", "m" ))
call check( nf90_put_att(grpid_metadata, varid_imph, "valid_range", real((/ 0.0, 200000.0 /))) )
call check( nf90_def_var_fill(grpid_metadata, varid_imph, 0, real(r_missing) ))

call check( nf90_def_var(grpid_metadata, "sensor_azimuth_angle", NF90_FLOAT, nlocs_dimid, varid_azim))
call check( nf90_put_att(grpid_metadata, varid_azim, "longname", "GNSS->LEO line of sight" ))
call check( nf90_put_att(grpid_metadata, varid_azim, "units", "degree" ))
call check( nf90_put_att(grpid_metadata, varid_azim, "valid_range", real((/ 0.0, 360.0 /))) )
call check( nf90_def_var_fill(grpid_metadata, varid_azim, 0, real(r_missing) ))

call check( nf90_def_var(grpid_metadata, "geoid_height_above_reference_ellipsoid", NF90_FLOAT, nlocs_dimid, varid_geoid))
call check( nf90_put_att(grpid_metadata, varid_geoid, "longname", "Geoid height above WGS-84 ellipsoid" ))
call check( nf90_put_att(grpid_metadata, varid_geoid, "units", "m" ))
call check( nf90_put_att(grpid_metadata, varid_geoid, "valid_range", real((/ -200.0, 200.0 /))) )
call check( nf90_def_var_fill(grpid_metadata, varid_geoid, 0, real(r_missing) ))

call check( nf90_def_var(grpid_metadata, "earth_radius_of_curvature", NF90_FLOAT, nlocs_dimid, varid_rfict))
call check( nf90_put_att(grpid_metadata, varid_rfict, "longname", "Earth’s local radius of curvature" ))
call check( nf90_put_att(grpid_metadata, varid_rfict, "units", "m" ))
call check( nf90_put_att(grpid_metadata, varid_rfict, "valid_range", real((/ 6200000.0, 6600000.0 /))) )
call check( nf90_def_var_fill(grpid_metadata, varid_rfict, 0, real(r_missing) ))
call check( nf90_enddef(ncid) )

call check( nf90_put_var(grpid_obsvalue, varid_ref, gnssro_data%ref(1:ndata)) )
call check( nf90_put_var(grpid_obserror, varid_refoe,gnssro_data%refoe_gsi(1:ndata)) )
call check( nf90_put_var(grpid_obsvalue, varid_bnd,gnssro_data%bend_ang(1:ndata)))
call check( nf90_put_var(grpid_obserror, varid_bndoe,gnssro_data%bndoe_gsi(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_lat, gnssro_data%lat(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_lon, gnssro_data%lon(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_time, gnssro_data%time(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_epochtime,gnssro_data%epochtime(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_recn, gnssro_data%recn(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_said, gnssro_data%said(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_siid, gnssro_data%siid(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_ptid, gnssro_data%ptid(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_sclf, gnssro_data%sclf(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_asce, gnssro_data%asce(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_ogce, gnssro_data%ogce(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_msl,  gnssro_data%msl_alt(1:ndata)))
call check( nf90_put_var(grpid_metadata, varid_impp, gnssro_data%impact_para(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_imph, gnssro_data%impact_para(1:ndata)   &
                                                   - gnssro_data%rfict(1:ndata)         &
                                                   - gnssro_data%geoid(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_azim, gnssro_data%azim(1:ndata)) )
call check( nf90_put_var(grpid_metadata, varid_geoid,gnssro_data%geoid(1:ndata)))
call check( nf90_put_var(grpid_metadata, varid_rfict,gnssro_data%rfict(1:ndata)))
call check( nf90_close(ncid) )

deallocate(gnssro_data%said)
deallocate(gnssro_data%siid)
deallocate(gnssro_data%sclf)
deallocate(gnssro_data%ptid)
deallocate(gnssro_data%recn)
deallocate(gnssro_data%asce)
deallocate(gnssro_data%ogce)
deallocate(gnssro_data%time)
deallocate(gnssro_data%epochtime)
deallocate(gnssro_data%lat)
deallocate(gnssro_data%lon)
deallocate(gnssro_data%rfict)
deallocate(gnssro_data%azim)
deallocate(gnssro_data%geoid)
deallocate(gnssro_data%msl_alt)
deallocate(gnssro_data%ref)
deallocate(gnssro_data%refoe_gsi)
deallocate(gnssro_data%bend_ang)
deallocate(gnssro_data%impact_para)
deallocate(gnssro_data%bndoe_gsi)

end subroutine read_write_gnssro

!contains
 subroutine check(status)
    integer, intent ( in) :: status
    
    if(status /= nf90_noerr) then 
     print *, trim(nf90_strerror(status))
      stop "Stopped"
    end if

  end subroutine check  


subroutine  refractivity_err_gsi(obsLat, obsZ, GlobalModel, obsErr)
real(r_kind),  intent(in)  :: obsLat,  obsZ
real(r_kind),  intent(out) :: obsErr
logical,       intent(in)  :: GlobalModel
real(r_kind)               :: obsZ_km

obsZ_km  = obsZ / 1000.0

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

subroutine  bendingangle_err_gsi(obsLat, obsZ,  obsErr, ogce)
real(r_kind), intent(in)   :: obsLat,  obsZ
integer,        intent(in) :: ogce
real(r_kind), intent(out)  :: obsErr
real(r_kind)               :: obsZ_km

obsZ_km  = obsZ / 1000.0
if((said==41).or.(said==722).or.(said==723).or.(said==42).or.&
                 (said>=3.and.said<=5).or.(said==821.or.(said==421)).or.(said==440).or.(said==43) .or. &
                  (ogce/=60) ) then
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

!-------------------------------------------------------------
! written by H. ZHANG based w3nco_v2.0.6/w3fs21.f and iw3jdn.f
! calculating epoch time since January 1, 1970
SUBROUTINE epochtimecalculator(IDATE, EPOCHTIME)
    INTEGER    IDATE(6)
    INTEGER    NMIN
    INTEGER    IYEAR, NDAYS, IJDN
    INTEGER(8) epochtime
    INTEGER    JDN1970
    DATA  JDN1970 / 2440588 /

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
!   SUBTRACT JULIAN DAY NUMBER OF JAN 1,1970 TO GET THE
!   NUMBER OF DAYS BETWEEN DATES
    NDAYS = IJDN - JDN1970
    NMIN = NDAYS * 1440 + IDATE(4) * 60 + IDATE(5)
    EPOCHTIME = NMIN * 60 + IDATE(6)
END SUBROUTINE epochtimecalculator

!end program
end module gnssro_bufr2ioda
