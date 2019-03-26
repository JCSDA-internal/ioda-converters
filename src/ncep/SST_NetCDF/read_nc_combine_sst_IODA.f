C MAIN PROGRAM: READ_$DATA
C   FORMAT.
C   VALID FOR 3 and 4 Channels Brightness Temperature: VIIRS, AVHRR ,
C   AHI, ABI
C
C USAGE:
C
C   INPUT FILES:
C     UNIT 11  - NETCDF FILE CONTAINING $DATA-ACSPO ?? MINUTE DATA
C
C   OUTPUT FILES:
C     UNIT 25  - .a file
C     UNIT 26  - .b file
C
C   SUBPROGRAMS CALLED:
C     LIBRARY:
C     NETCDF   - NF_OPEN          NF_INQ_VARID    NF_GET_VAR_INT  
C              - NF_INQ_DIMID     NF_INQ_DIMLEN   NF_GET_VAR_REAL
C              - NF_GET_ATT_REAL  NF_GET_VAR_INT2 NF_GET_ATT_INT2
C              - NF_GET_VAR_INT1  NF_GET_ATT_INT1
C   EXIT STATES:
C     COND =   0 - SUCCESSFUL RUN
C          = 100 - UNABLE TO OPEN INPUT NETCDF $DATA 10 MINUTE DATA FILE

C-----------------------------------------------------------------------
      implicit none
      include 'netcdf.inc'

      integer iundef,j,i,k,n_obs_sst,n_obs_sst_m1,n_obs_ist
      integer istat,istat0,istat1,istat2,istat3,istat4,istat5,istat6
      integer istat7,istat8,istat9,istat10,istat11,istat12
      integer istat13,istat14,istat15
      integer istat31,istat32,istat33,istat41,istat42
      integer istat61,istat62,istat63,istat71,istat72
      integer istat81,istat82,istat83,istat101,istat102,istat103
      integer istat111,istat112,istat113,istat121,istat122,istat123
      integer istat131,istat132,istat133,istat141,istat142,istat143
      integer istatidni,istatni,istatidnj,istatnj,imy,imx,idni,idnj
      integer itmp, iyr, mon, iday, ihr, min, isec, Tzone
      character*12 idate
      character*10 runtime
      character*50 FNAME_nc
      character*8  idate0
      character*4  iyear
      integer millisec
      integer iret, err_time, lunin, lundx, lunot, n
      integer ncid, idrtim, idlon, idtim, idlat, idsst
      integer idbias,iderr,idl2pf,idqua,idsza,idbt1,idbt2,idbt3,idbt4
      integer*2 filltmp,filltim,fillbt1,fillbt2,fillbt3,fillbt4
      integer*1 fillbias,fillerr,fillsza
      integer isza_add
      real filllat,filllon
      real factmp,addtmp,factim,facbias,trunc,facsza,addsza
      real facerr,adderr,facbt1,addbt1,facbt2,addbt2,facbt3,addbt3,
     +     facbt4,addbt4

      integer nz, nchan,nchfile,nchlnk,nmax,nobsmx,nobsmxi,istatlist
      integer sid
      integer maxsec,nchtim
      character(len=:), allocatable :: FNAME
      real szamx,avchtim

c     DATA LUNIN /11/
!      DATA LUNOT /12,13/
c     CHARACTER*11 XLFUNIT
c     CHARACTER*150 SWATHIN
      real*4,allocatable :: sst(:,:),sza(:,:)
!SP
      real (kind=8), allocatable :: tim(:,:,:)
!
      real*4,allocatable :: xt(:,:),yt(:,:),bias(:,:)
      real*4,allocatable :: err(:,:),bt1(:,:),bt2(:,:),bt3(:,:),bt4(:,:)
      integer,allocatable :: flag10(:,:),flag12(:,:),flag11(:,:)


      integer*4 irtim
      integer*4 itim1981(8),itimout(8)
      data itim1981/1981,1,1,5*0/
      real*4 rtime(5)
      data rtime/5*0.0/

      integer*2, allocatable :: isst(:,:),itim(:,:),il2pf(:,:)
      integer*2, allocatable :: ibt1(:,:),ibt2(:,:),ibt3(:,:),ibt4(:,:)
      integer*1, allocatable :: ibias(:,:),ierr(:,:),isza(:,:)
      integer*1, allocatable :: iqua(:,:) 
!SP*************************
      real (kind=8), allocatable :: dummy(:,:)
!****************************
c combined variables
c snow and ice
      character*12, allocatable :: dtgdi(:), dtgd(:)
      integer, allocatable :: satiddi(:), satidd(:), typdi(:),typd(:)
      real*4, allocatable :: biasdi(:),errdi(:),latdi(:),
     +                       londi(:), sstdi(:),
     +                       szadi(:),bt1di(:),bt2di(:),bt3di(:),
     +                       bt4di(:)
c not over snow or ice
      real*4, allocatable :: biasd(:),errd(:),latd(:),
     &                       lond(:), sstd(:),
     &                       szad(:),bt1d(:),bt2d(:),bt3d(:),bt4d(:)

      real*4 undef,tmp
c
! SP ************************************************
      real (kind=8), allocatable :: delta_t_hr(:,:), time_hr(:) 
      integer :: nmind_obs, nmind_anl
      integer :: run_yr,run_mon,run_day,run_hr,run_min
!****************************************************
c
c nz 0 for surface
c nchan 3 or 4 
c nchfile number of characters in file name
c nmax maximum number of nc files
c nobsmx maximum number of observations
c nobsmxi maximum number of observations over snow and ice
c maxsec time span of the nc file in seconds (10min or 1hr in sec)
c sid satellite id
c szamx maximum zenith angle
      namelist /satnl/nz,nchan,nchfile,nmax,nobsmx,nobsmxi
     +                ,maxsec,runtime,sid,szamx
      nz=0
      nobsmx=250000000
      nobsmxi=100000
      szamx=90.
      open (7,file='osatnl',form='formatted', status='old')
      read(7,NML=satnl)
      close(7)
      write(6,*)'sid',sid
      if(sid.lt.0) STOP 'sid has to be >0'
      write(6,*)'szamx', szamx
      if(szamx.gt.90..or.szamx.lt.0.) then
       write(6,*)'szamx should be between 0. and 90. deg'
       stop
      endif
      write(*,*) "runtime = ",runtime
      if (nz.lt.0) STOP 'nz has to be >=0'
      if (nchan.lt.3.or.nchan.gt.4) STOP 'nchan has to be 3 or 4'
      write(6,*) nchfile
      if(nmax.le.0) STOP 'nmax, n of files, has to be > 0'
      if(nobsmx.le.0) STOP 'nobsmx has to be >0' 
      if(nobsmxi.le.0) STOP 'nobsmxi has to be >0' 
      if(maxsec.le.0) STOP 'file time span in sec has to be >0'

      allocate (character (len=nchfile) :: FNAME)

c snow and ice
      allocate (biasdi(nobsmxi))
      allocate (dtgdi(nobsmxi))
      allocate (errdi(nobsmxi))
      allocate (latdi(nobsmxi))
      allocate (londi(nobsmxi))
      allocate (satiddi(nobsmxi))
      allocate (typdi(nobsmxi))
      allocate (sstdi(nobsmxi))
      allocate (szadi(nobsmxi))
      allocate (bt1di(nobsmxi))
      allocate (bt2di(nobsmxi))
      allocate (bt3di(nobsmxi))
      if (nchan.eq.4) 
     +allocate (bt4di(nobsmxi))
c not over snow or ice
      allocate (biasd(nobsmx))
      allocate (dtgd(nobsmx))
      allocate (errd(nobsmx))
      allocate (latd(nobsmx))
      allocate (lond(nobsmx))
! SP
      allocate (time_hr(nobsmx))
!
      allocate (satidd(nobsmx))
      allocate (typd(nobsmx))
      allocate (sstd(nobsmx))
      allocate (szad(nobsmx))
      allocate (bt1d(nobsmx))
      allocate (bt2d(nobsmx))
      allocate (bt3d(nobsmx))
      if (nchan.eq.4) 
     +allocate (bt4d(nobsmx))



      n_obs_sst = 0
      n_obs_ist = 0
      n_obs_sst_m1 = 0
      nchtim=0
      avchtim=0
      open(8,file='filenclist',form='formatted',status='old')
c cycle over files
      do n=1,nmax
      write(6,*)'file number',n
        read(8,'(a)',iostat=istatlist)FNAME
        write(6,*)'FNAME',FNAME
        if (istatlist.ne.0) then
        write(6,*)'error reading file list', istatlist
        stop
        endif
c

      istat = nf_open (FNAME,0,ncid)
      print *,'istat = ',istat
      if (istat.ne.0) then
        print *,'file ',FNAME,' could not be opened: stop 100'
        stop 100
      endif
      print *,'ncid = ',ncid
      istat0 = nf_inq_varid (ncid,'time',idrtim) 
      istat1 = nf_inq_varid (ncid,'lon',idlon)
      istat2 = nf_inq_varid (ncid,'lat',idlat)
      istat3 = nf_inq_varid (ncid,'sea_surface_temperature',idsst)
      istat4 = nf_inq_varid (ncid,'sst_dtime',idtim)
!      istat5 = nf_inq_varid (ncid,'proximity_confidence',idprox)
!      istat6 = nf_inq_varid (ncid,'wind_speed',idwind)
      istat7 = nf_inq_varid (ncid,'sses_bias',idbias)
      istat8 = nf_inq_varid (ncid,'sses_standard_deviation',iderr)
      istat9 = nf_inq_varid (ncid,'l2p_flags',idl2pf)
      istat10= nf_inq_varid (ncid,'satellite_zenith_angle',idsza)
      if (nchan.eq.3) then
      istat11= nf_inq_varid (ncid,'brightness_temperature_11um',idbt1)
      istat12= nf_inq_varid (ncid,'brightness_temperature_12um',idbt2)
      istat13= nf_inq_varid (ncid,'brightness_temperature_4um',idbt3)
      else if (nchan.eq.4) then
      istat11= nf_inq_varid (ncid,'brightness_temperature_08um6',idbt1)
      istat12= nf_inq_varid (ncid,'brightness_temperature_10um4',idbt2)
      istat13= nf_inq_varid (ncid,'brightness_temperature_11um2',idbt3)
      istat14= nf_inq_varid (ncid,'brightness_temperature_12um3',idbt4)
      endif
      istat15= nf_inq_varid (ncid,'quality_level',idqua)

      print *,'id rtime =',idrtim,'id lon =',idlon,'  id lat =',idlat
      print *,'id sst =',idsst,'id time =',idtim
!      print *,'id bias =',idbias,'id l2pf =',idl2pf,'id error =',iderr

      if (istat0.ne.0.or.istat1.ne.0.or.istat2.ne.0.or.istat3.ne.0) then
        print *,'status not 0:',istat0,istat1,istat2,istat3,' stop 101'
        stop 101
      endif

      if (nchan.eq.3) then
      if (istat4.ne.0.or.istat7.ne.0.or.istat8.ne.0.or. 
     +    istat9.ne.0.or.istat10.ne.0.or.istat11.ne.0.or.
     +    istat12.ne.0.or.istat13.ne.0.or.istat15.ne.0) then
        print *,'status not 0:',istat4,istat7,istat8,istat9, 
     +    istat10,istat11,istat12,istat13,istat15,' stop 102'
        stop 102
      endif !istat
      else if (nchan.eq.4) then
      if (istat4.ne.0.or.istat7.ne.0.or.istat8.ne.0.or. 
     +    istat9.ne.0.or.istat10.ne.0.or.istat11.ne.0.or.
     +    istat12.ne.0.or.istat13.ne.0.or.istat15.ne.0) then
        print *,'status not 0:',istat4,istat7,istat8,istat9, 
     +    istat10,istat11,istat12,istat13,istat14,istat15,' stop 102'
        stop 102
      endif !istat
      endif !nchan
c
      istat0 = nf_get_var_int (ncid, idrtim, irtim) ! time - reference time of SST file
      if (istat0.ne.0) then
        print *,'irtim: istat0 not 0:',istat0,' stop 103'
        stop 103
      endif
      print *,'time(seconds) of SST data (time_val) = ',irtim
c

      istatidni = nf_inq_dimid(ncid, 'ni', idni)
      print*,'idni ',idni
      istatni = nf_inq_dimlen(ncid, idni, imx)
      print*,'istatni is ',istatni,'  idni is ',idni
      print*,'imx is ',imx
      istatidnj = nf_inq_dimid(ncid, 'nj', idnj)
      istatnj = nf_inq_dimlen(ncid, idnj, imy)
      print*,'istatnj is ',istatnj,'  idnj is ',idnj
      print*,'imy is ',imy
! 2-D
      allocate (xt(imy,imx))
      allocate (yt(imy,imx))
      allocate (sst(imy,imx))
      allocate (bias(imy,imx))
      allocate (err(imy,imx))
      allocate (sza(imy,imx))
      allocate (bt1(imy,imx))
      allocate (bt2(imy,imx))
      allocate (bt3(imy,imx))
      if (nchan.eq.4) 
     +allocate (bt4(imy,imx))
      allocate (flag10(imy,imx))
      allocate (flag11(imy,imx))
      allocate (flag12(imy,imx))
      
!SFl      allocate (tim(imy,imx,4))
      allocate (tim(imy,imx,5))
      allocate (isst(imy,imx))
      allocate (itim(imy,imx))
      allocate (ibias(imy,imx))
      allocate (ierr(imy,imx))
      allocate (il2pf(imy,imx))
      allocate (isza(imy,imx))
      allocate (ibt1(imy,imx))
      allocate (ibt2(imy,imx))
      allocate (ibt3(imy,imx))
      if(nchan.eq.4)
     +allocate (ibt4(imy,imx))
      allocate (iqua(imy,imx))
!SP *********************************
      allocate (delta_t_hr(imy,imx))
      allocate (dummy(imy,imx))
!************************************

!---- latitude ----
      istat1 = nf_get_var_real (ncid, idlat, yt)
      if (istat1.ne.0) then
        print *,'alat: istat1 not 0:',istat1,' stop 103'
        stop 103
      endif
       print *,'yt(1,1) = ',yt(1,1)
       print *,'yt(imy,imx) = ',yt(imy,imx)
!---- longitude ----
      istat2 = nf_get_var_real (ncid, idlon, xt)
      if (istat2.ne.0) then
        print *,'alon: istat2 not 0:',istat2,' stop 103'
        stop 103
      endif
      print *,'xt(1,1) = ',xt(1,1)
      print *,'xt(imy,imx) = ',xt(imy,imx)
!---- sst --------------------------------------------------------
      print *,' '
      istat3 = nf_get_var_int2 (ncid, idsst, isst)
      if (istat3.ne.0) then
        print *,'isst: istat3 not 0:',istat3,' stop 103'
        stop 103
      endif
!---- sst scale factor ----
      istat31 = nf_get_att_real(ncid,idsst,'scale_factor',factmp)
      if (istat31.ne.0) then
        print *,'isst: istat31 not 0:',istat31,' stop 103'
        stop 103
      endif
!---- sst offset -----
      istat32 = nf_get_att_real(ncid,idsst,'add_offset',addtmp)
      if (istat32.ne.0) then
        print *,'isst: istat31 not 0:',istat32,' stop 103'
        stop 103
      endif
!---- sst fill value ----
      istat33 = nf_get_att_int2(ncid,idsst,'_FillValue',filltmp)
      if (istat33.ne.0) then
        print *,'isst: istat33 not 0:',istat33,' stop 103'
        stop 103
      endif
!---- time -------------------------------------------------
      istat4 = nf_get_var_int2 (ncid, idtim, itim) !sst_dtime
      if (istat4.ne.0) then
        print *,'itim: istat4 not 0:',istat4,' stop 103'
        stop 103
      endif
      istat41 = nf_get_att_real(ncid,idtim,'scale_factor',factim)
      if (istat41.ne.0) then
        print *,'itim: istat41 not 0:',istat41,' stop 103'
        stop 103
      endif
      istat42 = nf_get_att_int2(ncid,idtim,'_FillValue',filltim)
      if (istat42.ne.0) then
        print *,'itim: istat42 not 0:',istat42,' stop 103'
        stop 103
      endif

!--- Skip Wind
!---------------------------------------------------------

!---- Bias ------------------------------------------------
      istat7 = nf_get_var_int1 (ncid, idbias, ibias)
      if (istat7.ne.0) then
        print *,'ibias: istat7 not 0:',istat7,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat71 = nf_get_att_real(ncid,idbias,'scale_factor',facbias)
      if (istat71.ne.0) then
        print *,'ibias: istat71 not 0:',istat71,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat72 = nf_get_att_int1(ncid,idbias,'_FillValue',fillbias)
      if (istat72.ne.0) then
        print *,'ibias: istat72 not 0:',istat72,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif

!----- error (standard deviation)----------------------------------
      istat8 = nf_get_var_int1 (ncid, iderr, ierr)
      if (istat8.ne.0) then
        print *,'ierr: istat8 not 0:',istat8,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat81 = nf_get_att_real(ncid,iderr,'scale_factor',facerr)
      if (istat81.ne.0) then
        print *,'ierr: istat81 not 0:',istat81,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat82 = nf_get_att_real(ncid,iderr,'add_offset',adderr)
      if (istat82.ne.0) then
        print *,'ierr: istat82 not 0:',istat82,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat83 = nf_get_att_int1(ncid,iderr,'_FillValue',fillerr)
      if (istat83.ne.0) then
        print *,'ierr: istat83 not 0:',istat83,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif

! ----  Flags ------------------------------------------------------
      istat9 = nf_get_var_int2 (ncid, idl2pf, il2pf)
      if (istat9.ne.0) then
        print *,'il2pf: istat9 not 0:',istat9,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
!---- Satellite Zenith Angle ---------------------------------------
      istat10 = nf_get_var_int1 (ncid, idsza, isza)
      if (istat10.ne.0) then
        print *,'isza: istat10 not 0:',istat10,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
!---- sza scale factor ----
      istat101 = nf_get_att_real(ncid,idsza,'scale_factor',facsza)
      if (istat101.ne.0) then
        print *,'isza: istat101 not 0:',istat101,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
!---- sza offset -----
      istat102 = nf_get_att_real(ncid,idsza,'add_offset',addsza)
      if (istat102.ne.0) then
        print *,'isza: istat102 not 0:',istat102,' stop 103'
        stop 103
      endif
!---- sza fill value ----
      istat103 = nf_get_att_int1(ncid,idsza,'_FillValue',fillsza)
      if (istat103.ne.0) then
        print *,'isza: istat103 not 0:',istat103,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif

!---- Brightness Temperature 11um (3chan) or 8.6um (4chan)----------
      istat11 = nf_get_var_int2 (ncid, idbt1, ibt1)
      if (istat11.ne.0) then
        print *,'ibt1: istat11 not 0:',idbt1,istat11,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat111 = nf_get_att_real(ncid,idbt1,'scale_factor',facbt1)
      if (istat111.ne.0) then
        print *,'ibt1: istat111 not 0:',istat111,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat112 = nf_get_att_real(ncid,idbt1,'add_offset',addbt1)
      if (istat112.ne.0) then
        print *,'ibt1: istat112 not 0:',istat112,' stop 103'
        stop 103
      endif
      istat113 = nf_get_att_int2(ncid,idbt1,'_FillValue',fillbt1)
      if (istat113.ne.0) then
        print *,'ibt1: istat113 not 0:',istat113,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
!        stop 103
      endif
!---- Brightness Temperature 12um (3chan) or 10.4um (4chan)---------
      istat12 = nf_get_var_int2 (ncid, idbt2, ibt2)
      if (istat12.ne.0) then
        print *,'ibt2: istat12 not 0:',istat12,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat121 = nf_get_att_real(ncid,idbt2,'scale_factor',facbt2)
      if (istat121.ne.0) then
        print *,'ibt2: istat121 not 0:',istat121,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat122 = nf_get_att_real(ncid,idbt2,'add_offset',addbt2)
      if (istat122.ne.0) then
        print *,'ibt2: istat122 not 0:',istat122,' stop 103'
        stop 103
      endif
      istat123 = nf_get_att_int2(ncid,idbt2,'_FillValue',fillbt2)
      if (istat123.ne.0) then
        print *,'ibt2: istat123 not 0:',istat123,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
!---- Brightness Temperature  4um (3chan) or 11.2um (4chan)---------
      istat13 = nf_get_var_int2 (ncid, idbt3, ibt3)
      if (istat13.ne.0) then
        print *,'ibt3: istat13 not 0:',istat13,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat131 = nf_get_att_real(ncid,idbt3,'scale_factor',facbt3)
      if (istat131.ne.0) then
        print *,'ibt3: istat131 not 0:',istat131,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat132 = nf_get_att_real(ncid,idbt3,'add_offset',addbt3)
      if (istat132.ne.0) then
        print *,'ibt3: istat132 not 0:',istat132,' stop 103'
        stop 103
      endif
      istat133 = nf_get_att_int2(ncid,idbt3,'_FillValue',fillbt3)
      if (istat133.ne.0) then
        print *,'ibt3: istat133 not 0:',istat133,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      if (nchan.eq.4)then
!---- Brightness Temperature  12.3um ----------------------------------
      istat14 = nf_get_var_int2 (ncid, idbt4, ibt4)
      if (istat14.ne.0) then
        print *,'ibt4: istat14 not 0:',istat14,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat141 = nf_get_att_real(ncid,idbt4,'scale_factor',facbt4)
      if (istat141.ne.0) then
        print *,'ibt4: istat141 not 0:',istat141,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      istat142 = nf_get_att_real(ncid,idbt4,'add_offset',addbt4)
      if (istat142.ne.0) then
        print *,'ibt4: istat142 not 0:',istat142,' stop 103'
        stop 103
      endif
      istat143 = nf_get_att_int2(ncid,idbt4,'_FillValue',fillbt4)
      if (istat143.ne.0) then
        print *,'ibt4: istat143 not 0:',istat143,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif
      endif !nchan
!----  Quality level -----------------------------------------------
      istat15 = nf_get_var_int1 (ncid, idqua, iqua)
      if (istat15.ne.0) then
        print *,'iqua: istat15 not 0:',istat15,' stop 103'
c        CALL W3TAGE('READ_VIIRS10MIN')
        stop 103
      endif      

!--- close nc file
      istat=nf_close(ncid)
      write(6,*)'close nc file', istat

! scan time loop, here time =1, so k=1
!---Apply flags and quality_level to SST ---------------------------------------------
!  --- flag(bit2) = 0 ==> ocean, =1 ==> land
!  --- flag(bit3) = 0 ==> no ice, =1 ==> ice
!  --- flag(bit4-8) = 0 ==> reserved, set to 0
!  --- flag(bit9) = 0 ==> radiance valid , =1 ==> not valid
!  --- flag(bit10) = 0 ==> night, =1 ==> day
!  --- flag(bit11) = 0 ==> ocean, =1 ==> land
!  --- flag(bit12) = 0 ==> good quality, =1 ==> degraded due to twilight
!  --- flag(bit13) = 0 ==> no glint, =1 ==> glint
!  --- flag(bit14) = 0 ==> no snow/ice, =1 ==> snow/ice
!  --- flag(bit15,16)=00(clear),01(probably
!  clear),10(cloudy),11(undefined)

c     undef = real(filltmp)
      undef = real(filltmp)
        do j=1,imy
         do i=1,imx
           if (isst(j,i).eq.filltmp .or. iqua(j,i).ne.5) then
              sst(j,i) = undef
           else
              tmp = isst(j,i)
!              sst(j,i) = factmp*tmp+addtmp !calculate sst in K
              sst(j,i) = factmp*tmp !calculate sst in C
           endif
         enddo
        enddo
         call maxmin (sst,imy,imx,undef,'sst net ')
!---- Calculate days, hour, min, sec from reference time
!----------------------

! tim(:,:,1) is days, tim(:,:,2) is hour, tim(:,:,3) is min, tim(:,:,4)
! is sec  
        undef = real(filltim)
        do j=1,imy
         do i=1,imx
          if (itim(j,i).eq.filltim) then
             tim(j,i,1) = undef
             tim(j,i,2) = undef
             tim(j,i,3) = undef
             tim(j,i,4) = undef
             tim(j,i,5) = undef
          else
             if(itim(j,i)*factim.ge.maxsec) then
             nchtim=nchtim+1
             avchtim=int((maxsec-1)/factim)-itim(j,i)
c              write(6,*)'time changed from to '
c    +         ,i,j,itim(j,i),int((maxsec-1)/factim)
               itim(j,i)=int((maxsec-1)/factim)
             endif
             itmp = irtim + int(factim * itim(j,i))
             trunc = factim * itim(j,i) - int(factim * itim(j,i))


             tim(j,i,4) = real(mod(itmp,60)) + trunc
             itmp = int(itmp / 60)            !Seconds
             tim(j,i,3) = real(mod(itmp,60))
             itmp = itmp / 60               !Minutes    
!           tim(j,i,2) = real(mod(itmp,24))
           tim(j,i,2) = irtim + int(factim * itim(j,i))
           tim(j,i,1) = real(itmp / 24)   !Days
        endif
       enddo
      enddo
c      write(*,*) 'time is: ',tim(1000,:,2)
      write(*,'(A,I20,1x,f20.2,I10,f20.2)') 'Reference time of SST file:
     +', irtim, factim,itim(j,i), tim(j,i,2)
      
!--------- Calculate real bias ----------------------------------------
      undef = real(fillbias)
      do j=1,imy
       do i=1,imx
        if (ibias(j,i).eq.fillbias) then
           bias(j,i) = undef
        else
           tmp = ibias(j,i)
           bias(j,i) = facbias*tmp
        endif
       enddo
      enddo
       call maxmin (bias,imy,imx,undef,'bias net ')

!--------- Calculate real error ----------------------------------------
      undef = real(fillerr)
      do j=1,imy
       do i=1,imx
        if (ierr(j,i).eq.fillerr) then
           err(j,i) = undef
        else
           tmp = ierr(j,i)
           err(j,i) = facerr*tmp+adderr
        endif
       enddo
      enddo
       call maxmin (err,imy,imx,undef,'err net ')
!--------- Calculate real sza ----------------------------------------
      undef = real(fillsza)
      do j=1,imy
       do i=1,imx
        if (isza(j,i).eq.fillsza) then
           sza(j,i) = undef
        else
           tmp = isza(j,i)
           sza(j,i) = facsza*tmp+addsza * isza_add
           if(abs(sza(j,i)).gt.szamx) sza(j,i) = undef
        endif
       enddo
      enddo
       write(6,*)'addsza',addsza, 'facsza', facsza
       call maxmin (sza,imy,imx,undef,'sza net ')
!--------- Calculate real bt1 ---------   
      undef = real(fillbt1)
      do j=1,imy
       do i=1,imx
        if (ibt1(j,i).eq.fillbt1) then
           bt1(j,i) = undef
        else
           tmp = ibt1(j,i)
           bt1(j,i) = facbt1*tmp+addbt1
        endif
       enddo
      enddo
       call maxmin (bt1,imy,imx,undef,'bt1 net ')
!--------- Calculate real bt2 ----------------------------------------
      undef = real(fillbt2)
      do j=1,imy
       do i=1,imx
        if (ibt2(j,i).eq.fillbt2) then
           bt2(j,i) = undef
        else
           tmp = ibt2(j,i)
           bt2(j,i) = facbt2*tmp+addbt2
        endif
       enddo
      enddo
       call maxmin (bt2,imy,imx,undef,'bt2 net ')
!--------- Calculate real bt3 ----------------------------------------
      undef = real(fillbt3)
      do j=1,imy
       do i=1,imx
        if (ibt3(j,i).eq.fillbt3) then
           bt3(j,i) = undef
        else
           tmp = ibt3(j,i)
           bt3(j,i) = facbt3*tmp+addbt3
        endif
       enddo
      enddo
      if(nchan.eq.4)then
!--------- Calculate real bt4 ----------------------------------------
      undef = real(fillbt4)
      do j=1,imy
       do i=1,imx
        if (ibt4(j,i).eq.fillbt4) then
           bt4(j,i) = undef
        else
           tmp = ibt4(j,i)
           bt4(j,i) = facbt4*tmp+addbt4
        endif
       enddo
      enddo
       call maxmin (bt4,imy,imx,undef,'bt4 net ')
      endif !nchan
!------- Flag 10 and Flag 12-------------------------------------------
      do j=1,imy
       do i=1,imx
! test bit-----------------------------------------------------------
        if(btest(il2pf(j,i),9) .eq. .true.) then  !Fortran bit 0-15
          flag10(j,i)=1 !Day
        else
          flag10(j,i)=0 !Night
        endif 
        if(btest(il2pf(j,i),10) .eq. .true.) then
          flag11(j,i)=1 !land
        else
          flag11(j,i)=0 !ocean
        endif
        if(btest(il2pf(j,i),11) .eq. .true.) then
          flag12(j,i)=1 !twilight
        else
          flag12(j,i)=0 !no twilight
        endif
       enddo
      enddo
!---- Calculate idate -------------------------------------------------
      do j=1,imy
       do i=1,imx
         if(itim(j,i).eq.filltim .or. sst(j,i).eq.real(filltmp).
     &   or. flag11(j,i).eq.1 .or. sza(j,i).eq.real(fillsza) ) then
c         write (78,*) 'skipping: ',tim(j,i,:),sst(j,i)
         cycle
         endif
         if(ibias(j,i).eq.fillbias .or. ierr(j,i).eq.fillerr) then
!          write (*,*) 'skipping bias and err null:'
          cycle
         endif
c         rtime(1:4) = tim(j,i,1:4)
c discard seconds
! SFl          rtime(1:3) = tim(j,i,1:3)
! SFl          rtime(4) = 0
! SFl          rtime(5) = 0
          call w3movdat(rtime,itim1981,itimout)
          IYR = itimout(1)
          MON = itimout(2)
          IDAY = itimout(3)
          Tzone = itimout(4)  !Time Zone
          IHR = itimout(5)
          MIN = itimout(6)
          ISEC = itimout(7)
          MILLISEC = itimout(8)
!SP
          read(runtime(1:4), *) run_yr
          read(runtime(5:6), *) run_mon
          read(runtime(7:8), *) run_day
          read(runtime(9:10), *) run_hr
          run_min = 0
!          read(runtime(11:12), *) iMin

!          print *, 'reading runDate'
!          print *, run_yr, run_mon, run_day, run_hr, run_min
!

          call w3fs21(itimout, nmind_obs) !NUMBER OF MINUTES of SINCE JAN 1, 1978 
        call w3fs21((/run_yr,run_mon,run_day,run_hr,run_min/),nmind_anl) !NUMBER OF MINUTES analysis time SINCE JAN 1, 1978
          dummy(j,i) = factim * itim(j,i) ! seconds since 1981-01-01 00:00:00 UTC
!          write(*,*) 'Dummy=',dummy(j,i), factim, itim(j,i)
          tim(j,i,2) = float(irtim) + dummy(j,i)
!          write(*,'(f10.2,1x,I10,f20.2)') dummy(j,i), irtim, tim(j,i,2)  
          delta_t_hr(j,i) = ((tim(j,i,2)+(nmind_obs*60.0))
     +                      -(nmind_anl*60.0))/3600.0
!          write(*,'(3f20.2)') tim(j,i,2), nmind_obs*60.0,
!     +                        nmind_anl*60.0
!          write(*,'(3f20.2)') nmind_obs*60.0, nmind_anl*60.0, 
!     +                        delta_t_hr(j,i)
!          write(*,*) IYR,MON,IDAY,Tzone,IHR,MIN,ISEC,MILLISEC
!          IDATE = IYR*100000000+MON*1000000+IDAY*10000+IHR*100+MIN
          if (MON*1000000+IDAY*10000+IHR*100+MIN.lt.10000000) then
            write(IDATE0,'(i7)') MON*1000000+IDAY*10000+IHR*100+MIN
            idate0='0'//idate0
          else
            write(IDATE0,'(i8)') MON*1000000+IDAY*10000+IHR*100+MIN
          endif
          write(iyear,'(i4)') iyr
          idate=iyear//idate0
          ERR_time = 0
          IF(IYR.LT.0 .OR. IYR.GT.9999 .OR.
     +       MON.LT.1 .OR. MON.GT.12 .OR.
     +       IDAY.LT.1 .OR. IDAY.GT.31 .OR.
     +       IHR.LT.0 .OR. IHR.GT.24 .OR.
     +       MIN.LT.0 .OR. MIN.GT.60 .OR.
     +       ISEC.LT.0 .OR. ISEC.GT.60 .OR.
     +       MILLISEC.LT.0 .OR. MILLISEC.GT.999) THEN
             write(*,*)'BAD DATE',
     .               IYR,MON,IDAY,IHR,MIN,ISEC,MILLISEC
             ERR_time = 1
          ENDIF

c bt3, bt1, bt2,  brightness temp
c flag10 1 Day, flag12 1 twilight 
c skipped itim, sst, sza fill values, flag11=1(land), bad date,
c sza larger than max given value
         if(ERR_time .eq. 0) then
           if(btest(il2pf(j,i),13) .eq. .true.) then ! snow ice IST
c            write(12) idate,yt(j,i),xt(j,i),sst(j,i),
c    +        bt3(j,i),bt1(j,i),bt2(j,i),sza(j,i),bias(j,i),err(j,i),
c    +        flag10(j,i),flag12(j,i)
             if (abs(sza(j,i)).le.szamx) then !accept obs with sza <= szamx
               n_obs_ist = n_obs_ist + 1
c              if(n_obs_ist.gt.0.and.mod(n_obs_ist,1000).eq.0)
c    +           write(6,*)'n_obs_ist',n_obs_ist
               biasdi(n_obs_ist)=bias(j,i)
               dtgdi(n_obs_ist)=idate     
               errdi(n_obs_ist)=err(j,i)
               latdi(n_obs_ist)=yt(j,i)
               londi(n_obs_ist)=xt(j,i)
               satiddi(n_obs_ist)=sid
               if (flag10(j,i).eq.1) then 
                typdi(n_obs_ist)=151 !day
               else
                typdi(n_obs_ist)=152 !night
               endif
               if (flag12(j,i).eq.1) typdi(n_obs_ist)=159 !twylight
               sstdi(n_obs_ist)=sst(j,i)
               szadi(n_obs_ist)=sza(j,i)
               if (nchan.eq.3) then
               bt1di(n_obs_ist)=bt3(j,i)
               bt2di(n_obs_ist)=bt1(j,i)
               bt3di(n_obs_ist)=bt2(j,i)
               else if(nchan.eq.4) then
               bt1di(n_obs_ist)=bt1(j,i)
               bt2di(n_obs_ist)=bt2(j,i)
               bt3di(n_obs_ist)=bt3(j,i)
               bt4di(n_obs_ist)=bt4(j,i)
               endif !nchan
             endif
           else   ! no snow/ice  SST
             if (abs(sza(j,i)).le.szamx) then !accept obs with abs(sza) <= szamx
               n_obs_sst = n_obs_sst + 1
               if (n_obs_sst.gt.nobsmx) STOP 'increase nobsmx'
               biasd(n_obs_sst)=bias(j,i)
               dtgd(n_obs_sst)=idate     
               errd(n_obs_sst)=err(j,i)
               latd(n_obs_sst)=yt(j,i)
               lond(n_obs_sst)=xt(j,i)
               time_hr(n_obs_sst)=delta_t_hr(j,i)
!               write(*,*) 'IODA time=',time_hr(n_obs_sst)
               satidd(n_obs_sst)=sid
               if (flag10(j,i).eq.1) then 
                typd(n_obs_sst)=151 !day
               else
                typd(n_obs_sst)=152 !night
               endif
               if (flag12(j,i).eq.1) typd(n_obs_sst)=159 !twylight
               sstd(n_obs_sst)=sst(j,i)
               szad(n_obs_sst)=sza(j,i)
               if (nchan.eq.3) then
               bt1d(n_obs_sst)=bt3(j,i)
               bt2d(n_obs_sst)=bt1(j,i)
               bt3d(n_obs_sst)=bt2(j,i)
               else if (nchan.eq.4) then
               bt1d(n_obs_sst)=bt1(j,i)
               bt2d(n_obs_sst)=bt2(j,i)
               bt3d(n_obs_sst)=bt3(j,i)
               bt4d(n_obs_sst)=bt4(j,i)
               endif !nchan
             endif

           endif
         endif
        enddo ! i
      enddo ! j
         write(6,'(a,i5,2i7,2i10)')'n,j,i,n_obs_sst, nobs this file'
     +              ,n,j,i,n_obs_sst,n_obs_sst-n_obs_sst_m1
         if (nchtim.gt.0)then 
         write(6,*)'time changed total and avg', nchtim, avchtim/nchtim
         write(15,*)'n,j,i,n_obs_sst, nobs, nchtim, avchtim, this file'
     + ,n,j,i,n_obs_sst,n_obs_sst-n_obs_sst_m1,nchtim,avchtim/nchtim
         else
         write(15,*)'n,j,i,n_obs_sst, nobs this file'
     + ,n,j,i,n_obs_sst,n_obs_sst-n_obs_sst_m1,nchtim
         endif
         n_obs_sst_m1=n_obs_sst


! 2-D
      deallocate (xt)
      deallocate (yt)
      deallocate (sst)
      deallocate (bias)
      deallocate (err)
      deallocate (sza)
      deallocate (bt1)
      deallocate (bt2)
      deallocate (bt3)
      if (nchan.eq.4) 
     +deallocate (bt4)
      deallocate (flag10)
      deallocate (flag11)
      deallocate (flag12)
      
      deallocate (tim)
      deallocate (isst)
      deallocate (itim)
      deallocate (ibias)
      deallocate (ierr)
      deallocate (il2pf)
      deallocate (isza)
      deallocate (ibt1)
      deallocate (ibt2)
      deallocate (ibt3)
      if(nchan.eq.4)
     +deallocate (ibt4)
      deallocate (iqua)

!SP****************************
      deallocate(delta_t_hr)
      deallocate(dummy)
!      deallocate(time_hr)
!******************************

      enddo ! n cycle over files
      write(6,*)'number and avg change time', nchtim, avchtim/nchtim
      
!  write out $data.ist.a.${ymdhm} file
!      if (n_obs_ist.gt.0) then
!       open(16,form='formatted',access='sequential',status='unknown')
!       write(16,'(3i10, 2x, a)',err=920) n_obs_ist,nz,nchan,idate(1:10)
!       close(16)
!      endif
! write out $data.sst.a.${ymdhm} file

      if (n_obs_sst.gt.0) then
         print *, "Writing IODA SST NC4 file"
!         write(*,*) 'time=',time_hr
         FNAME_nc ='sst-obs-'//runtime(1:4)//'-'//runtime(5:6)//'-'//
     +             runtime(7:8)//'.nc'
!         write(*,*) FNAME_nc
         call IODA_sst_nc4( FNAME_nc,n_obs_sst,lond,latd,runtime,sstd,
     +                      errd,time_hr )
      endif         
C

      PRINT*,'*** PROCESSING ENDED NORMALLY ***'
      stop
910   write(*,*) "error reading file list"
920   write(*,*) "error writing SST file"
      end

      subroutine maxmin (fld,jm,im,undef,name)
c
c   compute maximum, minium and average of fld and print
c
      dimension fld(jm,im)
      character*8 name
      real*8 ave
      fldmin = 1.e30
      fldmax = -fldmin
      inum = 0
      ave = 0.
      do 90 i=1,im
      do 90 j=1,jm
      if (fld(j,i).ne.undef) then
        inum = inum + 1
        ave = ave + fld(j,i)
        if (fldmin.gt.fld(j,i))  fldmin = fld(j,i)
        if (fldmax.lt.fld(j,i))  fldmax = fld(j,i)
      endif
   90 continue
      if (inum.gt.0) then
        r = 1./float(inum)
        ave = ave*r
      else
        fldmin = 0.
        fldmax = 0.
      endif
      print 88,ave,fldmin,fldmax,inum,name
   88 format (' ave =',f10.3,2x,'min =',f10.3,2x,
     1 'max =',f10.3,2x,'inum =',i8,2x,a8)
      return
      end

!*************************************************************************************
! IODA netcdf4 
!************************************************************************************
      subroutine IODA_sst_nc4 (FNAME_nc,nobs,lon_data,lat_data,runtime, 
     +                         obsValue_data, obsError_data, time_data) 
!, oceanUpLvlTmp_data) TO BE ADDED LATER
      use netcdf

      character (len=10) :: runtime
      character (len=50) :: FNAME_nc
      character (len=50) :: NLOCS_NAME = "nlocs"
      character (len=50) :: NRECS_NAME = "nrecs"
      character (len=50) :: NVARS_NAME = "nvars"
      character (len=50) :: NOBS_NAME  = "nobs"
      character (len=50), parameter :: NLEV_NAME = "nlev"
      character (len=50) :: Station_ID_maxstrlen_NAME = 
     + "Station_ID_maxstrlen"
      character (len=50), parameter :: longitude_NAME = "longitude"
      character (len=50), parameter :: latitude_NAME = "latitude"
      character (len=50), parameter :: time_NAME = "time"
      character (len=50), parameter :: obs_sstObsValue_NAME =
     + "obs_sst@ObsValue"
      character (len=50), parameter :: obs_sstObsError_NAME = 
     + "obs_sst@ObsError"
      character (len=50), parameter :: ocean_UpLVL_Temp_NAME = 
     + "ocean_upper_level_temperature"

      integer :: ncid, status, nobs
      integer, parameter :: nrecs=1, nvars=1, nlev=1
      real :: lon_data(nobs), lat_data(nobs), obsValue_data(nobs), 
     +        time_data_real(nobs),obsError_data(nobs), 
     +        oceanUpLvlTmp_data(nobs, nlev)
      real*8  :: time_data (nobs)

! Dimensions
      integer :: nlocsID, Station_ID_maxstrlenID, nrecsID, nvarsID,
     + nobsID, nlevID

! Variables
      integer :: lon_varID, lat_varID, time_varID, obsValue_varID,
     + obsError_varID, oceanUpLvlTmp_varID

! Create netCDF file
      call check ( nf90_create (FNAME_nc, nf90_clobber, ncid) )

! Create Dimensions
      call check ( nf90_def_dim(ncid, NLOCS_NAME, NF90_UNLIMITED,
     + nlocsID) )
      call check ( nf90_def_dim (ncid, Station_ID_maxstrlen_NAME, 8,
     + Station_ID_maxstrlenID) )
      call check ( nf90_def_dim (ncid, trim(NRECS_NAME),nrecs,nrecsID) )
      call check ( nf90_def_dim (ncid, trim(NVARS_NAME),nvars,nvarsID) )
      call check ( nf90_def_dim (ncid, NOBS_NAME, nobs, nobsID) )
      call check ( nf90_def_dim (ncid, NLEV_NAME, nlev, nlevID) )

! Create Variables
      call check( nf90_def_var(ncid, longitude_NAME, nf90_float, 
     & nlocsID, lon_varID) )
      call check( nf90_def_var(ncid, latitude_NAME, nf90_float, 
     & nlocsID, lat_varID) )
      call check( nf90_def_var(ncid, time_NAME, nf90_double, 
     & nlocsID, time_varID) )
      call check( nf90_def_var(ncid, obs_sstObsValue_NAME, nf90_float,
     & nlocsID, obsValue_varID) )
      call check( nf90_def_var(ncid, obs_sstObsError_NAME, nf90_float,
     & nlocsID, obsError_varID) )
      call check( nf90_def_var(ncid, ocean_UpLVL_Temp_NAME, 
     & nf90_float, (/nlevID, nlocsID/), oceanUpLvlTmp_varID) )

      call check ( nf90_put_att(ncid, obsValue_varID, "units", "C"))
      call check ( nf90_put_att(ncid, obsError_varID, "units", "C"))
      call check ( nf90_put_att(ncid, obsError_varID, "description",
     +             "Observation error standard deviation"))
      call check ( nf90_put_att(ncid, nf90_global, "date_time",
     +             runtime ))

! End Definition
      call check ( nf90_enddef(ncid) )

!      read(time_data,*) time_data_real
!       write(*,*) time_data
!       time_data = 24.1
! Put values
      call check ( nf90_put_var(ncid, lon_varID, lon_data))
      call check ( nf90_put_var(ncid, lat_varID, lat_data))
      call check ( nf90_put_var(ncid, time_varID, time_data))
      call check ( nf90_put_var(ncid, obsValue_varID, obsValue_data))
      call check ( nf90_put_var(ncid, obsError_varID, obsError_data))
      call check ( nf90_put_var(ncid, obsError_varID, obsError_data))
!      call check ( nf90_put_var(ncid, oceanUpLvlTmp_varID, 
!     +           oceanUpLvlTmp_data))

! Close NetCDF File
      call check ( nf90_close(ncid) )

      contains
      subroutine check (status)
      integer, intent (in) :: status

      if (status /= nf90_noerr) then
        print *, nf90_strerror(status)
        stop "Stopped"
      endif
      end subroutine check

      end subroutine IODA_sst_nc4
