subroutine read_mlsT_files(cdtg_an,t_upd_cy)

!
!
!   This subroutine reads mls T data from temporary files.
!   Each file contains all the T measurements for a single
!   day, reformated from the standard daily mls HDF files.  
!   These temporary files are used because I was too lazy to 
!   write a fortran HDF reader.
!
      use mls_module
!     use dtg_module

      implicit none

      interface
        subroutine get_ev(c_env, c_value)
          implicit none
          character(len=*), intent(in)  :: c_env
          character(len=*), intent(out) :: c_value
        end subroutine get_ev
      end interface

      interface
        subroutine slen (cstr,lenc)
          implicit none
          character(len=*),intent(in) :: cstr
          integer, intent(out) :: lenc
        end subroutine slen
      end interface

!===
!===     Arguments                               
!===
      character, intent(in) :: cdtg_an*(*) ! analysis date time group
      real,      intent(in) :: t_upd_cy    ! time window in hours of update cycle

!===
!===   Working variables for reading mls data       
!===
   character(len=120):: path                          ! path to mls data files
   character(len=80) :: file,file2                    ! mls file name             
   character(len=10) :: a                             ! scratch (dtg)         

   integer     ::  i,j,k,np,n,lu=66   ! scratch 
   integer     ::  istat              ! scratch 
   integer     ::  inc,nfiles,ifile   ! scratch 
   real        ::  sec0,dtime         ! scratch 
      
   real,    dimension(maxprof) :: sec
   integer, dimension(maxprof) :: jday
   real,    dimension(maxlev)  :: x        

   integer, parameter :: ngrid=35
   real,    dimension(ngrid) :: pgrid,bias
   integer, parameter :: ngrid4=40
   real,    dimension(ngrid4) :: pgrid4,bias4

   logical :: mlsver4=.false.
   integer :: imin(1)


    !
    ! MLS v2 Bias estimate by Steve Eckermann, based on a large camparison of MLS and 
    ! SABER-v2 temperature profiles
    !
    data pgrid/ &
      316.22775, 261.01572, 215.44347, 177.82794, 146.77992, 121.15276, 100.00000,  82.54042, &
       68.12920,  56.23413,  46.41589,  38.31187,  31.62278,  26.10157,  21.54435,  14.67799, &
       10.00000,   6.81292,   4.64159,   3.16228,   2.15443,   1.46780,   1.00000,   0.68129, & 
        0.46416,   0.31623,   0.21544,   0.14678,   0.10000,   0.04642,   0.02154,   0.01000, &
        0.00464,   0.00215,   0.00100/
    data bias/ &
          0.000,    0.000,    0.000,    0.000,    0.000,    0.000,    0.000,    0.000, &
          0.000,    0.000,    0.000,    0.000,    0.000,    0.000,    0.000,    0.000, &
          0.000,    0.000,    0.164,   -1.039,   -0.870,    0.619,    2.319,    1.788, &
         -2.536,   -4.475,   -2.997,    0.252,   -2.459,   -3.801,   -3.126,   -2.196, &
         -1.876,   -5.277,  -10.224 /

    !
    ! MLS v4 Bias estimate by Steve Eckermann, based on a large camparison of MLS and 
    ! SABER-v2 temperature profiles
    !
    data pgrid4/ &
      100.000000,  82.540421,  68.129204,  56.234131,  46.415890,  38.311867,  31.622776, &
       26.101572,  21.544348,  17.782795,  14.677993,  12.115276,  10.000000,   8.254042, &
        6.812921,   5.623413,   4.641589,   3.831187,   3.162278,   2.610157,   2.154435, &
        1.778279,   1.467799,   1.211528,   1.000000,   0.681292,   0.464159,   0.316228, &
        0.215443,   0.146780,   0.100000,   0.046416,   0.021544,   0.010000,   0.004642, &
        0.002154,   0.001000,   0.000464,   0.000215,   0.000100/ 
    data bias4/ &
          0.000,    0.000,    0.000,    0.000,    0.000,    0.000,    0.000, &
          0.000,    0.000,    0.000,    0.000,    0.000,    0.000,    0.000, &
          0.000,    0.000,   -0.035,   -0.490,   -0.932,   -0.790,   -0.330, &
          0.207,    1.079,    2.396,    2.365,    1.435,   -2.885,   -4.642, &
         -3.001,    0.350,   -2.446,   -4.049,   -2.537,   -1.593,   -1.560, &
         -6.225,  -10.467,   -8.748,   -4.388,   -4.959/ 



    call get_ev('MLS_LOC',path)
    call get_ev('MLS_VER',a)
    if (a(1:1) .eq. '4') mlsver4=.true.

!
!     Do we need to read 2 daily files or just one?
!
    inc = 0
    read(cdtg_an(9:10),*) sec0    
    sec0 = sec0*3600.                   !seconds of day           
    dtime = t_upd_cy*3600./2.0          !time window in seconds     

    if (sec0 .lt. dtime) then
      inc = -1                          !need previous day's file
    else if (sec0+dtime .gt. 86400.) then 
       inc = 1                          !need next day's file 
    endif

    if (inc .eq. 0) nfiles=1 
    if (inc .ne. 0) nfiles=2 
         
    print *,"Starting read_mlsT_files"
    print *,"   Dtime= ",dtime
    print *,"   cdtg = ",cdtg_an
    print *,"   Path to MLS data: ",path
    print *,"   Seconds of day for cdtg_an = ",sec0
    print *,"   Looking for data withn +/- ",dtime," seconds"

    if (mlsver4) then
      file = "mls_T_v4_"//cdtg_an(1:8)//".dat"
    else
      file = "mls_T_"//cdtg_an(1:8)//".dat"
    endif
    call slen(path,i)
    if(path(i:i).eq.'/') i=i-1
    file = path(1:i)//'/'//file
    print *,"   MLS file name: ",file

    if (inc .ne. 0) then
       a = cdtg_an
       call dtgmod(cdtg_an,inc*24,a,istat)  
       if (mlsver4) then
         file2 = "mls_T_v4_"//a(1:8)//".dat"
       else
         file2 = "mls_T_"//a(1:8)//".dat"
       endif
       file2 = path(1:i)//'/'//file2
       print *,"   2nd MLS file name: ",file2 
    endif 
     
    k = 1
    nmls=0
    do ifile=1,nfiles 
!
!     Read mls T file
!
        if (ifile .eq. 1) then
           open(lu,file=file,status='OLD',form='UNFORMATTED',access='SEQUENTIAL',iostat=istat)!,convert='BIG_ENDIAN')
        else
           open(lu,file=file2,status='OLD',form='UNFORMATTED',access='SEQUENTIAL',iostat=istat)!,convert='BIG_ENDIAN')
        endif
        if (istat .eq. 0) then 

          read(lu) n,np
          print *,"   # of MLS pressure levels = ",np
          print *,"   # of MLS profiles = ",n
          if (k+n .gt. maxprof) then
            print *,"read_mlsT_files: ERROR: exceeded dimensions, too many profiles too read"
            stop
          endif
          if (np .gt. maxlev) then
            print *,"read_mlsT_files: ERROR: exceeded dimensions, too many pressure levels to read"
            stop
          endif
          read(lu) (x(i),i=1,np)
          print *,"   read presure grid    - completed"
          read(lu) (sec(i)  ,i=k,k+n-1)
          print *,"   read seconds array   - completed"
          read(lu) (jday(i) ,i=k,k+n-1)
          print *,"   read jday array      - completed"
          read(lu) (mls_lat(i)  ,i=k,k+n-1)
          print *,"   read latitude array  - completed"
          read(lu) (mls_lon(i)  ,i=k,k+n-1)
          print *,"   read longitude array - completed"
          read(lu) ((mls_val(i,j)  ,i=k,k+n-1), j=1,np)
          print *,"   read T values        - completed"
          read(lu) ((mls_err(i,j)  ,i=k,k+n-1), j=1,np)
          print *,"   read error values    - completed"
          close(lu)

          !
          ! For previous day data, shift the seconds field relative to the
          ! analysis time of curent day.
          !
          if (ifile .ne. 1) then
             sec(k:k+n-1) = sec(k:k+n-1) + inc*86400
          endif

          k = k+n
          mls_p(1:np) = x(1:np)     
          nlev = np
       
        else
         print *," Warning: Cannot open MLS file"
        endif

    enddo !ifile

    nmls = k-1  !total number of profiles read
    print *,"   Total number of MLS profiles read = ",nmls
    if (nmls .eq. 0) then
      print*,'Warning: no MLS profiles read, finished.'
      return
    endif

!
!   APPLY MLS BIAS CORRECTION
!
    do j=1,nlev

      if (mlsver4) then

        imin = minloc( abs(mls_p(j)-pgrid4) )
        k    = imin(1)
        write (*,'(a,i4,3f12.5)')"mls bias correction: k,press,pgrid,bias= ",j,mls_p(j),pgrid4(k),bias4(k)
        mls_val(1:nmls,j) = mls_val(1:nmls,j) - bias4(k)
        
      else

        imin = minloc( abs(mls_p(j)-pgrid) )
        k    = imin(1)
        write (*,'(a,i4,3f12.5)')"mls bias correction: k,press,pgrid,bias= ",j,mls_p(j),pgrid(k),bias(k)
        mls_val(1:nmls,j) = mls_val(1:nmls,j) - bias(k)

      endif

    enddo
!
!     Assign dt value 
!
      do i=1,nmls
        mls_dt(i)  = sec(i)-sec0
      enddo
!
!     Cull profiles not in time window
!   
      k=1
      do i=1,nmls
        if (abs(mls_dt(i)) .le. dtime) then
          mls_dt(k)    = mls_dt(i)
          mls_lat(k)   = mls_lat(i)
          mls_lon(k)   = mls_lon(i)
          mls_val(k,:) = mls_val(i,:)
          mls_err(k,:) = mls_err(i,:)
          k=k+1
        endif
      enddo
      nmls = k-1  !New value for number of profiles 
      print *,"   Total number of MLS profiles in time window = ",nmls

!
!     Cut altitude range (pressure range) if needed 
!
   if (mls_p(1) .gt. mls_pmax .or. mls_p(nlev) .lt. mls_pmin) then
   print *,"    Reducing pressure range to pmin,pmax= ",mls_pmin,mls_pmax
     i=2     
     do k=2,nlev
       if (mls_p(k) .ge. mls_pmin) i=k
     enddo
     j=0     
     do k=nlev,1,-1
       if (mls_p(k) .le. mls_pmax) j=k-1
     enddo
     np=i-j
     do k=1,np
        mls_p(k) = mls_p(k+j)
        mls_val(1:nmls,k) = mls_val(1:nmls,k+j)
        mls_err(1:nmls,k) = mls_err(1:nmls,k+j)
     enddo
     nlev=np
   endif
!
!     Cull profiles with bad data flag (err<0)
!   
      k=1
      do i=1,nmls
        if (minval(mls_err(k,1:nlev)) .gt. 0) then
          mls_dt(k)    = mls_dt(i)
          mls_lat(k)   = mls_lat(i)
          mls_lon(k)   = mls_lon(i)
          mls_val(k,:) = mls_val(i,:)
          mls_err(k,:) = mls_err(i,:)
          k=k+1
        endif
      enddo
      nmls = k-1  !New value for number of profiles 
      print *,"   Total number of good MLS profiles = ",nmls

!
!     Adjust error values:
!        Error ~2K below 0.1 hPa, increases linearly (in log-pressure)
!        above 0.1 hPa to ~4K at .001 hPa.
!
    do i=1,nmls
      do j=1,nlev
         if (mls_err(i,j) .lt. 2) mls_err(i,j)=2.0  
         if (mls_p(j) .lt. 0.1) then
            mls_err(i,j)=mls_err(i,j)+3.0*(alog(.1)-alog(mls_p(j)))/4.60517
         endif
      enddo
    enddo

!
!     Add additional pressure levels between original levels
!
    if (maxlev .lt. nlev*2) then
      print *,"read_mlsT_files: ERROR: cannot added extra levels, dimension maxlev too small"
      stop
    endif
    x(:) = mls_p(:)
    do j=1,nlev-1
       mls_p(j*2-1) = x(j)
       mls_p(j*2)   =  exp((alog(x(j))+alog(x(j+1)))/2.0) !mid point pressure
    enddo
    mls_p(nlev*2-1) = x(nlev)
    do i=1,nmls
      x(:) = mls_val(i,:)
      do j=1,nlev-1
         mls_val(i,j*2-1) = x(j)
         mls_val(i,j*2)   = (x(j)+x(j+1))/2.0
      enddo 
      mls_val(i,nlev*2-1) = x(nlev)
      x(:) = mls_err(i,:)
      do j=1,nlev-1
         mls_err(i,j*2-1) = x(j)
         mls_err(i,j*2)   = (x(j)+x(j+1))/2.0
      enddo 
      mls_err(i,nlev*2-1) = x(nlev)
    enddo
    nlev = 2*nlev-1

return
end subroutine read_mlsT_files
!------------------------------------------------------------------------------!
