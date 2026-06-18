subroutine read_saberT_files(cdtg_an,t_upd_cy)

!
!   This subroutine reads SABER T data from daily files.
!   The daily files are produced by KWH.
!   Each file contains all the T measurements for a single day.
!
! We reuse structures/arrays originally designed for MLS, 
! but don't let the names confuse you.  SABER data structure is
! essentially identical to MLS
!
!
      use mls_module
      implicit none
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
   integer     ::  istat,imin(1)      ! scratch 
   integer     ::  inc,nfiles,ifile   ! scratch 
   real        ::  sec0,dtime         ! scratch 
      
   real,    dimension(maxprof) :: sec
   integer, dimension(maxprof) :: jday
   real,    dimension(maxlev)  :: x        

   integer, parameter :: ngrid=23
   real,    dimension(ngrid) :: pgrid,bias


      data pgrid/  68.00,    57.94,    49.37,    42.07,    35.85, &
       30.5544,  26.0367,  22.1870,  18.9065,  16.1111,  13.7290,  11.6991,   9.9693,   8.4953, &
        7.2392,   6.1688,   5.2567,   4.4795,   3.8172,   3.2528,   2.7718,   2.3620,   2.0128 /

      data bias/  1.80,     1.80,     1.80,     1.80,     1.80, &
         1.80,    1.60,     1.20,     1.00,     1.00,     1.00,     1.00,     1.00,    0.90, &
         0.80,    0.40,     0.00,     0.00,     0.00,     0.00,     0.00,     0.00,    0.00 /

    call get_ev('SABER_LOC',path)

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
         
    print *,"Starting read_saberT_files"
    print *,"   Dtime= ",dtime
    print *,"   cdtg = ",cdtg_an
    print *,"   Path to SABER data: ",path
    print *,"   Seconds of day for cdtg_an = ",sec0
    print *,"   Looking for data withn +/- ",dtime," seconds"

    file = "saber_T_"//cdtg_an(1:8)//".dat"
    call slen(path,i)
    if(path(i:i).eq.'/') i=i-1
    file = path(1:i)//'/'//file
    print *,"   SABER file name: ",file

    if (inc .ne. 0) then
       a = cdtg_an
!      call dtgfxx(cdtg_an,a,inc*24)  
       call dtgmod(cdtg_an,inc*24,a,istat)  
       file2 = "saber_T_"//a(1:8)//".dat"
       file2 = path(1:i)//'/'//file2
       print *,"   2nd SABER file name: ",file2 
    endif 
     
    k = 1
    nmls=0
    do ifile=1,nfiles 
!
!     Read mls T file
!
        if (ifile .eq. 1) then
           open(lu,file=file,status='OLD',form='UNFORMATTED',access='SEQUENTIAL',iostat=istat)
        else
           open(lu,file=file2,status='OLD',form='UNFORMATTED',access='SEQUENTIAL',iostat=istat)
        endif
        if (istat .eq. 0) then 

          read(lu) n,np
          print *,"   # of SABER pressure levels = ",np
          print *,"   # of SABER profiles = ",n
          if (k+n .gt. maxprof) then
            print *,"read_saberT_files: ERROR: exceeded dimensions, too many profiles too read"
            stop
          endif
          if (np .gt. maxlev) then
            print *,"read_saberT_files: ERROR: exceeded dimensions, too many pressure levels to read"
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
         print *," Warning: Cannot open SABER file"
        endif

    enddo !ifile

    nmls = k-1  !total number of profiles read
    print *,"   Total number of SABER profiles read = ",nmls
    if (nmls .eq. 0) then
      print*,'Warning: no SABER profiles read, finished.'
      return
    endif

!
!   APPLY BIAS CORRECTION
!
    do j=1,nlev

      imin = minloc( abs(mls_p(j)-pgrid) )
      k    = imin(1)
      write (*,'(a,i4,3f12.5)')"saber bias correction: k,press,pgrid,bias= ",k,mls_p(j),pgrid(k),bias(k)
      mls_val(1:nmls,j) = mls_val(1:nmls,j) - bias(k)

    enddo
!
!     Assign dt value 
!
      do i=1,nmls
        mls_dt(i)  = sec(i)-sec0
      enddo
!
!     Cull profiles not in time window and with bad data flag (err<0)
!   
      k=1
      do i=1,nmls
        if (minval(mls_err(k,1:nlev)) .gt. 0) then
          if (abs(mls_dt(i)) .le. dtime) then 
            mls_dt(k)    = mls_dt(i)
            mls_lat(k)   = mls_lat(i)
            mls_lon(k)   = mls_lon(i)
            mls_val(k,:) = mls_val(i,:)
            mls_err(k,:) = mls_err(i,:)
            k=k+1
          endif
        endif
      enddo
      nmls = k-1  !New value for number of profiles 
      print *,"   Total number of good SABER profiles in time window = ",nmls

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
! Vertical smoothing for better representation on model grid
!
    do i=1,nmls
      !
      ! 3-level boxcar smoothing over profile
      !
      x(1:nlev) = mls_val(i,1:nlev)
      do k=2,nlev-1
        x(k)= (mls_val(i,k-1)+mls_val(i,k)+mls_val(i,k+1))/3.0
      enddo 
      x(nlev) = (mls_val(i,nlev-1)+mls_val(i,nlev))/2.0
      mls_val(i,1:nlev) = x(1:nlev)
      !
      ! Additional smoothing at higher altitudes 
      !
      do k=2,nlev-1
        if (mls_p(k) .lt. 1.0) then
          x(k)= (mls_val(i,k-1)+mls_val(i,k)+mls_val(i,k+1))/3.0
        endif
      enddo 
      mls_val(i,1:nlev) = x(1:nlev)
    enddo !i

!
!     Cut altitude range (pressure range) if needed 
!
   if (mls_p(1) .gt. mls_pmax .or. mls_p(nlev) .lt. saber_pmin) then
     i=2     
     do k=2,nlev
       if (mls_p(k) .ge. saber_pmin) i=k
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

return
end subroutine read_saberT_files
