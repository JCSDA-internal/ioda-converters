Module ATMS_Spatial_Average_Mod
!
!
! abstract:  This routine reads BUFR format ATMS radiance 
!            (brightness temperature) files, spaatially 
!            averages the data using the AAPP averaging routines
!            and writes the data back into BUFR format
!
!
!
! Program history log:
!    2011-11-18   collard   - Original version
!    2017-07-13   yanqiu zhu - fix index bugs in subroutine ATMS_Spatial_Average
! 

  use kinds, only: r_kind,r_double,i_kind, i_llong

  implicit none     


! Declare module level parameters
  real(r_double), parameter    :: Missing_Value=1.e11_r_double

  private
  public :: ATMS_Spatial_Average

CONTAINS 

  SUBROUTINE ATMS_Spatial_Average(num_loc, nchanl, time, fov, channel, bt_inout, scanline, Error_Status)
    IMPLICIT NONE
    
    ! Declare passed variables
    integer(i_kind),          intent(in   ) :: num_loc, nchanl 
    integer(i_kind),          intent(in   ) :: fov(num_loc)
    integer(i_llong),         intent(in   ) :: time(num_loc)
    integer(i_kind),          intent(in   ) :: channel(nchanl*num_loc)
    integer(i_kind),          intent(inout) :: scanline(num_loc)
    real(r_kind),             intent(inout) :: bt_inout(nchanl*num_loc)
    integer(i_kind),          intent(inout) :: error_status 

    ! Declare local variables
    logical          :: do_interface_check, do_output_check
    integer(i_kind)  :: iobs, iloc, ichn
    integer(i_kind), dimension(nchanl, num_loc) :: channel_number
    real(r_kind),    dimension(nchanl, num_loc) :: bt_obs

    ! -------------------------------------------------------------------- 
    ! Declare local parameters
    integer(i_kind), parameter :: atms1c_h_wmosatid=224
    integer(i_kind), parameter :: lninfile=15
    integer(i_kind), parameter :: max_fov=96
    real(r_kind), parameter    :: scan_interval = 8.0_r_kind/3.0_r_kind
    ! Maximum number of channels 
    integer(i_kind), parameter :: maxchans = 22
    ! Minimum allowed BT as a function of channel number
    real(r_kind), parameter :: minbt(maxchans) = &
         (/ 120.0_r_kind, 120.0_r_kind, 190.0_r_kind, 190.0_r_kind, &
            200.0_r_kind, 200.0_r_kind, 200.0_r_kind, 190.0_r_kind, &
            190.0_r_kind, 180.0_r_kind, 180.0_r_kind, 180.0_r_kind, &
            190.0_r_kind, 200.0_r_kind, 200.0_r_kind, 120.0_r_kind, &
            120.0_r_kind, 120.0_r_kind, 150.0_r_kind, 170.0_r_kind, &
            180.0_r_kind, 190.0_r_kind /)
    ! Maximum allowed BT as a function of channel number
    real(r_kind), parameter :: maxbt(maxchans) = &
         (/ 320.0_r_kind, 320.0_r_kind, 300.0_r_kind, 300.0_r_kind, &
            300.0_r_kind, 270.0_r_kind, 250.0_r_kind, 240.0_r_kind, &
            240.0_r_kind, 250.0_r_kind, 250.0_r_kind, 270.0_r_kind, &
            280.0_r_kind, 290.0_r_kind, 300.0_r_kind, 320.0_r_kind, &
            320.0_r_kind, 300.0_r_kind, 300.0_r_kind, 300.0_r_kind, &
            300.0_r_kind, 300.0_r_kind /)

    ! Declare local variables
    character(30) :: Cline

    integer(i_kind) :: i, iscan, ifov, ichan, nchannels, wmosatid, version
    integer(i_kind) :: ios, max_scan, mintime
    integer(i_kind) :: nxaverage(nchanl), nyaverage(nchanl)
    integer(i_Kind) :: channelnumber(nchanl),qc_dist(nchanl)
    integer(i_kind), ALLOCATABLE ::  scanline_back(:,:)

    real(r_kind) :: sampling_dist, beamwidth(nchanl)
    real(r_kind) :: newwidth(nchanl), cutoff(nchanl),err(nchanl)
    real(r_kind), allocatable, target :: bt_image(:,:,:)

    ! ------------------------------------------------------------------------------

    do_interface_check = .true.
    do_output_check = .false.
    if (do_output_check) do_interface_check = .true.

    if (do_interface_check) then

       ! Check passed variables: dimension 
       write(6,*) 'ATMS_Spatial_Average: checking dimension ... '
       write(6,*) 'ATMS_Spatial_Average: num_loc = ', num_loc
       write(6,*) 'ATMS_Spatial_Average: nchanl  = ', nchanl

       ! Check passed variable: channel 
       write(6,*) 'ATMS_Spatial_Average: checking channel ... '
       write(6,*) 'ATMS_Spatial_Average: are we passing channel in here OK ? ... '
       write(6,*) 'minval/maxval channel = ', minval(channel), maxval(channel)

       ! Check passed variable: fov
       write(6,*) 'ATMS_Spatial_Average: checking fov ... '
       write(6,*) 'ATMS_Spatial_Average: are we passing fov in here OK ? ... '
       write(6,*) 'minval/maxval fov = ', minval(fov), maxval(fov)

       write(6,*) 'ATMS_Spatial_Average: chechking scanline ...'
       write(6,*) 'ATMS_Spatial_Average: are we passing and getting scanline in here OK ? ... '

       ! Check passed variable: scanline 
       ! Calculate scanline    
       do iloc = 1, num_loc
          iscan = int(iloc/96)
          scanline(iloc) = iscan + 1  
       enddo
       write(6,*) 'minval/maxval scanline = ', minval(scanline), maxval(scanline)

       ! Check passed variable: time
       write(6,*) 'ATMS_Spatial_Average: checking time ... '
       write(6,*) 'ATMS_Spatial_Average: are we passing time in here OK ? ... '
       write(6,*) 'minval/maxval time = ', minval(time), maxval(time)

       write(6,*) 'ATMS_Spatial_Average: chechking scanline ...'
       write(6,*) 'ATMS_Spatial_Average: are we passing and getting scanline in here OK ? ... '

       ! Check passed variable: bt_inout
       write(6,*) 'ATMS_Spatial_Average: chechking bt_inout ...'
       write(6,*) 'ATMS_Spatial_Average: are we passing and getting bt inout here OK ? ... '

       ! print out for checking and debugging
       bt_obs = reshape(bt_inout, (/nchanl, num_loc/))
       channel_number = reshape(channel, (/nchanl, num_loc/))
 
       if (do_output_check) then
          write(6,  *) 'emily check 1 ... '
          write(6,102) 'iobs', 'iloc', 'time', 'fov', 'ichn', 'channel', 'bt_obs'
          iobs = 1 
          do iloc = 1, num_loc
             do ichn = 1, nchanl 
                write(6,101) iobs, iloc, time(iloc), fov(iloc), ichn, channel_number(ichn,iloc), bt_obs(ichn,iloc)
                iobs = iobs+1
             enddo
          enddo
101       format(i12,2x,i12,2x,i20,2x,3(i12,2x),f15.8)
102       format(a12,2x,a12,2x,a20,2x,3(a12,2x),a15)
       endif
       write(6,*) 'minval/maxval bt_obs = ', minval(bt_obs), maxval(bt_obs)

       if (do_output_check) then

          write(6,  *) 'emily check 2 ... '
          write(6,202) 'iobs', 'iloc', 'time', 'fov', 'ichn', 'channel', 'bt_obs'
          do iobs = 0, num_loc*nchanl-1 
             iloc = int(iobs/nchanl)
             ichn = int(mod(iobs,nchanl)) 
             write(6,201) iobs+1, iloc+1, time(iloc+1), fov(iloc+1), ichn+1, channel(iobs+1), bt_inout(iobs+1)
          enddo 
201       format(i12,2x,i12,2x,i20,2x,3(i12,2x),f15.8)
202       format(a12,2x,a12,2x,a20,2x,3(a12,2x),a15)
       endif
       write(6,*) 'minval/maxval bt_inout = ', minval(bt_inout), maxval(bt_inout)
       error_status = 0
       write(6,*)'emily checking: error_status = ', error_status
    endif ! do_interface_check

    ! ------------------------------------------------------------------------------

    error_status=0

    write(6,*) 'emily checking nchanl = ', nchanl
    if (nchanl > maxchans) then 
       write(0,*) 'Unexpected number of ATMS channels: ',nchanl
       error_status = 1
       return 
    endif

    write(6,*) 'emily checking: begin reading the beamwidth requirement from file ...'
    ! Read the beamwidth requirements
    open(lninfile,file='atms_beamwidth.txt',form='formatted',status='old', &
         iostat=ios)
    write(6,*) 'emily checking: ios = ', ios
    if (ios /= 0) then 
       write(*,*) 'Unable to open atms_beamwidth.txt'
       error_status=1
       return 
    endif 
    wmosatid=999
    read(lninfile,'(a30)',iostat=ios) cline
    do while (wmosatid /= atms1c_h_wmosatid .AND. ios == 0)
       write(6,*) 'emily checking: in do while loop reading ...'
       do while (cline(1:1) == '#')
          read(lninfile,'(a30)') cline
       enddo 
       read(cline,*) wmosatid
       write(6,*) 'emily checking: wmosatid = ', wmosatid

       read(lninfile,'(a30)') cline
       do while (cline(1:1) == '#')
          read(lninfile,'(a30)') cline
       enddo 
       read(cline,*) version
       write(6,*) 'emily checking: version = ', version 

       read(lninfile,'(a30)') cline
       do while (cline(1:1) == '#')
          read(lninfile,'(a30)') cline
       enddo 
       read(cline,*) sampling_dist
       write(6,*) 'emily checking: sampleing_dist = ', sampling_dist 

       read(lninfile,'(a30)') cline
       do while (cline(1:1) == '#')
          read(lninfile,'(a30)') cline
       enddo 
       read(cline,*) nchannels
       write(6,*) 'emily checking: nchannels = ', nchannels 

       read(lninfile,'(a30)') cline
       if (nchannels > 0) then
          do ichan=1,nchannels
             read(lninfile,'(a30)') cline
             do while (cline(1:1) == '#')
                read(lninfile,'(a30)') cline
             enddo 
             read(cline,*) channelnumber(ichan),beamwidth(ichan), &
                  newwidth(ichan),cutoff(ichan),nxaverage(ichan), &
                  nyaverage(ichan), qc_dist(ichan)
             write(6,*) channelnumber(ichan),beamwidth(ichan), &
                  newwidth(ichan),cutoff(ichan),nxaverage(ichan), &
                  nyaverage(ichan), qc_dist(ichan)
          enddo 
       end if
       read(lninfile,'(a30)',iostat=ios) cline
    enddo 
    write(6,*) 'emily checking: done reading the beamwidth requirement from file ...'
    if (wmosatid /= atms1c_h_wmosatid) then 
       write(*,*) 'ATMS_Spatial_Averaging: sat id not matched in atms_beamwidth.dat'
       error_status=1
       return 
    endif 
    close(lninfile)

    write(6,*) 'emily checking: determine scanline from time ...'
    ! Determine scanline from time
    mintime = minval(time)
    scanline(:)   = nint((time(1:num_loc)-mintime)/scan_interval)+1
    Max_Scan=maxval(scanline)
    write(6,*) 'minval/maxval scanline = ', minval(scanline), maxval(scanline)

    allocate(bt_image(max_fov, max_scan, nchanl))
    allocate(scanline_back(max_fov, max_scan))
    bT_image(:,:,:) = 1000.0_r_kind

    write(6,*) 'emily checking: loading bt image array for FFT transform ...'
    scanLine_back(:,:) = -1
    do i = 1, num_loc
       bt_image(fov(i),scanline(i),:) = bt_obs(:,i)
       scanline_back(fov(i),scanline(i)) = i
       write(6, 301) fov(i), scanline(i), bt_image(fov(i), scanline(i), 1:nchanl) 
    end do 
301 format(i6,2x,i6,2x,22(f8.3))

    ! Do FFT transform
    write(6,*) 'emily checking: begin doing FFT transform ...'
    DO ichan = 1, nchanl

       err(ichan) = 0

       ! Set all scan positions to missing in a scanline if one is missing
       do iscan = 1,max_scan
          if (any(bt_image(:, iscan, ichan) > 500.0_r_kind)) &
                  bt_image(:, iscan, ichan)=1000.0_r_kind
       enddo

       ! If the channel number is present in the channelnumber array we should
       ! process it 
       ! (otherwise bt_inout just keeps the same value):
       do i = 1, nchannels

          if (channelnumber(i) == ichan) then

             call MODIFY_BEAMWIDTH ( max_fov, max_scan, bt_image(:,:,ichan),   &
                                     sampling_dist, beamwidth(i), newwidth(i), &
                                     cutoff(i), nxaverage(i), nyaverage(i),    &
                                     qc_dist(i), minbt(Ichan), maxbt(ichan), ios)
          
             ! Load the transform image array back
             if (ios == 0) THEN
                do iscan = 1, max_scan
                   do ifov = 1, max_fov
                      if (scanline_back(ifov, iscan) > 0) &
                        bt_obs(ichan, scanline_back(ifov, iscan)) = &
                        bt_image(ifov, iscan, ichan)
                   enddo
                enddo
             else 
                err(ichan) = 1
             end if 
          end if
       enddo
    enddo 
    write(6,*) 'emily checking: end doing FFT transform ...'

    write(6,*) 'emily checking: checking error status for each channel ...'
    do ichan = 1,nchanl
      if(err(ichan) >= 1)then
         error_status = 1
         return
      end if
    end do

    write(6,*) 'emily checking: reshape bt_obs to bt_inout for output ...'
    ! Reshape for output
    bt_inout = reshape(bt_obs, (/nchanl*num_loc/))

    write(6,*) 'emily checking: deallocating ...'
    deallocate(bt_image, scanline_back)

    write(6,*) 'emily checking: DONE!!'

END Subroutine ATMS_Spatial_Average

SUBROUTINE MODIFY_BEAMWIDTH ( nx, ny, image, sampling_dist,& 
     beamwidth, newwidth, mtfcutoff, nxaverage, nyaverage, qc_dist, &
     Minval, MaxVal, Error)
     
!-----------------------------------------
! Name: $Id$
!
! Purpose:
!   Manipulate the effective beam width of an image. For example, convert ATMS
!   to AMSU-A-like resolution while reducing the noise.
!
! Method:
!   1) Pad the image to a power of 2 in each dimension.
! If FFT technique is to be used then: 
!   2) Assuming Gaussian beam shapes, calcluate the input and output Modulation
!      Transfer Functions (MTF).
!   3) FFT image to frequency domain (2-D).
!   4) Multiply by output MTF divided by input MTF. If a cut-off is specified
!      (when attempting to make the beam width narrower), attenuate further
!      by an exponential function - factor of 2 at the cutoff. 
!   5) FFT back to image domain 
! Finally,
!   6) Over-write the input image, with averaging if requested.
!
! COPYRIGHT
!    This software was developed within the context of the EUMETSAT Satellite
!    Application Facility on Numerical Weather Prediction (NWP SAF), under the
!    Cooperation Agreement dated 1 December 2006, between EUMETSAT and the
!    Met Office, UK, by one or more partners within the NWP SAF. The partners
!    in the NWP SAF are the Met Office, ECMWF, KNMI and MeteoFrance.
!
!    Copyright 2010, EUMETSAT, All Rights Reserved.
!
! History:
! Version    Date     Comment
!
!  1.0   22/07/2010   N.C.Atkinson
!  1.1   21/11/2011   Convert to f90. A. Collard
!
! Code Description:
!   FORTRAN 77, following AAPP standards
!
! Declarations:


      IMPLICIT NONE
! Parameters
      INTEGER(I_KIND), PARAMETER :: nxmax=128  !Max number of spots per scan line
      INTEGER(I_KIND), PARAMETER :: nymax=8192 !Max number of lines. Allows 6hrs of ATMS.

! Arguments
      INTEGER(I_KIND), INTENT(IN)  :: nx, ny         !Size of image
      REAL(R_KIND), INTENT(INOUT)  :: image(nx,ny)   !BT or radiance image
      REAL(R_KIND), INTENT(IN)     :: sampling_dist  !typically degrees
      REAL(R_KIND), INTENT(IN)     :: beamwidth      !ditto
      REAL(R_KIND), INTENT(IN)     :: newwidth       !ditto
      REAL(R_KIND), INTENT(IN)     :: mtfcutoff      !0.0 to 1.0
      INTEGER(I_KIND), INTENT(IN)  :: nxaverage      !Number of samples to average (or zero)
      INTEGER(I_KIND), INTENT(IN)  :: nyaverage      !Number of samples to average (or zero)
      INTEGER(I_KIND), INTENT(IN)  :: qc_dist        !Number of samples around missing data to set to 
      REAL(R_KIND), INTENT(IN)     :: maxval         !BTs above this are considered missing
      REAL(R_KIND), INTENT(IN)     :: minval         !BTs below this are considered missing
      INTEGER(I_KIND), INTENT(OUT) :: Error          !Error Status
       
! Local variables
      INTEGER(I_KIND) :: nxpad, nypad, dx, dy
      INTEGER(I_KIND) :: i,j,k,ix,iy, ii, jj
      INTEGER(I_KIND) :: ifirst
      INTEGER(I_KIND) :: xpow2, ypow2
      INTEGER(I_KIND) :: nxav2, nyav2, naverage
      INTEGER(I_KIND) :: deltax, minii, maxii, minjj, maxjj
      REAL(R_KIND), ALLOCATABLE :: mtfxin(:),mtfxout(:)
      REAL(R_KIND), ALLOCATABLE :: mtfyin(:),mtfyout(:)
      REAL(R_KIND) :: mtfin,mtfout,mtf_constant
      REAL(R_KIND), ALLOCATABLE :: mtfpad(:,:)
      REAL(R_KIND), ALLOCATABLE :: imagepad(:,:)
      REAL(R_KIND) :: f,df,factor
      REAL(R_KIND) :: PI, LN2, LNcsquared
      LOGICAL :: missing
      LOGICAL, ALLOCATABLE :: gooddata_map(:,:)


! End of declarations
!-----------------------------------------
      
      PI = 4.0_r_kind*atan(1.0)
      LN2 = LOG(2.0_r_kind)
      MTF_Constant=-(PI/(2*sampling_dist))**2/LN2
      IF (mtfcutoff > 0.0_r_kind) LNcsquared = LOG(mtfcutoff)**2
      nxav2 = nxaverage/2
      nyav2 = nyaverage/2
      naverage = nxaverage*nyaverage
      Error = 0

!1) Pad the image up to the nearest power of 2 in each dimension, by reversing
!the points near the edge.

      xpow2 = INT(LOG(nx*1.0_r_kind)/LN2 + 1.0_r_kind)
      ypow2 = INT(LOG(ny*1.0_r_kind)/LN2 + 1.0_r_kind)
      nxpad = 2**xpow2
      nypad = 2**ypow2
      dx = (nxpad - nx)/2
      dy = (nypad - ny)/2

      IF (nxpad > nxmax) THEN
         write(*,*) 'ATMS_Spatial_Average: nx too large, maximum allowed value is ',nxmax-1
         Error = 1
         RETURN
      END IF
      
      IF (nypad > nymax) THEN
         write(*,*) 'ATMS_Spatial_Average: ny too large, maximum allowed value is ',nymax-1
         Error = 1
         RETURN
      END IF

      ALLOCATE(mtfxin(nxpad),mtfxout(nxpad))
      ALLOCATE(mtfyin(nypad),mtfyout(nypad))
      ALLOCATE(mtfpad(nxpad,nypad))
      ALLOCATE(imagepad(nxpad,nypad))
      ALLOCATE(gooddata_map(nxmax,nymax))

!Loop over scan positions
      DO j=dy+1,dy+ny
        DO i=dx+1,dx+nx
          if (image(i-dx,j-dy) < minval) &
               image(i-dx,j-dy) = minval - 1.0_r_kind
          if (image(i-dx,j-dy) > maxval ) &
               image(i-dx,j-dy) = maxval + 1.0_r_kind
          imagepad(i,j) = image(i-dx,j-dy)   !Take a copy of the input data
          gooddata_map(i,j) = .TRUE.   ! Initialised for step 6)
        ENDDO

!Interpolate missing points in the along-track direction

        ifirst = -1
        missing = .false.
        
        DO i=dx+1,dx+nx
          IF (.not.missing) THEN
            IF (imagepad(i,j) >= minval .AND. imagepad(i,j) <= maxval) THEN
              ifirst = i
            ELSE
              missing = .true.
            ENDIF
          ELSE
            IF (imagepad(i,j) >= minval .AND. imagepad(i,j) <= maxval) THEN  !First good point 
                                                                             ! after missing
               missing = .false.
               IF (ifirst == -1) THEN
                  DO k=dx+1,i-1
                     imagepad(k,j) = imagepad(i,j)      !Constant
                  ENDDO
               ELSE
                  DO k=ifirst+1,i-1
                     factor = (i-k)*1.0_r_kind/(i-ifirst)      !Interpolate
                     imagepad(k,j) = imagepad(ifirst,j)*factor + &
                          imagepad(i,j)*(1.0_r_kind-factor)
                  ENDDO
               ENDIF
            ENDIF
          ENDIF
        ENDDO
        IF (missing) THEN         !Last scan is missing
          IF (ifirst >= 1) then
            DO k=ifirst+1,dx+nx
              imagepad(k,j) = imagepad(ifirst,j)     !Constant
            ENDDO
          ENDIF
        ENDIF          

!Continue padding the edges

        DO i=1,dx
          imagepad(i,j) = imagepad(dx+dx+2-i,j)
        ENDDO
        DO i=nx+dx+1,nxpad
          imagepad(i,j) = imagepad(nx+dx+nx+dx-i,j)
        ENDDO
     ENDDO
     DO j=1,dy
        DO i=1,nxpad
           imagepad(i,j) = imagepad(i,dy+dy+2-j)
        ENDDO
     ENDDO
     DO j=ny+dy+1,nypad
        DO i=1,nxpad
           imagepad(i,j) = imagepad(i,ny+dy+ny+dy-j)
        ENDDO
     ENDDO

!2) Compute the MTF modifications. Assume beams are Gaussian.

      IF (newwidth > 0) THEN
        df = 1.0_r_kind/nxpad
        DO i=1,nxpad/2+1
          f = df*(i-1)      !DC to Nyquist
          mtfxin(i) = exp(MTF_Constant*(f*beamwidth)**2)
          mtfxout(i) = exp(MTF_Constant*(f*newwidth)**2)
          IF (i > 1 .AND. i < nxpad/2+1) THEN
            mtfxin(nxpad-i+2) = mtfxin(i)
            mtfxout(nxpad-i+2) = mtfxout(i)
          ENDIF
        ENDDO
        df = 1.0_r_kind/nypad
        DO i=1,nypad/2+1
          f = df*(i-1)      !DC to Nyquist
          mtfyin(i) = exp(MTF_Constant*(f*beamwidth)**2)
          mtfyout(i) = exp(MTF_Constant*(f*newwidth)**2)
          IF (i > 1 .AND. i < nypad/2+1) THEN
            mtfyin(nypad-i+2) = mtfyin(i)
            mtfyout(nypad-i+2) = mtfyout(i)
          ENDIF
        ENDDO
        DO i=1,nxpad
          DO j=1,nypad
            mtfin = mtfxin(i)*mtfyin(j)
            mtfout = mtfxout(i)*mtfyout(j)
            if (mtfcutoff > 0.0_r_kind) THEN
              mtfpad(i,j) = (mtfout * &
                exp(-LN2/LNcsquared*(LOG(mtfout))**2))/mtfin
            else
              mtfpad(i,j) = mtfout/mtfin
            endif
          ENDDO
        ENDDO

!3) Fourier transform, line by line then column by column.
!After each FFT, points 1 to nxpad/2+1 contain the real part of the spectrum,
!the rest contain the imaginary part in reverse order.

        DO j=1,nypad
           CALL SFFTCF(imagepad(:,j),nxpad,xpow2)
        ENDDO

        DO i=1,nxpad
           CALL SFFTCF(imagepad(i,:),nypad,ypow2)

!4) Multiply the spectrum by the MTF factor
           DO j=1,nypad
              imagepad(i,j) = imagepad(i,j)*mtfpad(i,j)
           ENDDO
        ENDDO

!5) Inverse Fourier transform, column by column then line by line 

        DO i=1,nxpad
          CALL SFFTCB(imagepad(i,:),nypad,ypow2)
        ENDDO

        DO j=1,nypad
          CALL SFFTCB(imagepad(:,j),nxpad,xpow2)
        ENDDO
     ENDIF   !New width is specified

!6) Reset missing values in gooddata_map, based on qc_dist and the values 
!   in the input image array

     ! Set the ends of the image to missing in the along track direction
     ! (doing the same across track will remove too much data)
     gooddata_map(:,1:MIN(qc_dist,ny))=.FALSE.
     gooddata_map(:,MAX(ny-qc_dist+1,1):ny)=.FALSE.
     
     DO j=1,ny
        DO i=1,nx
           IF (image(i,j) <= minval .OR. image(i,j) >= maxval ) THEN
              minjj=max(j+dy-qc_dist,1)
              maxjj=min(j+dy+qc_dist,nymax)
              DO jj=minjj,maxjj
                 deltax=INT(SQRT(REAL(qc_dist**2 - (jj-j-dy)**2 )))
                 minii=max(i+dx-deltax,1)
                 maxii=min(i+dx+deltax,nxmax)
                 DO ii=minii,maxii
                    gooddata_map(ii,jj)=.FALSE.
                 END DO
              END DO
           END IF
        END DO
     END DO

!7) Over-write the input image (points that are not missing)

     DO j=1,ny
        DO i=1,nx
           IF (gooddata_map(i+dx,j+dy)) THEN
              IF (nxav2 == 0.0_r_kind .AND. nyav2 == 0) THEN
                 image(i,j) = imagepad(i+dx,j+dy)
              ELSE
                 image(i,j) = 0.0_r_kind             !Do averaging
                 DO ix = -nxav2,nxav2
                    DO iy = -nyav2,nyav2
                       image(i,j) = image(i,j) + imagepad(i+dx+ix,j+dy+iy)
                    ENDDO
                 ENDDO
                 image(i,j) = image(i,j)/naverage
              ENDIF
           ELSE
              image(i,j) = missing_value
           END IF
        ENDDO
     ENDDO

!8) Deallocate arrays

     DEALLOCATE(mtfxin,mtfxout)
     DEALLOCATE(mtfyin,mtfyout)
     DEALLOCATE(mtfpad)
     DEALLOCATE(imagepad)
     DEALLOCATE(gooddata_map)

     RETURN
   END SUBROUTINE MODIFY_BEAMWIDTH

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix FFT program
!  Real input and output in data array X
!  Length is N = 2 ** M
!  Decimation-in-time, cos/sin in second loop
!  Output in order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     X(k) = sum_{j=0}^{N-1} x(j)*exp(-2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Oct. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named RVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE SFFTCF( X, N, M )

      IMPLICIT NONE

! ... Parameters ...
      REAL(R_KIND), PARAMETER :: SQRT2 = 1.4142135623730950488
      REAL(R_KIND), PARAMETER :: TWOPI = 6.2831853071795864769 

! ... Scalar arguments ...
      INTEGER(I_KIND), INTENT(IN) :: N, M
! ... Array arguments ...
      REAL(R_KIND), INTENT(INOUT) ::  X(N)
! ... Local scalars ...
      INTEGER(I_KIND)  J, I, K, IS, ID, I0, I1, I2, I3, I4, I5, I6, I7, I8
      INTEGER(I_KIND)  N1, N2, N4, N8
      REAL(R_KIND)  XT, R1, T1, T2, T3, T4, T5, T6
      REAL(R_KIND)  A, A3, E, CC1, SS1, CC3, SS3
!
! ... Exe. statements ...
!
      IF ( N == 1 ) RETURN
!
      J = 1
      N1 = N - 1
      DO 104, I = 1, N1
         IF ( I < J ) THEN
            XT = X(J)
            X(J) = X(I)
            X(I) = XT
         END IF
         K = N / 2
 102     DO WHILE (K < J) 
            J = J - K
            K = K / 2
         END DO
         J = J + K
 104  CONTINUE
! 
      IS = 1
      ID = 4
      LOOP1:DO
         DO 60, I0 = IS, N, ID
            I1 = I0 + 1
            R1 = X(I0)
            X(I0) = R1 + X(I1)
            X(I1) = R1 - X(I1)
 60      CONTINUE
         IS = 2 * ID - 1
         ID = 4 * ID
         IF ( IS >= N ) EXIT LOOP1
      END DO LOOP1
!
      N2 = 2
      DO 10, K = 2, M
         N2 = N2 * 2
         N4 = N2 / 4
         N8 = N2 / 8
         E = TWOPI / N2
         IS = 0
         ID = N2 * 2
         LOOP2: DO
            DO 38, I = IS, N-1, ID
               I1 = I + 1
               I2 = I1 + N4
               I3 = I2 + N4
               I4 = I3 + N4
               T1 = X(I4) + X(I3)
               X(I4) = X(I4) - X(I3)
               X(I3) = X(I1) - T1
               X(I1) = X(I1) + T1
               IF ( N4 == 1 ) CYCLE
               I1 = I1 + N8
               I2 = I2 + N8
               I3 = I3 + N8
               I4 = I4 + N8
               T1 = ( X(I3) + X(I4) ) / SQRT2
               T2 = ( X(I3) - X(I4) ) / SQRT2
               X(I4) = X(I2) - T1
               X(I3) = - X(I2) - T1
               X(I2) = X(I1) - T2
               X(I1) = X(I1) + T2
 38         CONTINUE
            IS = 2 * ID - N2
            ID = 4 * ID
            IF ( IS >=  N ) EXIT LOOP2
         END DO LOOP2
         A = E
         DO 32, J = 2, N8
            A3 = 3 * A
            CC1 = COS(A)
            SS1 = SIN(A)
            CC3 = COS(A3)
            SS3 = SIN(A3)
            A = J * E
            IS = 0
            ID = 2 * N2
            LOOP3: DO
               DO 30, I = IS, N-1, ID
                  I1 = I + J
                  I2 = I1 + N4
                  I3 = I2 + N4
                  I4 = I3 + N4
                  I5 = I + N4 - J + 2
                  I6 = I5 + N4
                  I7 = I6 + N4
                  I8 = I7 + N4
                  T1 = X(I3) * CC1 + X(I7) * SS1
                  T2 = X(I7) * CC1 - X(I3) * SS1
                  T3 = X(I4) * CC3 + X(I8) * SS3
                  T4 = X(I8) * CC3 - X(I4) * SS3
                  T5 = T1 + T3
                  T6 = T2 + T4
                  T3 = T1 - T3
                  T4 = T2 - T4
                  T2 = X(I6) + T6
                  X(I3) = T6 - X(I6)
                  X(I8) = T2
                  T2 = X(I2) - T3
                  X(I7) = - X(I2) - T3
                  X(I4) = T2
                  T1 = X(I1) + T5
                  X(I6) = X(I1) - T5
                  X(I1) = T1
                  T1 = X(I5) + T4
                  X(I5) = X(I5) - T4
                  X(I2) = T1
 30            CONTINUE
               IS = 2 * ID - N2
               ID = 4 * ID
               IF ( IS >= N ) EXIT LOOP3
            END DO LOOP3
 32      CONTINUE
 10   CONTINUE
      RETURN
!
! ... End of subroutine SFFTCF ...
!
   END SUBROUTINE SFFTCF

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  A real-valued, in place, split-radix IFFT program
!  Hermitian symmetric input and real output in array X
!  Length is N = 2 ** M
!  Decimation-in-frequency, cos/sin in second loop
!  Input order:
!         [ Re(0), Re(1), ..., Re(N/2), Im(N/2-1), ..., Im(1) ]
!
!  This FFT computes
!     x(j) = (1/N) * sum_{k=0}^{N-1} X(k)*exp(2ijk*pi/N)
!
!
!  H.V. Sorensen, Rice University, Nov. 1985
!
!  Reference:  H.V. Sorensen, D.L. Jones, M.T. Heideman, & C.S. Burrus;
!              Real-Valued Fast Fourier Transform Algorithms; IEEE
!              Trans. Acoust., Speech, Signal Process.; Vol ASSP-35,
!              June 1987, pp. 849-863.
!
!  This code was originally named IRVFFT.
!
!  History:
!   21/11/2011 Converted to something resembling f90.   A.Collard
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
      SUBROUTINE SFFTCB( X, N, M )

      use kinds, only: r_kind,r_double,i_kind

      IMPLICIT NONE

! ... Parameters ...
      REAL(R_KIND), PARAMETER :: SQRT2 = 1.4142135623730950488
      REAL(R_KIND), PARAMETER :: TWOPI = 6.2831853071795864769 

! ... Scalar arguments ...
      INTEGER(I_KIND), INTENT(IN) :: N, M
! ... Array arguments ...
      REAL(R_KIND), INTENT(INOUT) ::  X(N)
! ... Local scalars ...
      INTEGER(I_KIND)  J, I, K, IS, ID, I0, I1, I2, I3, I4, I5, I6, I7, I8
      INTEGER(I_KIND)  N1, N2, N4, N8
      REAL(R_KIND)  XT, R1, T1, T2, T3, T4, T5
      REAL(R_KIND)  A, A3, E, CC1, SS1, CC3, SS3
!
! ... Exe. statements ...
!
      IF ( N == 1 ) RETURN
!
      N2 = 2 * N
      DO 10, K = 1, M-1
         IS = 0
         ID = N2
         N2 = N2 / 2
         N4 = N2 / 4
         N8 = N4 / 2
         E = TWOPI / N2
         LOOP1: DO
            DO 15, I = IS, N-1, ID
               I1 = I + 1
               I2 = I1 + N4
               I3 = I2 + N4
               I4 = I3 + N4
               T1 = X(I1) - X(I3)
               X(I1) = X(I1) + X(I3)
               X(I2) = 2 * X(I2)
               X(I3) = T1 - 2 * X(I4)
               X(I4) = T1 + 2 * X(I4)
               IF ( N4 == 1 ) CYCLE
               I1 = I1 + N8
               I2 = I2 + N8
               I3 = I3 + N8
               I4 = I4 + N8
               T1 = ( X(I2) - X(I1) ) / SQRT2
               T2 = ( X(I4) + X(I3) ) / SQRT2
               X(I1) = X(I1) + X(I2)
               X(I2) = X(I4) - X(I3)
               X(I3) = 2 * ( - T2 - T1 )
               X(I4) = 2 * ( -T2 + T1 )
 15         CONTINUE
            IS = 2 * ID - N2
            ID = 4 * ID
            IF ( IS >= N-1 ) EXIT LOOP1
         END DO LOOP1
         A = E
         DO 20, J = 2, N8
            A3 = 3 * A
            CC1 = COS(A)
            SS1 = SIN(A)
            CC3 = COS(A3)
            SS3 = SIN(A3)
            A = J * E
            IS = 0
            ID = 2 * N2
            LOOP2: DO
 40            DO 30, I = IS, N-1, ID
                  I1 = I + J
                  I2 = I1 + N4
                  I3 = I2 + N4
                  I4 = I3 + N4
                  I5 = I + N4 - J + 2
                  I6 = I5 + N4
                  I7 = I6 + N4
                  I8 = I7 + N4
                  T1 = X(I1) - X(I6)
                  X(I1) = X(I1) + X(I6)
                  T2 = X(I5) - X(I2)
                  X(I5) = X(I2) + X(I5)
                  T3 = X(I8) + X(I3)
                  X(I6) = X(I8) - X(I3)
                  T4 = X(I4) + X(I7)
                  X(I2) = X(I4) - X(I7)
                  T5 = T1 - T4
                  T1 = T1 + T4
                  T4 = T2 - T3
                  T2 = T2 + T3
                  X(I3) = T5 * CC1 + T4 * SS1
                  X(I7) = - T4 * CC1 + T5 * SS1
                  X(I4) = T1 * CC3 - T2 * SS3
                  X(I8) = T2 * CC3 + T1 * SS3
 30            CONTINUE
               IS = 2 * ID - N2
               ID = 4 * ID
               IF ( IS >= N-1 ) EXIT LOOP2
             END DO LOOP2
 20      CONTINUE
 10   CONTINUE
!
      IS = 1
      ID = 4
      LOOP3: DO
         DO 60, I0 = IS, N, ID
            I1 = I0 + 1
            R1 = X(I0)
            X(I0) = R1 + X(I1)
            X(I1) = R1 - X(I1)
 60      CONTINUE
         IS = 2 * ID - 1
         ID = 4 * ID
         IF ( IS >= N ) EXIT LOOP3
      END DO LOOP3
!
      J = 1
      N1 = N - 1
      DO 104, I = 1, N1
         IF ( I < J ) THEN
            XT = X(J)
            X(J) = X(I)
            X(I) = XT
         END IF
         K = N / 2
         DO WHILE (K < J )         
            J = J - K
            K = K / 2
          END DO
          J = J + K
 104  CONTINUE
      XT = 1.0_r_kind / FLOAT( N )
      DO 99, I = 1, N
         X(I) = XT * X(I)
 99   CONTINUE
      RETURN
!
! ... End of subroutine SFFTCB ...
! 
      END SUBROUTINE SFFTCB

END MODULE ATMS_Spatial_Average_Mod
