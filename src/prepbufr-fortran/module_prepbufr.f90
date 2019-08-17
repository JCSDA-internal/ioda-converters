module module_prepbufr

   use kinds, only: r_kind,r_single,len_sta_name,rmissing,imissing
   use module_obs_conv_pt, only :  obs_conv_pt
   use module_obs_base, only :  obsbase
   use module_time, only : mtime

   implicit none

   public :: read_prepbufr

   private
   type, extends(obs_conv_pt) :: read_prepbufr
      integer :: datatype_match   ! hold wind data type 
      contains
         procedure :: initial_prepbufr
         procedure :: destroy_prepbufr
         procedure :: decodeprepbufr_all
   end type
   contains
!===================
      subroutine initial_prepbufr(this,datatype,yyyymmddhh,mm,time_window)
! initial prepbufr read
         implicit none
         class(read_prepbufr) :: this
         integer,intent(in) :: datatype,yyyymmddhh,mm
         real(r_single), intent(in) :: time_window

         call this%initial(datatype,yyyymmddhh,mm,time_window)
         
         this%datatype_match=1000
         if( datatype==120 .or.  datatype==122  .or. &
            (datatype>=130 .and. datatype<=135) .or. &
            (datatype>=180 .and. datatype<=188)) then
            this%datatype_match=datatype+100
         endif
      end subroutine initial_prepbufr

      subroutine destroy_prepbufr(this)
! releae memory
         implicit none
         class(read_prepbufr) :: this
         
         call this%destroy

      end subroutine destroy_prepbufr

      subroutine decodeprepbufr_all(this,filename)
! read in station info
         implicit none
         class(read_prepbufr) :: this

         type(obsbase),pointer :: thisobs => NULL()
         type(obsbase),pointer :: foundobs => NULL()

         character(len=*),intent(in)  :: filename
!
         integer, parameter :: mxmn=35, mxlv=250
         character(80):: hdstr='SID XOB YOB DHR TYP ELV SAID T29'
         character(80):: obstr='POB QOB TOB ZOB UOB VOB PWO CAT PRSS'
         character(80):: qcstr='PQM QQM TQM ZQM WQM NUL PWQ     '
         character(80):: oestr='POE QOE TOE NUL WOE NUL PWE     '
         real(8) :: hdr(mxmn),obs(mxmn,mxlv),qcf(mxmn,mxlv),oer(mxmn,mxlv)
         real(8) :: hdr2(mxmn),obs2(mxmn,mxlv),qcf2(mxmn,mxlv),oer2(mxmn,mxlv)
         real(8) :: hdr3(mxmn),obs3(mxmn,mxlv),qcf3(mxmn,mxlv),oer3(mxmn,mxlv)

         INTEGER        :: ireadmg,ireadsb

         character(8)   :: subset
         integer        :: unit_in=10,idate,nmsg,ntb

         character(8)   :: c_sid
         real(8)        :: rstation_id
         equivalence(rstation_id,c_sid)
 
         integer        :: i,k,kk,kk2,kk3,iret,iobs,ivar,iret2,iret3
         logical        :: if_duplicate,ifnotused
         integer        :: index_dup,index_sameobs

         type(mtime)    :: mt
         integer        :: yyyy,mm,dd,hh
         integer        :: refepochmins,cyclepochmins
         real(r_single) :: deltahh
!
!
! get the verification target time in epoch in minutes
!
         yyyy=this%idate/1000000
         hh=this%idate-yyyy*1000000
         mm=hh/10000
         hh=hh-mm*10000
         dd=hh/100
         hh=hh-dd*100
         refepochmins=mt%date2mins(yyyy,mm,dd,hh,this%mm)
!
!
!
         iobs=0
         index_sameobs=0   ! 0 no obs in obs2
                           ! 1 fresh obs in obs2

!         open(24,file='prepbufr.table')
         open(unit_in,file=trim(filename),form='unformatted',status='old')
         call openbf(unit_in,'IN',unit_in)
!         call dxdump(unit_in,24)
         call datelen(10)
           nmsg=0
           msg_report: do while (ireadmg(unit_in,subset,idate) == 0)
             yyyy=idate/1000000
             hh=idate-yyyy*1000000
             mm=hh/10000
             hh=hh-mm*10000
             dd=hh/100
             hh=hh-dd*100
             cyclepochmins=mt%date2mins(yyyy,mm,dd,hh,0)
             deltahh=float(cyclepochmins-refepochmins)/60.0

             nmsg=nmsg+1
             ntb = 0

             sb_report: do while (ireadsb(unit_in) == 0)
               ntb = ntb+1
               call ufbint(unit_in,hdr,mxmn,1   ,iret,hdstr)
               call ufbint(unit_in,obs,mxmn,mxlv,iret,obstr)
               call ufbint(unit_in,oer,mxmn,mxlv,iret,oestr)
               call ufbint(unit_in,qcf,mxmn,mxlv,iret,qcstr)
               rstation_id=hdr(1)
!          
!  profiler height is above sea level
!  convert it to above surface level
               if(int(hdr(5))==227) then
                  do k=1,iret
                     if(obs(4,k) < 10.0e9) obs(4,k)=obs(4,k)-hdr(6)
                  enddo
               endif
!
               if(int(hdr(5))==this%datatype .or.          &
                  int(hdr(5))==this%datatype_match) then
                  hdr(4)=hdr(4)+deltahh
!                  write(*,*) 
!                  write(*,'(2I10,a14,8f15.1)') ntb,iret,c_sid,(hdr(i),i=2,8)
!                  do i=1,iret
!                    write(*,'(I4,7f16.2)') i,obs(1:7,i)
!                    write(*,'(I4,7f16.2)') i,oer(1:7,i)
!                    write(*,'(I4,7f16.2)') i,qcf(1:7,i)
!                  enddo

                  if(index_sameobs==0) then  ! no saved obs in array 2
                     hdr2=hdr
                     obs2=obs
                     oer2=oer
                     qcf2=qcf
                     iret2=iret
                     index_sameobs=1
                     cycle
                  else if(index_sameobs==1) then  ! has saved obs in array 2
                     if( abs(hdr(2)-hdr2(2)) < 0.01 .and. &  ! found same obs
                         abs(hdr(3)-hdr2(3)) < 0.01 .and. &  ! merge to 3
                         abs(hdr(4)-hdr2(4)) < 0.01 .and. &  ! and write 3
                         abs(hdr(6)-hdr2(6)) < 0.01 ) then
                         hdr3=hdr2
                         kk=1
                         kk2=1
                         do k=1,iret
                            if(abs(obs(1,k)-obs2(1,kk2)) < 0.001) then
                               obs3(:,kk)=min(obs(:,k),obs2(:,kk2))
                               oer3(:,kk)=min(oer(:,k),oer2(:,kk2))
                               qcf3(:,kk)=min(qcf(:,k),qcf2(:,kk2))
                               kk2=kk2+1
                               kk=kk+1 
                            else if( obs(1,k) > obs2(1,kk2)) then
                               obs3(:,kk)=obs(:,k)
                               oer3(:,kk)=oer(:,k)
                               qcf3(:,kk)=qcf(:,k)
                               kk=kk+1 
                            else if( obs(1,k) < obs2(1,kk2)) then
                               kk3=kk2
                               ifnotused=.true.
                               do i=kk3,iret2
                                  if( obs(1,k) < obs2(1,kk2)) then
                                     obs3(:,kk)=obs2(:,kk2)
                                     oer3(:,kk)=oer2(:,kk2)
                                     qcf3(:,kk)=qcf2(:,kk2)
                                     kk=kk+1 
                                     kk2=kk2+1
                                  elseif(abs(obs(1,k) - obs2(1,kk2)) < 0.001) then
                                     obs3(:,kk)=min(obs(:,k),obs2(:,kk2))
                                     oer3(:,kk)=min(oer(:,k),oer2(:,kk2))
                                     qcf3(:,kk)=min(qcf(:,k),qcf2(:,kk2))
                                     kk=kk+1 
                                     kk2=kk2+1
                                     ifnotused=.false.
                                  else
                                     if(ifnotused) then
                                        obs3(:,kk)=obs(:,k)
                                        oer3(:,kk)=oer(:,k)
                                        qcf3(:,kk)=qcf(:,k)
                                        kk=kk+1 
                                        ifnotused=.false.
                                     endif
                                     cycle
                                  endif
                               enddo
                            else
                               write(*,*) 'Something wrong with merge'
                               stop 1234
                            endif
                         enddo
                         if(kk2 < iret2) then
                            do i=1,kk2,iret2
                               obs3(:,kk)=obs2(:,i)
                               oer3(:,kk)=oer2(:,i)
                               qcf3(:,kk)=qcf2(:,i)
                               kk=kk+1 
                            enddo
                         endif
                         iret3=kk-1
                         index_sameobs=0
                     else    ! not the same obs, write 3 and save 2
                        hdr3=hdr2
                        obs3=obs2
                        oer3=oer2
                        qcf3=qcf2
                        iret3=iret2
                        hdr2=hdr
                        obs2=obs
                        oer2=oer
                        qcf2=qcf
                        iret2=iret
                        index_sameobs=1
                     endif
                  endif
                  iobs=iobs+1
!
! save obs in time window 
                 if(abs(hdr3(4)) <= this%time_window) then
                    allocate(thisobs)
                    call thisobs%alloc(this%numvar,iret3,.true.,.true.)
                    thisobs%obs=rmissing
                    rstation_id=hdr3(1)
                    thisobs%name=trim(c_sid)
                    thisobs%lon=hdr3(2)
                    thisobs%lat=hdr3(3)
                    thisobs%ele=hdr3(6)
                    thisobs%time=hdr3(4)
                    do k=1,thisobs%numlvl
                       if(obs3(1,k) < 10.0e9 .and. this%ip>0) thisobs%obs((k-1)*thisobs%numvar+this%ip)=obs3(1,k)
                       if(obs3(3,k) < 10.0e9 .and. this%it>0) thisobs%obs((k-1)*thisobs%numvar+this%it)=obs3(3,k)
                       if(obs3(2,k) < 10.0e9 .and. this%iq>0) thisobs%obs((k-1)*thisobs%numvar+this%iq)=obs3(2,k)/1000.0_r_single
                       if(obs3(4,k) < 10.0e9 .and. this%ih>0) thisobs%obs((k-1)*thisobs%numvar+this%ih)=obs3(4,k)
                       if(obs3(5,k) < 10.0e9 .and. this%iu>0) thisobs%obs((k-1)*thisobs%numvar+this%iu)=obs3(5,k)
                       if(obs3(6,k) < 10.0e9 .and. this%iv>0) thisobs%obs((k-1)*thisobs%numvar+this%iv)=obs3(6,k)
                    enddo
                    if(thisobs%ifquality) then
                       thisobs%quality=imissing
                       do k=1,thisobs%numlvl
                          if(qcf3(1,k) < 10.0e9 .and. this%ip>0) thisobs%quality((k-1)*thisobs%numvar+this%ip)=qcf3(1,k)
                          if(qcf3(3,k) < 10.0e9 .and. this%it>0) thisobs%quality((k-1)*thisobs%numvar+this%it)=qcf3(3,k)
                          if(qcf3(2,k) < 10.0e9 .and. this%iq>0) thisobs%quality((k-1)*thisobs%numvar+this%iq)=qcf3(2,k)
                          if(qcf3(4,k) < 10.0e9 .and. this%ih>0) thisobs%quality((k-1)*thisobs%numvar+this%ih)=qcf3(4,k)
                          if(qcf3(5,k) < 10.0e9 .and. this%iu>0) thisobs%quality((k-1)*thisobs%numvar+this%iu)=qcf3(5,k)
                          if(qcf3(6,k) < 10.0e9 .and. this%iv>0) thisobs%quality((k-1)*thisobs%numvar+this%iv)=qcf3(6,k)
                       enddo
                    endif
                    if(thisobs%iferror) then
                       thisobs%error=rmissing
                       do k=1,thisobs%numlvl
                          if(oer3(1,k) < 10.0e9 .and. this%ip>0) thisobs%error((k-1)*thisobs%numvar+this%ip)=oer3(1,k)
                          if(oer3(3,k) < 10.0e9 .and. this%it>0) thisobs%error((k-1)*thisobs%numvar+this%it)=oer3(3,k)
                          if(oer3(2,k) < 10.0e9 .and. this%iq>0) thisobs%error((k-1)*thisobs%numvar+this%iq)=oer3(2,k)
                          if(oer3(4,k) < 10.0e9 .and. this%ih>0) thisobs%error((k-1)*thisobs%numvar+this%ih)=oer3(4,k)
                          if(oer3(5,k) < 10.0e9 .and. this%iu>0) thisobs%error((k-1)*thisobs%numvar+this%iu)=oer3(5,k)
                          if(oer3(6,k) < 10.0e9 .and. this%iv>0) thisobs%error((k-1)*thisobs%numvar+this%iv)=oer3(6,k)
                       enddo
                    endif

                    call this%findsta(thisobs,foundobs)
                    if(associated(foundobs)) then
                       if(associated(foundobs,this%head)) then
                          if(abs(thisobs%time) < abs(foundobs%time)) then
                             call this%replace(thisobs,foundobs)
                          endif
                       else
                          if(abs(thisobs%time) < abs(foundobs%next%time)) then
                             call this%replace(thisobs,foundobs)
                          endif
                       endif
                    else
                       call this%append(thisobs)
                    endif

                    thisobs=>NULL()
                 endif
               endif

             enddo sb_report
           enddo msg_report
! 
           if(index_sameobs==1) then  ! if there is last obs
              hdr3=hdr2
              obs3=obs2
              oer3=oer2
              qcf3=qcf2
              iret3=iret2

              if(abs(hdr3(4)) <= this%time_window) then
                 allocate(thisobs)
                 call thisobs%alloc(this%numvar,iret3,.true.,.true.)
                 thisobs%obs=rmissing
                 rstation_id=hdr3(1)
                 thisobs%name=trim(c_sid)
                 thisobs%lon=hdr3(2)
                 thisobs%lat=hdr3(3)
                 thisobs%ele=hdr3(6)
                 thisobs%time=hdr3(4)
                 do k=1,thisobs%numlvl
                    if(obs3(1,k) < 10.0e9 .and. this%ip>0) thisobs%obs((k-1)*thisobs%numvar+this%ip)=obs3(1,k)
                    if(obs3(3,k) < 10.0e9 .and. this%it>0) thisobs%obs((k-1)*thisobs%numvar+this%it)=obs3(3,k)
                    if(obs3(2,k) < 10.0e9 .and. this%iq>0) thisobs%obs((k-1)*thisobs%numvar+this%iq)=obs3(2,k)/1000.0_r_single
                    if(obs3(4,k) < 10.0e9 .and. this%ih>0) thisobs%obs((k-1)*thisobs%numvar+this%ih)=obs3(4,k)
                    if(obs3(5,k) < 10.0e9 .and. this%iu>0) thisobs%obs((k-1)*thisobs%numvar+this%iu)=obs3(5,k)
                    if(obs3(6,k) < 10.0e9 .and. this%iv>0) thisobs%obs((k-1)*thisobs%numvar+this%iv)=obs3(6,k)
                 enddo
                 if(thisobs%ifquality) then
                    thisobs%quality=imissing
                    do k=1,thisobs%numlvl
                       if(qcf3(1,k) < 10.0e9 .and. this%ip>0) thisobs%quality((k-1)*thisobs%numvar+this%ip)=qcf3(1,k)
                       if(qcf3(3,k) < 10.0e9 .and. this%it>0) thisobs%quality((k-1)*thisobs%numvar+this%it)=qcf3(3,k)
                       if(qcf3(2,k) < 10.0e9 .and. this%iq>0) thisobs%quality((k-1)*thisobs%numvar+this%iq)=qcf3(2,k)
                       if(qcf3(4,k) < 10.0e9 .and. this%ih>0) thisobs%quality((k-1)*thisobs%numvar+this%ih)=qcf3(4,k)
                       if(qcf3(5,k) < 10.0e9 .and. this%iu>0) thisobs%quality((k-1)*thisobs%numvar+this%iu)=qcf3(5,k)
                       if(qcf3(6,k) < 10.0e9 .and. this%iv>0) thisobs%quality((k-1)*thisobs%numvar+this%iv)=qcf3(6,k)
                    enddo
                 endif
                 if(thisobs%iferror) then
                    thisobs%error=rmissing
                    do k=1,thisobs%numlvl
                       if(oer3(1,k) < 10.0e9 .and. this%ip>0) thisobs%error((k-1)*thisobs%numvar+this%ip)=oer3(1,k)
                       if(oer3(3,k) < 10.0e9 .and. this%it>0) thisobs%error((k-1)*thisobs%numvar+this%it)=oer3(3,k)
                       if(oer3(2,k) < 10.0e9 .and. this%iq>0) thisobs%error((k-1)*thisobs%numvar+this%iq)=oer3(2,k)
                       if(oer3(4,k) < 10.0e9 .and. this%ih>0) thisobs%error((k-1)*thisobs%numvar+this%ih)=oer3(4,k)
                       if(oer3(5,k) < 10.0e9 .and. this%iu>0) thisobs%error((k-1)*thisobs%numvar+this%iu)=oer3(5,k)
                       if(oer3(6,k) < 10.0e9 .and. this%iv>0) thisobs%error((k-1)*thisobs%numvar+this%iv)=oer3(6,k)
                    enddo
                 endif

                 call this%findsta(thisobs,foundobs)
                 if(associated(foundobs)) then
                    call this%replace(thisobs,foundobs)
                 else
                    call this%append(thisobs)
                 endif
   
                 thisobs=>NULL()
              endif

           endif
         call closbf(unit_in)
         close(unit_in)

!         this%n_alloc=iobs
         write(*,*) '>>>decodeprepbufr_all: read in', iobs,' stations for data type',this%datatype

      end subroutine decodeprepbufr_all

end module module_prepbufr
