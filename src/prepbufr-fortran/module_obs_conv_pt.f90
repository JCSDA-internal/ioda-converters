module module_obs_conv_pt
!
!  this module defines observation data structure and the method to 
!           read and write conventional observations.
!
!
   use kinds, only: r_kind,r_single,len_sta_name,rmissing
   use module_obs_base, only : obsbase

   implicit none

   public :: obs_conv_pt
   public :: obsbase_conv

   private
   type :: obsbase_conv
      integer :: datatype
      integer :: numvar
      integer :: ip,it,iq,ih,iu,iv
      character(len=20),allocatable :: varnames(:)
      integer :: maxnumlvl=1
      integer :: n_alloc=0
      integer :: idate,mm
      real(r_single) :: time_window   ! time_window in hour
   endtype obsbase_conv

   type,extends(obsbase_conv) :: obs_conv_pt
!      type(obsbase),pointer :: thisobs => NULL()
      type(obsbase),pointer :: head => NULL()
      type(obsbase),pointer :: tail => NULL()
      contains
          procedure :: initial => initial_conv
          procedure :: list_initial => list_initial_conv
          procedure :: append  => append_conv
          procedure :: replace => replace_conv
          procedure :: findsta => findsta_conv
          procedure :: list    => list_conv
          procedure :: writept => write_conv_point
          procedure :: readpt  => read_conv_point
          procedure :: destroy => destroy_conv
   end type obs_conv_pt

   contains
      subroutine initial_conv(this,itype,yyyymmddhh,mm,time_window)
! initial a sounding list
         class(obs_conv_pt) :: this
         integer, intent(in) :: itype
         integer, intent(in) :: yyyymmddhh,mm
         real(r_single), intent(in):: time_window

!
         this%idate=yyyymmddhh
         this%mm=mm
         this%time_window=time_window
         this%maxnumlvl=1
         this%n_alloc=0

         if(itype==120) then  ! 120 is sounding
            this%datatype=itype
            this%numvar=6

            this%ip=1
            this%it=2
            this%iq=3
            this%ih=4
            this%iu=5
            this%iv=6

            allocate(this%varnames(this%numvar))
            this%varnames(1)='pressure (Pa)'
            this%varnames(2)='temperature (c)'
            this%varnames(3)='moisture (g/kg)'
            this%varnames(4)='height (m)'
            this%varnames(5)='U wind (m/s)'
            this%varnames(6)='V wind (m/s)'
         else if(itype >=180 .and. itype <=189) then  ! 187 is METOR
            this%datatype=itype
            this%numvar=6

            this%ip=1
            this%it=2
            this%iq=3
            this%ih=4
            this%iu=5
            this%iv=6

            allocate(this%varnames(this%numvar))
            this%varnames(1)='pressure (Pa)'
            this%varnames(2)='temperature (c)'
            this%varnames(3)='moisture (g/kg)'
            this%varnames(4)='height (m)'
            this%varnames(5)='U wind (m/s)'
            this%varnames(6)='V wind (m/s)'
         else if(itype >=130 .and. itype <=135) then ! Aircraft
            this%datatype=itype
            this%numvar=6

            this%ip=1
            this%it=2
            this%iq=3
            this%ih=4
            this%iu=5
            this%iv=6

            allocate(this%varnames(this%numvar))
            this%varnames(1)='pressure (Pa)'
            this%varnames(2)='temperature (c)'
            this%varnames(3)='moisture (g/kg)'
            this%varnames(4)='height (m)'
            this%varnames(5)='U wind (m/s)'
            this%varnames(6)='V wind (m/s)'
         else if(itype >=240 .and. itype <=260)  then  ! satwnd
            this%datatype=itype
            this%numvar=6

            this%ip=1
            this%ih=2
            this%iu=3
            this%iv=4

            allocate(this%varnames(this%numvar))
            this%varnames(1)='pressure (Pa)'
            this%varnames(2)='height (m)'
            this%varnames(3)='U wind (m/s)'
            this%varnames(4)='V wind (m/s)'
         else if(itype==227)  then  ! profile and 
            this%datatype=itype
            this%numvar=4

            this%ip=1
            this%ih=2
            this%iu=3
            this%iv=4

            allocate(this%varnames(this%numvar))
            this%varnames(1)='pressure (Pa)'
            this%varnames(2)='height (m)'
            this%varnames(3)='U wind (m/s)'
            this%varnames(4)='V wind (m/s)'
         else
            write(*,*) 'Unknow data type:',itype
            stop 1234
         endif
      
         this%head => NULL()
         this%tail => NULL()

      end subroutine initial_conv

      subroutine list_initial_conv(this)
! initial a sounding list
         class(obs_conv_pt) :: this

         integer :: k
!
         write(*,*) 'List initial setup for ', this%datatype
         write(*,*) 'yyyymmddhh, mm ', this%idate,this%mm
         write(*,*) 'number of variable', this%numvar
         write(*,*) 'variable index: P, T, Q, H, U, V'
         write(*,'(15x,10I3)') this%ip,this%it,this%iq,this%ih,this%iu,this%iv
         write(*,*) 'variable name:'
         do k=1,this%numvar
             write(*,*) k,trim(this%varnames(k))
         enddo 
         
      end subroutine list_initial_conv

      subroutine append_conv(this,targetobs)
! append a observation to a list 
         class(obs_conv_pt) :: this
         type(obsbase),pointer, intent(in) :: targetobs
      
!         write(*,*) 'append obs=',targetobs%name

         if(.not.associated(this%head)) then
            this%n_alloc=1
            this%head => targetobs
            this%tail => this%head
         else
            this%n_alloc=this%n_alloc+1
            this%tail%next => targetobs
            this%tail      => this%tail%next
         endif
         if(associated(this%tail)) this%tail%next => NULL()
         this%maxnumlvl=max(this%maxnumlvl,this%tail%numlvl)

      end subroutine append_conv

      subroutine replace_conv(this,targetobs,foundobs)
! replace or append a observation to a list 
         class(obs_conv_pt) :: this
         type(obsbase),pointer, intent(in) :: targetobs
         type(obsbase),pointer, intent(in) :: foundobs
         type(obsbase),pointer :: tmpobs=>NULL(), tmpobsnext=>NULL()
      
         if(associated(foundobs,this%head)) then
            tmpobs=>this%head
            targetobs%next=>tmpobs%next
            this%head=>targetobs
         elseif(associated(foundobs%next,this%tail)) then
            tmpobs=>this%tail
            targetobs%next=>NULL()
            foundobs%next =>targetobs
            this%tail=>targetobs
         else
            tmpobs=>foundobs%next
            targetobs%next=>tmpobs%next
            foundobs%next =>targetobs
         endif
!            write(*,*) 'found obs to replace =',targetobs%name,targetobs%time,tmpobs%name,tmpobs%time

         call tmpobs%destroy()
         tmpobs=>NULL()

      end subroutine replace_conv

      subroutine destroy_conv(this)
! release memory
         class(obs_conv_pt) :: this
         type(obsbase), pointer :: thisobs,thisobsnext
      
!
         this%datatype=0
         this%numvar=0
         deallocate(this%varnames)
         this%ip=0
         this%it=0
         this%iq=0
         this%ih=0
         this%iu=0
         this%iv=0

         thisobs => this%head
         if(.NOT.associated(thisobs)) then
            write(*,*) 'No memory to release'
            return
         endif
         do while(associated(thisobs))
!            write(*,*) 'destroy ==',thisobs%name

            thisobsnext => thisobs%next
            call thisobs%destroy()
            thisobs => thisobsnext
         enddo

      end subroutine destroy_conv

      subroutine list_conv(this,minlist)
! display the content in a conventional obs list
         class(obs_conv_pt) :: this
         type(obsbase), pointer :: thisobs
         integer, intent(in), optional :: minlist
         integer :: listnum,num
      
         write(*,*) 
         write(*,*) '====list this observation====',this%datatype
         write(*,*) 'maximum level is=',this%maxnumlvl,this%idate
         write(*,*) 'variable index: P, T, Q, H, U, V'
         write(*,'(15x,10I3)') this%ip,this%it,this%iq,this%ih,this%iu,this%iv

         thisobs => this%head
         if(.NOT.associated(thisobs)) then
            write(*,*) 'list_conv: No obs in this variable'
             return
         endif

         num=0
         listnum=this%n_alloc
         if(present(minlist)) listnum=min(this%n_alloc,minlist)
         
         do while(associated(thisobs))
            num=num+1
            if(num <= listnum) call thisobs%list()
            thisobs => thisobs%next 
         enddo
         write(*,*) 
      
      end subroutine list_conv
!
      subroutine findsta_conv(this,targetobs,foundobs)
! display the content in a conventional obs list
         class(obs_conv_pt) :: this
         type(obsbase), pointer :: thisobs
         type(obsbase),intent(in),pointer :: targetobs
         type(obsbase),intent(inout),pointer :: foundobs
      
         foundobs=>NULL()

         thisobs => this%head
         if(.NOT.associated(thisobs)) then
!            write(*,*) 'findsta_conv: No obs in this variable'
             return
         endif
         if(abs(thisobs%lon-targetobs%lon) < 0.001 .and. &
            abs(thisobs%lat-targetobs%lat) < 0.001 .and. &
            abs(thisobs%ele-targetobs%ele) < 0.001) then
               foundobs=>thisobs
               return
         endif

         do while(associated(thisobs%next))
            if(abs(thisobs%next%lon-targetobs%lon) < 0.001 .and. &
               abs(thisobs%next%lat-targetobs%lat) < 0.001 .and. &
               abs(thisobs%next%ele-targetobs%ele) < 0.001) then
               foundobs=>thisobs
               return
            endif
            thisobs => thisobs%next 
         enddo
      
      end subroutine findsta_conv

      subroutine read_conv_point(this,filename)
! read in station info
         implicit none
         class(obs_conv_pt) :: this
         character(len=*),intent(in) :: filename
        
         type(obsbase),pointer :: thisobs=>NULL()

         integer :: i,j,k,numobs
         integer :: iunit
         logical :: ifexist

         iunit=10
         write(*,*) trim(filename)
         inquire( file=trim(filename), EXIST=ifexist )
         if(ifexist) then
            open(iunit,file=trim(filename),form='unformatted',status='old')
               read(iunit) this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
               allocate(this%varnames(this%numvar))
               read(iunit) this%varnames(1:this%numvar)
               read(iunit) this%ip,this%it,this%iq,this%ih,this%iu,this%iv,this%idate,this%mm

               do i=1,this%n_alloc
                  allocate(thisobs)
                  read(iunit) thisobs%numlvl
                  read(iunit) thisobs%name,thisobs%lon,thisobs%lat,   &
                        thisobs%ele,thisobs%time,                  &
                        thisobs%ifquality,thisobs%iferror

                  call thisobs%alloc(this%numvar,thisobs%numlvl,thisobs%ifquality,thisobs%iferror)
                  read(iunit) thisobs%obs 
                  if(thisobs%ifquality) read(iunit) thisobs%quality
                  if(thisobs%iferror) read(iunit) thisobs%error
                  call this%append(thisobs)
                  thisobs=>NULL()
               enddo

            close(iunit)
         else
            this%n_alloc=0
         endif

      end subroutine read_conv_point

      subroutine write_conv_point(this,filename)
! read in station info
         implicit none
         class(obs_conv_pt) :: this
         character(len=*),intent(in) :: filename
        
         type(obsbase), pointer :: thisobs=>NULL()
         integer :: iunit
         character(len=180) :: filenameall
         character(len=12) :: timetag

         iunit=10

         write(timetag,'(I10,I2.2)') this%idate,this%mm

         write(filenameall,'(a,a,I4.4,3a)') trim(filename),'_type',this%datatype,'_',timetag,'.bin'  

         if(this%n_alloc>0) then
            write(*,'(a,I8,a,I8)') '>>>write_conv_point: Write',this%n_alloc,& 
                                 ' obs for data type:',this%datatype
            write(*,*) 'save file=',trim(filenameall)
         else
            write(*,*) '>>>write_conv_point: NO ',this%datatype,' station'
            return
         endif

         thisobs => this%head
         if(.NOT.associated(thisobs)) then
            write(*,*) 'write_conv_point: No obs in this variable'
            return
         endif

         open(iunit,file=trim(filenameall),form='unformatted')
            write(iunit) this%n_alloc,this%numvar,this%maxnumlvl,this%datatype
            write(iunit) this%varnames(1:this%numvar)
            write(iunit) this%ip,this%it,this%iq,this%ih,this%iu,this%iv,this%idate,this%mm

            do while(associated(thisobs))
               write(iunit)thisobs%numlvl
               write(iunit) thisobs%name,thisobs%lon,thisobs%lat,   &
                        thisobs%ele,thisobs%time,                   &
                        thisobs%ifquality,thisobs%iferror
               write(iunit) thisobs%obs  
               if(thisobs%ifquality) write(iunit) thisobs%quality
               if(thisobs%iferror) write(iunit) thisobs%error
               thisobs => thisobs%next
            enddo

         close(iunit)

      end subroutine write_conv_point

end module module_obs_conv_pt
