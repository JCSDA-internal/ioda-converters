module module_obs_base
!
!  this module defines observation data structure and the method to 
!           read and write observations from and to those data structure.
!
!
   use kinds, only: r_kind,r_single,len_sta_name,rmissing

   implicit none

   public :: obsbase
   public :: obslocation

   private

! define a type to hold the observtion location
   type obslocation
      character(len=len_sta_name) :: name  ! station name
      real(r_single)    :: lon   ! station longitude
      real(r_single)    :: lat   ! station latitiude
      real(r_single)    :: ele   ! station evelation  
   end type obslocation

! define a base observation type
   type, extends(obslocation) :: obsbase
      type(obsbase), pointer :: next => NULL()
      real(r_single)    :: time      ! observation time  
      integer :: numlvl              ! number of observation levels
      integer :: numvar              ! number of variable in this obs type
      real(r_single),allocatable :: obs(:)  ! observation value=numlvl*numvar
      logical :: ifquality  ! does this obs include quality information
      integer,allocatable :: quality(:)
      logical :: iferror    ! does this obs include observation error
      real(r_single),allocatable :: error(:)
      contains
         procedure :: list => list_obsbase   ! list content of base obs
         procedure :: alloc => alloc_obsbase ! allocate memory for base obs
         procedure :: destroy => destroy_obsbase  ! deallocate memory for base obs
   end type obsbase

   contains
      subroutine list_obsbase(this)
! list the content in a bas observation
         class(obsbase) :: this

         integer :: numvar,numlvl,obslen
         integer :: i,k
!
         write(*,'(a,a10,4f10.3)') 'observation: name, longitude, latitude, hight, time =', &
                     trim(this%name),this%lon,this%lat,this%ele,this%time

         numvar=this%numvar
         numlvl=this%numlvl
         obslen=numvar*numlvl
         if(obslen >=1) then
            do k=1,numlvl
               write(*,'(a4,I4,10f12.2)') 'obs=',k,(this%obs((k-1)*numvar+i),i=1,numvar)
               if(this%ifquality) &
               write(*,'(a4,I4,10I12)') 'qul=',k,(this%quality((k-1)*numvar+i),i=1,numvar)
               if(this%iferror) &
               write(*,'(a4,I4,10E12.3)') 'err=',k,(this%error((k-1)*numvar+i),i=1,numvar)
            enddo
         else
            write(*,*) 'No obs for this location'
         endif
      
      end subroutine list_obsbase

      subroutine alloc_obsbase(this,numvar,numlvl,ifquality,iferror)
! allocate memory for a base observation variable
!
! input variables:
!        numvar : number of variable in this obs type
!        numlvl : number of observation levels
!        ifquality: does this obs include quality information
!        iferror: does this obs include observation error
!
         class(obsbase) :: this
         integer,intent(in) :: numvar,numlvl
         logical,intent(in),optional :: ifquality,iferror

         integer :: obslen

         obslen=numvar*numlvl
         if(obslen >=1) then
            this%numvar=numvar
            this%numlvl=numlvl
            if(allocated(this%obs)) deallocate(this%obs)
            allocate(this%obs(obslen))
! let's see if need to allocate memory for quality 
            this%ifquality=.false.
            if(present(ifquality)) this%ifquality=ifquality
            if(this%ifquality) then
               allocate(this%quality(obslen))
               this%quality=0
            endif
! let's see if need to allocate memory for obs error 
            this%iferror=.false.
            if(present(iferror)) this%iferror=iferror
            if(this%iferror) then
               allocate(this%error(obslen))
               this%error=0.0
            endif
         else
            write(*,*) 'alloc_obsbase Error: dimension must larger than 0:',numvar,numlvl
            stop 1234
         endif
      
      end subroutine alloc_obsbase

      subroutine destroy_obsbase(this)
! release memory
         class(obsbase) :: this
!
         this%numvar=0
         this%numlvl=0
         this%time=0
         if(allocated(this%obs)) deallocate(this%obs)
         this%ifquality=.false.
         if(allocated(this%quality)) deallocate(this%quality)
         this%iferror=.false.
         if(allocated(this%error)) deallocate(this%error)
         this%next => NULL()
      
      end subroutine destroy_obsbase

end module module_obs_base
