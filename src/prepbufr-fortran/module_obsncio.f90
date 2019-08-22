module module_obsncio

   use kinds, only: r_single,len_sta_name,rmissing,imissing
   use module_obs_conv_pt, only :  obs_conv_pt
   use module_obs_base, only :  obsbase
   use module_prepbufr, only :  read_prepbufr
   use module_time, only : mtime

   implicit none

   public :: obsncio

   private
   type :: obsncio
      integer :: datatype   ! hold wind data type 
      integer :: nvars
      integer :: nlocs
      integer :: nrecs
      integer :: nstring
      integer :: ndatetime
      integer :: maxnumlvl
      character(len=80),allocatable :: variable_names(:)
      contains
         procedure :: initial =>initial_obsncio
         procedure :: destroy =>destroy_obsncio
         procedure :: write => write_obs2nc
   end type
   contains
!===================
      subroutine initial_obsncio(this,obslist)
! initial obs netcdf io 
         implicit none
         class(obsncio) :: this
!         type(obs_conv_pt),pointer,intent(in) :: obslist
         type(read_prepbufr),intent(in) :: obslist
         integer :: datatype

         datatype=obslist%datatype
         this%nvars=obslist%numvar
         this%nlocs=obslist%n_alloc
         this%maxnumlvl=obslist%maxnumlvl
         this%nrecs=3
         this%nstring=50
         this%ndatetime=20
         this%datatype=datatype
         if( datatype==120 .or.  datatype==187 .or. datatype==180) then
            this%nvars=5
           ! allocate(this%variable_names(this%nvars,this%nstring))
            allocate(this%variable_names(this%nvars))
            this%variable_names(1)='surface_pressure'
            this%variable_names(2)='specific_humidity'
            this%variable_names(3)='air_temperature'
            this%variable_names(4)='eastward_wind'
            this%variable_names(5)='northward_wind'
         elseif(datatype==245) then
            this%nvars=2
            allocate(this%variable_names(this%nvars))
            this%variable_names(1)='eastward_wind'
            this%variable_names(2)='northward_wind'
         else
            write(*,*) 'unknown data type in initial_obsncio'
            stop 123
         endif

      end subroutine initial_obsncio

      subroutine destroy_obsncio(this)
! releae memory
         implicit none
         class(obsncio) :: this
         
         this%datatype=1000
         deallocate(this%variable_names)

      end subroutine destroy_obsncio

      subroutine write_obs2nc(this,obslist,outfile)
! read in station info
         use netcdf
         implicit none
         class(obsncio) :: this
!         type(obs_conv_pt),pointer,intent(in) :: obslist
         type(read_prepbufr),intent(in) :: obslist
         character(len=*),intent(in)  :: outfile
!
         type(obsbase),pointer :: thisobs => NULL()

         real(r_single),allocatable :: rs1d(:)
         integer       ,allocatable :: int1d(:)
         character     ,allocatable :: char2d(:,:)
         integer        :: ncid,nlocs_dimid,nrecs_dimid,nvars_dimid
         integer        :: nstring_dimid,ndatetime_dimid
         integer        :: varid_stationid,varid_elv,varid_airprs
         integer        :: varid_height,varid_datetime,varid_varname
         integer        :: varid_lat,varid_lon,varid_time,varid_recn
         integer        :: numfield
         integer,allocatable :: varid_var(:,:)
         character(len=80), allocatable :: cgroupname(:)
         character(len=80)  :: varname
!
         integer :: numobs,i,ie,j,n,k,ivar
         integer :: dimids(2)
!
!
         numfield=11
         allocate(cgroupname(numfield))
         allocate(varid_var(numfield,this%nvars))
         cgroupname(1)='@ObsType'
         cgroupname(2)='@PreUseFlag'
         cgroupname(3)='@GsiUseFlag'
         cgroupname(4)='@GsiQCWeight'
         cgroupname(5)='@GsiAdjustObsError'
         cgroupname(6)='@GsiFinalObsError'
         cgroupname(7)='@GsiHofXBc'
         cgroupname(8)='@GsiHofX'
         cgroupname(9)='@ObsValue'
         cgroupname(10)='@ObsError'
         cgroupname(11)='@PreQC'

         numobs=0

         thisobs => obslist%head
         if(.NOT.associated(thisobs)) then
            write(*,*) 'write_obs2nc: No obs in this variable'
            return
         endif

         do while(associated(thisobs))
            numobs=numobs+thisobs%numlvl
            thisobs => thisobs%next
         enddo
         
         allocate(rs1d(numobs))
         allocate(int1d(numobs))
         
         call check( nf90_create(trim(outfile), nf90_clobber, ncid))
         call check( nf90_def_dim(ncid, 'nvars', this%nvars,   nvars_dimid) )
         call check( nf90_def_dim(ncid, 'nlocs', numobs,   nlocs_dimid) )
         call check( nf90_def_dim(ncid, 'nrecs', this%nrecs,   nrecs_dimid) )
         call check( nf90_def_dim(ncid, 'nstring', this%nstring,   nstring_dimid) )
         call check( nf90_def_dim(ncid, 'ndatetime', this%ndatetime,   ndatetime_dimid) )
         call check( nf90_put_att(ncid, NF90_GLOBAL, 'nrecs', this%nrecs) )
         call check( nf90_put_att(ncid, NF90_GLOBAL, 'nvars', this%nvars) )
         call check( nf90_put_att(ncid, NF90_GLOBAL, 'nlocs', numobs) )
         call check( nf90_put_att(ncid, NF90_GLOBAL, 'date_time', obslist%idate) )
         call check( nf90_put_att(ncid, NF90_GLOBAL, 'missing_value', rmissing) )

         do n=1,this%nvars
            do k=1,numfield
               varname=trim(this%variable_names(n))//trim(cgroupname(k))
               if(k==1 .or. k==2 .or. k==3 .or. k==11) then
                 call check( nf90_def_var(ncid, trim(varname), NF90_INT,   nlocs_dimid, varid_var(k,n)))
                 call check( nf90_put_att(ncid, varid_var(k,n), "_FillValue",imissing))
               else
                 call check( nf90_def_var(ncid, trim(varname), NF90_FLOAT,   nlocs_dimid, varid_var(k,n)))
                 call check( nf90_put_att(ncid, varid_var(k,n), "_FillValue",rmissing))
               endif
            enddo
         enddo

         call check( nf90_def_var(ncid, "time@MetaData",         NF90_FLOAT, nlocs_dimid, varid_time) )
         dimids =  (/ nstring_dimid, nlocs_dimid /)
         call check( nf90_def_var(ncid, "station_id@MetaData",   NF90_CHAR,   dimids, varid_stationid))
         call check( nf90_def_var(ncid, "latitude@MetaData",     NF90_FLOAT, nlocs_dimid, varid_lat) )
         call check( nf90_def_var(ncid, "longitude@MetaData",    NF90_FLOAT, nlocs_dimid, varid_lon) )
         call check( nf90_def_var(ncid, "station_elevation@MetaData",    NF90_FLOAT, nlocs_dimid, varid_elv) )
         call check( nf90_def_var(ncid, "air_pressure@MetaData",    NF90_FLOAT, nlocs_dimid, varid_airprs) )
         call check( nf90_def_var(ncid, "height@MetaData",    NF90_FLOAT, nlocs_dimid, varid_height) )
         dimids =  (/ ndatetime_dimid, nlocs_dimid /)
         call check( nf90_def_var(ncid, "datetime@MetaData",  NF90_CHAR,  dimids, varid_datetime))
         call check( nf90_def_var(ncid, "record_number@MetaData", NF90_INT,   nlocs_dimid, varid_recn))
         dimids =  (/ nstring_dimid, nvars_dimid /)
         call check( nf90_def_var(ncid, "variable_names@MetaData",NF90_CHAR,  dimids, varid_varname))
         
         call check( nf90_enddef(ncid) )

         do n=1,this%nvars
             do k=1,numfield
               if(trim(this%variable_names(n))=="surface_pressure") ivar=obslist%ip
               if(trim(this%variable_names(n))=="specific_humidity") ivar=obslist%iq
               if(trim(this%variable_names(n))=="air_temperature") ivar=obslist%it
               if(trim(this%variable_names(n))=="eastward_wind") ivar=obslist%iu
               if(trim(this%variable_names(n))=="northward_wind") ivar=obslist%iv
               if(k==1 .or. k==2 .or. k==3 .or. k==11) then
                 int1d=imissing
                 if( trim(cgroupname(k))=='@ObsType' ) int1d=this%datatype
                 if( trim(cgroupname(k))=="@PreQC" ) then
                    if(trim(this%variable_names(n))=="surface_pressure") then
                       call getdatai1d(obslist,int1d,numobs,"preqc_sfcprs",ivar)
                    else
                       call getdatai1d(obslist,int1d,numobs,"preqc",ivar)
                    endif
                 endif
                 call check( nf90_put_var(ncid, varid_var(k,n), int1d))
               else
                 rs1d=rmissing
                 if( trim(cgroupname(k))=="@ObsValue" ) then
                    if(trim(this%variable_names(n))=="surface_pressure") then
                       call getdatar1d(obslist,rs1d,numobs,"obs_sfcprs",ivar)
                       rs1d=rs1d*100.0
                    else
                       call getdatar1d(obslist,rs1d,numobs,"obs",ivar)
                    endif
                 endif
                 if( trim(cgroupname(k))=="@ObsError" ) then
                    if(trim(this%variable_names(n))=="surface_pressure") then
                       call getdatar1d(obslist,rs1d,numobs,"error_sfcprs",ivar)
                    else
                       call getdatar1d(obslist,rs1d,numobs,"error",ivar)
                    endif
                    rs1d=2.5
                 endif
                 call check( nf90_put_var(ncid, varid_var(k,n), rs1d))
               endif
            enddo
         enddo

         call getdatar1d(obslist,rs1d,numobs,"time",0)
         call check( nf90_put_var(ncid, varid_time, rs1d) )

         call getdatar1d(obslist,rs1d,numobs,"lat",0)
         call check( nf90_put_var(ncid, varid_lat, rs1d) )

         call getdatar1d(obslist,rs1d,numobs,"lon",0)
         call check( nf90_put_var(ncid, varid_lon, rs1d) )

         call getdatar1d(obslist,rs1d,numobs,"staelv",0)
         call check( nf90_put_var(ncid, varid_elv, rs1d) )

         call getdatar1d(obslist,rs1d,numobs,"airprs",0)
         call check( nf90_put_var(ncid, varid_airprs, rs1d) )

         call getdatar1d(obslist,rs1d,numobs,"height",0)
         call check( nf90_put_var(ncid, varid_height, rs1d) )
 
         call getdatai1d(obslist,int1d,numobs,"recn",0)
         call check( nf90_put_var(ncid, varid_recn, int1d) )

         deallocate(int1d)
         deallocate(rs1d)

         allocate(char2d(this%nstring,numobs))
         call getdatachar(obslist,char2d,this%nstring,numobs,"stationid")
         call check( nf90_put_var(ncid, varid_stationid, char2d,start=(/1,1/),count=(/this%nstring,numobs/)) )

         call getdatachar(obslist,char2d,this%ndatetime,numobs,"datetime")
         call check( nf90_put_var(ncid, varid_datetime, char2d,start=(/1,1/),count=(/this%ndatetime,numobs/)) )

         deallocate(char2d)

         allocate(char2d(this%nstring,this%nvars))
         call getdata_varname(char2d,this%nstring,this%nvars,this%variable_names)
         call check( nf90_put_var(ncid, varid_varname, char2d,start=(/1,1/),count=(/this%nstring,this%nvars/)) )
         deallocate(char2d)

         call check( nf90_close(ncid) )

      end subroutine write_obs2nc

      subroutine check(status)
         use netcdf
         implicit none
!
         integer, intent ( in) :: status

         if(status /= nf90_noerr) then
          print *, trim(nf90_strerror(status))
           stop "Stopped"
         end if

       end subroutine check

       subroutine getdatar1d(obslist,rs1d,numobs,vname,ivar)

         implicit none
         type(read_prepbufr),intent(in) :: obslist
         integer,intent(in)             :: numobs
         real(r_single),intent(inout)   :: rs1d(numobs)
         character(len=*),intent(in)    :: vname
         integer,intent(in)             :: ivar

         type(obsbase),pointer :: thisobs => NULL()

         integer :: i,ie,k
         integer :: numvar

         rs1d=rmissing
         i=1
         thisobs => obslist%head
         do while(associated(thisobs))
            ie=i+thisobs%numlvl-1
            numvar=thisobs%numvar

            if(vname=='lat') then
               rs1d(i:ie)=thisobs%lat
            elseif(vname=='lon') then
               rs1d(i:ie)=thisobs%lon
            elseif(vname=='time') then
               rs1d(i:ie)=thisobs%time
            elseif(vname=='staelv') then
               rs1d(i:ie)=thisobs%ele
            elseif(vname=='airprs') then
               do k=1,thisobs%numlvl
                  rs1d(i+k-1)=thisobs%obs((k-1)*numvar+obslist%ip)
               enddo
            elseif(vname=='height') then
               do k=1,thisobs%numlvl
                  rs1d(i+k-1)=thisobs%obs((k-1)*numvar+obslist%ih)
               enddo
            elseif(vname=='obs_sfcprs') then
               do k=1,thisobs%numlvl
                  rs1d(i+k-1)=thisobs%obs(ivar)
               enddo
            elseif(vname=='obs') then
               do k=1,thisobs%numlvl
                  rs1d(i+k-1)=thisobs%obs((k-1)*numvar+ivar)
               enddo
            elseif(vname=='error' .and. thisobs%iferror) then
               do k=1,thisobs%numlvl
                  rs1d(i+k-1)=thisobs%error((k-1)*numvar+ivar)
               enddo
            elseif(vname=='error_sfcprs' .and. thisobs%iferror) then
               do k=1,thisobs%numlvl
                  rs1d(i+k-1)=thisobs%error(ivar)
               enddo
            endif

            thisobs => thisobs%next
            i=ie+1
         enddo

       end subroutine getdatar1d

       subroutine getdatai1d(obslist,int1d,numobs,vname,ivar)

         implicit none
         type(read_prepbufr),intent(in) :: obslist
         integer,intent(in)             :: numobs
         integer,intent(inout)          :: int1d(numobs)
         character(len=*),intent(in)    :: vname
         integer,intent(in)             :: ivar

         type(obsbase),pointer :: thisobs => NULL()

         integer :: i,j,ie,k
         integer :: numvar

         int1d=imissing
         i=1
         j=1
         thisobs => obslist%head
         do while(associated(thisobs))
            ie=i+thisobs%numlvl-1
            numvar=thisobs%numvar

            if(vname=='preqc' .and. thisobs%ifquality) then
               do k=1,thisobs%numlvl
                  int1d(i+k-1)=thisobs%quality((k-1)*numvar+ivar)
               enddo
           elseif(vname=='preqc_sfcprs' .and. thisobs%ifquality) then
               do k=1,thisobs%numlvl
                  int1d(i+k-1)=thisobs%quality(ivar)
               enddo

            elseif(vname=='recn' ) then
               int1d(i:ie)=j
            endif

            thisobs => thisobs%next
            i=ie+1
            j=j+1
         enddo

       end subroutine getdatai1d

       subroutine getdatachar(obslist,char2d,numd1,numd2,vname)

         implicit none
         type(read_prepbufr),intent(in) :: obslist
         integer,intent(in)             :: numd1,numd2
         character,intent(inout)          :: char2d(numd1,numd2)
         character(len=*),intent(in)    :: vname

         character :: chartemp(numd1)
         integer :: string_length,min_length
         character(len=80)  :: mystring

         type(mtime)    :: mt
         integer        :: refepochmins,obsepochmins
         integer :: iy,im,id,ih,imin,isec

         type(obsbase),pointer :: thisobs => NULL()

         integer :: i,j,ie,k

         char2d=char(0)
         i=1
         j=1
         iy=obslist%idate/1000000
         isec=obslist%idate-iy*1000000
         im=isec/10000
         isec=isec-im*10000
         id=isec/100
         ih=isec-id*100
         imin=obslist%mm
         isec=0
         refepochmins=mt%date2mins(iy,im,id,ih,imin)

         thisobs => obslist%head
         do while(associated(thisobs))
            ie=i+thisobs%numlvl-1

            if(vname=='stationid') then
               string_length=len(trim(thisobs%name))
               min_length=min(numd1,string_length)
               do k=1,min_length
                  chartemp(k)=thisobs%name(k:k)
               enddo
               do k=1,thisobs%numlvl
                  char2d(1:min_length,i+k-1)=chartemp(1:min_length)
               enddo
            elseif(vname=='datetime') then
               obsepochmins=refepochmins+int(thisobs%time*60.0)
               call mt%mins2date(obsepochmins,iy,im,id,ih,imin)
               write(mystring,'(I4,"-",I2.2,"-",I2.2,"T",I2.2,":",I2.2,":",I2.2,"Z")') iy,im,id,ih,imin,isec
               string_length=len(trim(mystring))
               min_length=min(numd1,string_length)
               do k=1,min_length
                  chartemp(k)=mystring(k:k)
               enddo
               do k=1,thisobs%numlvl
                  char2d(1:min_length,i+k-1)=chartemp(1:min_length)
               enddo
            endif

            thisobs => thisobs%next
            i=ie+1
            j=j+1
         enddo

       end subroutine getdatachar

       subroutine getdata_varname(char2d,numd1,numd2,vnamestring)

         implicit none
         integer,intent(in)             :: numd1,numd2
         character,intent(inout)        :: char2d(numd1,numd2)
         character(len=*),intent(in)    :: vnamestring(numd2)

         character :: chartemp(numd1)
         integer :: string_length,min_length

         integer :: i,k

         char2d=char(0)
         do i=1,numd2
            string_length=len(trim(vnamestring(i)))
            min_length=min(numd1,string_length)
            do k=1,min_length
               chartemp(k)=vnamestring(i)(k:k)
            enddo
            char2d(1:min_length,i)=chartemp(1:min_length)
         enddo

       end subroutine getdata_varname

end module module_obsncio
