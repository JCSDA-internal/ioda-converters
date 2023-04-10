program bufr2nc

   use define_mod, only: write_nc_conv, write_nc_radiance
   use kinds, only: i_kind
   use prepbufr_mod, only: read_prepbufr, sort_obs_conv, filter_obs_conv
   use radiance_mod, only: read_amsua_amsub_mhs, read_airs_colocate_amsua, sort_obs_radiance
   use ncio_mod, only: write_obs
   use gnssro_bufr2ioda, only: read_write_gnssro

   implicit none

   integer(i_kind), parameter :: StrLen = 512
   integer(i_kind), parameter :: NameLen = 64
   integer(i_kind), parameter :: DateLen = 10
   integer(i_kind), parameter :: nfile_all = 5
   integer(i_kind), parameter :: ftype_unknown = -1
   integer(i_kind), parameter :: ftype_prepbufr = 1
   integer(i_kind), parameter :: ftype_gnssro = 2
   integer(i_kind), parameter :: ftype_amsua = 3
   integer(i_kind), parameter :: ftype_mhs = 4
   integer(i_kind), parameter :: ftype_airs = 5

   integer(i_kind)            :: ftype(nfile_all)
   character(len=NameLen)     :: flist_all(nfile_all) = &
                                 (/ &
                                 "gnssro.bufr    ", &
                                 "prepbufr.bufr  ", &
                                 "amsua.bufr     ", &
                                 "airs.bufr      ", &
                                 "mhs.bufr       " &
                                 /)
   character(len=NameLen) :: flist(nfile_all)  ! file names to be read in from command line arguments
   character(len=NameLen) :: filename
   character(len=DateLen) :: filedate
   character(len=StrLen)  :: inpdir, outdir
   logical                 :: fexist
   logical                 :: do_radiance
   logical                 :: apply_gsi_qc
   integer(i_kind)         :: nfile, ifile
   integer(i_kind)         :: itmp

   do_radiance = .false. ! initialize
   apply_gsi_qc = .false.

   call parse_files_to_convert

   do ifile = 1, nfile

      filename = flist(ifile)

      if (ftype(ifile) == ftype_gnssro) then
         inquire (file=trim(inpdir)//trim(filename), exist=fexist)
         if (.not. fexist) then
            write (*, *) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
         else
            write (*, *) '--- processing gnssro.bufr ---'
            call read_write_gnssro(trim(inpdir)//trim(filename), trim(outdir))
         end if
      end if

      if (ftype(ifile) == ftype_prepbufr) then
         inquire (file=trim(inpdir)//trim(filename), exist=fexist)
         if (.not. fexist) then
            write (*, *) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
         else
            ! read prepbufr file and store data in sequential linked list for conv obs
            call read_prepbufr(trim(inpdir)//trim(filename), filedate)

            if (apply_gsi_qc) then
               write (*, *) '--- applying some additional QC as in GSI read_prepbufr.f90 for the global model ---'
               call filter_obs_conv
            end if

            ! transfer info from limked list to arrays grouped by obs/variable types
            call sort_obs_conv

            ! write out netcdf files
            call write_obs(filedate, write_nc_conv, outdir)
         end if
      end if

      if (ftype(ifile) == ftype_amsua) then
         inquire (file=trim(inpdir)//trim(filename), exist=fexist)
         if (.not. fexist) then
            write (*, *) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
         else
            do_radiance = .true.
            ! read bufr file and store data in sequential linked list for radiances
            call read_amsua_amsub_mhs(trim(inpdir)//trim(filename), filedate)
         end if
      end if

      if (ftype(ifile) == ftype_airs) then
         inquire (file=trim(inpdir)//trim(filename), exist=fexist)
         if (.not. fexist) then
            write (*, *) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
         else
            do_radiance = .true.
            ! read bufr file and store data in sequential linked list for radiances
            call read_airs_colocate_amsua(trim(inpdir)//trim(filename), filedate)
         end if
      end if

      if (ftype(ifile) == ftype_mhs) then
         inquire (file=trim(inpdir)//trim(filename), exist=fexist)
         if (.not. fexist) then
            write (*, *) 'Warning: ', trim(inpdir)//trim(filename), ' not found for decoding...'
         else
            do_radiance = .true.
            ! read bufr file and store data in sequential linked list for radiances
            call read_amsua_amsub_mhs(trim(inpdir)//trim(filename), filedate)
         end if
      end if

   end do ! nfile list

   if (do_radiance) then
      ! transfer info linked list to arrays grouped by satellite instrument types
      call sort_obs_radiance

      ! write out netcdf files
      call write_obs(filedate, write_nc_radiance, outdir)
   end if

   write (6, *) 'all done!'

contains

   subroutine parse_files_to_convert

      implicit none

      integer(i_kind)       :: iunit = 21
      integer(i_kind)       :: narg, iarg, iarg_inpdir, iarg_outdir
      integer(i_kind)       :: itmp
      integer(i_kind)       :: iost, iret, idate
      character(len=StrLen) :: strtmp
      character(len=8)      :: subset

      narg = command_argument_count()
      ifile = 0
      inpdir = '.'
      outdir = '.'
      flist(:) = 'null'
      if (narg > 0) then
         do iarg = 1, narg
            call get_command_argument(number=iarg, value=strtmp)
            if (trim(strtmp) == '-qc') then
               apply_gsi_qc = .true.
            else if (trim(strtmp) == '-i') then
               iarg_inpdir = iarg + 1
            else if (trim(strtmp) == '-o') then
               iarg_outdir = iarg + 1
            else
               if (iarg == iarg_inpdir) then
                  call get_command_argument(number=iarg, value=inpdir)
               else if (iarg == iarg_outdir) then
                  call get_command_argument(number=iarg, value=outdir)
               else
                  ifile = ifile + 1
                  call get_command_argument(number=iarg, value=flist(ifile))
               end if
            end if
         end do
         if (ifile == 0) then
            nfile = nfile_all
            flist(:) = flist_all(:)
            ftype(:) = (/ftype_gnssro, ftype_prepbufr, ftype_amsua, ftype_airs, ftype_mhs/)
         else
            nfile = ifile
         end if
      else
         inpdir = '.'
         outdir = '.'
         nfile = nfile_all
         flist(:) = flist_all(:)
         ftype(:) = (/ftype_gnssro, ftype_prepbufr, ftype_amsua, ftype_airs, ftype_mhs/)
      end if

      itmp = len_trim(inpdir)
      if (inpdir(itmp:itmp) /= '/') inpdir = trim(inpdir)//'/'
      itmp = len_trim(outdir)
      if (outdir(itmp:itmp) /= '/') outdir = trim(outdir)//'/'

! use default file lists if not set in command-line arguemnt
      if (narg == 0 .or. ifile == 0) return

! determine the input file type
      fileloop: do ifile = 1, nfile
         if (trim(flist(ifile)) == 'null') then
            ftype(ifile) = ftype_unknown
            cycle fileloop
         end if
         open (unit=iunit, file=trim(inpdir)//trim(flist(ifile)), form='unformatted', iostat=iost, status='old')
         call openbf(iunit, 'IN', iunit)
         call readmg(iunit, subset, idate, iret)
         select case (trim(subset))
         case ('ADPUPA', 'ADPSFC')
            ftype(ifile) = ftype_prepbufr
         case ('NC003010')
            ftype(ifile) = ftype_gnssro
         case ('NC021023')
            ftype(ifile) = ftype_amsua
         case ('NC021027')
            ftype(ifile) = ftype_mhs
         case ('NC021249')
            ftype(ifile) = ftype_airs
         case default
            ftype(ifile) = ftype_unknown
         end select
         call closbf(iunit)
      end do fileloop

   end subroutine parse_files_to_convert

end program bufr2nc
