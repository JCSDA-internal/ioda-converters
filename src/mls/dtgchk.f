      function dtgchk(dtg)
C------------------------------------------------------------------------------C
C The original function will not compile with JEDI with this message: 
C A CHARACTER function name must not be declared with an asterisk 
C type-param-value (i.e., (LEN=*)) if the function is part of an interface-body.
C No interface block will be generated.   [DTGCHK]
C As a simple fix, the size of the string was to 10 characters 
C all references to dtgchk(11) and dtgchk(12) were commented out
C Look for commented lines starting by CFV. Francois Vandenberghe, May 2026 
C------------------------------------------------------------------------------C
C
C.............................START PROLOGUE............................
C
C SCCS IDENTIFICATION:   $HeadURL$
C SCCS IDENTIFICATION:   @(#)$Id$
C                        15:18:48 /h/cm/library/nognav/src/sub/fcst/dtgchk.f_v
C
C CONFIGURATION IDENTIFICATION:  None
C
C MODULE NAME:   dtgchk
C
C DESCRIPTION:
C
C        This function checks to see if a dtg is valid.
C
C CONTRACT NUMBER AND TITLE:  None
C
C REFERENCES: Programmers Thomas E. Rosmond (mostly),
C             Timothy F. Hogan, Ron Gelaro,  and/or Jim Ridout (NRL)
C
C CLASSIFICATION:  Unclassified
C
C RESTRICTIONS: None
C
C COMPILER DEPENDENCIES: FORTRAN 77, FORTRAN 90
C
C COMPILE OPTIONS:
C Fortran 90: -O2 -mips4 -64 -i4 -r8  -LANG:recursive=on -OPT:IEEE_arith=2:ro=2:SWP=on:Oli
C             -LNO:prefetch=2:prefetch_ahead=2
C Fortran 90: -O2 -mips4 -mp  -64 -i4 -r8 -extend_source -OPT:IEEE_arith=2:ro=2:SWP=on:Oli
C             -LNO: prefetch=2:prefetch_ahead=2
C
C LIBRARIES OF RESIDENCE: /a/ops/lib/librt.a
C
C USAGE: dtgerr = dtgchk(dtg)
C
C PARAMETERS:
C      Name            Type         Usage            Description
C   ----------      ----------     -------  ----------------------------
C
C COMMON BLOCKS:
C      Block      Name     Type    Usage              Notes
C     --------  --------   ----    ------   ------------------------
C
C FILES: None
C
C DATA BASES: None
C
C NON-FILE INPUT/OUTPUT: None
C
C ERROR CONDITIONS: None
C
C ADDITIONAL COMMENTS: None
C
C.................MAINTENANCE SECTION................................
C
C MODULES CALLED:
C         Name           Description
C        -------     ----------------------
C LOCAL VARIABLES AND
C          STRUCTURES:
C Name                  Type    Description
C -------------------   ------  -----------
C
C METHOD:
C
C See the latest edition of
C                         The NOGAPS Forecast Model:
C                         A Technical Description
C                         by T.F. Hogan, T.E. Rosmond, and R. Gelaro
C
C FILES: None
C
C INCLUDE FILES: None
C
C MAKEFILE:
C
C <<CHANGE NOTICE>> version 1.0 (12 Jan 1994) -- Pauley, R.
C   Initial installation under configuration management.
C <<CHANGE NOTICE>> (25 Nov 1997) --  Caudle, K., Hogan, T.
C   Standardization modifications.
C..............................END PROLOGUE.............................
c
c.........start prologue.........................................
c
c  subprogram name:  dtgchk
c
c  description:  check to see if a dtg is valid.
c
c  original programmer, date:  s. glassner, 5 march 1991
c
c  current programmer (util*):  s. glassner
c
c  computer/operating system: sun/unix, cray/unicos
c
c  libraries of residence(util*):
c
c  classification:  unclas
c
c  usage (calling sequence):
c
c    character*10 dtg, errdtg
c    errdtg = dtgchk ( dtg )
c            or
c    character*12 dtg, errdtg
c    errdtg = dtgchk ( dtg )
c
c  input parameters:
c
c    dtg           	  date-time group in one of two formats:
c		c*10        yyyymmddhh
c		c*12	    yyyymmddhhmm
c
c  output parameters:
c
c    errdtg	c*10/12   error designator.  all blank if dtg was valid.
c			    characters 1-4   = **** if year was bad.
c		            characters 5-6   = **   if month was bad.
c			    characters 7-8   = **   if day was bad.
c			    characters 9-10  = **   if hour was bad.
c			    characters 11-12 = **   if minutes were bad.
c
c  calls:
c
c    len	returns the length of a string.
c
c  example:
c
c    1)  character*10 dtgchk, dtg, errdtg
c
c	 data dtg / '1995070312' /
c
c        errdtg = dtgchk ( dtg )
c	 if ( errdtg .eq. ' ' ) then
c	    no error...
c	 else
c	    error...
c	 endif
c
c        errdtg will be blank.
c
c    2)  character*12 dtgchk, dtg, errdtg
c
c	 data dtg / '198607031245' /
c
c        errdtg = dtgchk ( dtg )
c	 if ( errdtg .eq. ' ' ) then
c	    no error...
c	 else
c	    error...
c	 endif
c
c        errdtg will be blank.
c
c    3)  character*12 dtgchk, dtg, errdtg
c
c	 data dtg / '198677031290' /
c
c        errdtg = dtgchk ( dtg )
c	 if ( errdtg .eq. ' ' ) then
c	    no error...
c	 else
c	    error...
c	 endif
c
c        errdtg will look like this, where a period denotes a blank:
c	   ....**....**	 meaning that the month and minutes are invalid.
c
c  restrictions:
c
c    the dtg may not contain any blanks.
c    the dtg must be either 10 or 12 digits long.
c    dtgchk rejects dtg years that are outside the range 1800-2799.
c
c  error conditions:
c
c    non-numeric dtg. dtgchk will be asterisk-filled.
c
c.........maintenance section......................................
c
c  principal variables and arrays:
c
c    iday	int	integer day from the input dtg.
c    ihour	int	integer hour from the input dtg.
c    imo	int	integer month from the input dtg.
c    iyr	int	integer year from the input dtg.
c    length	int	gets the return from len, i.e. the length
c			  of the argument string.
c    min	int	integer minutes from the input dtg.
c    month(12)	int	an array containing the number of days in
c			  each month.
c
c  method:
c
c    check that each value is numeric and within the ranges:
c	year:  1800-2799.
c	month: 1-12.
c	day:   1-31 for jan, mar, may, july, aug, oct and dec.
c	       1-30 for apr, june, sep, nov.
c	       28 or 29 for feb, depending on whether year is a leap year.
c		  note that 1900 is not a leap year and 2000 is.
c	hour:  0-23
c	min:   0-59
c    whenever a value is outside its range, set the corresponding characters
c	of dtgchk to asterisks.
c
c  language (util*):  fortran 77
c
c  record of changes:
c
c <<change notice>> version 1.0 (17 mar 1992) -- kunitani, c. (cri)
c                   change to dtgchk required to port to cray:
c                      from implicit undefined ( a-z )
c                      to   implicit none
c
c.........end prologue..................................................
c
      implicit none
c
CFV   character dtgchk*(*), dtg*(*)
      character(len=10) dtgchk, dtg
c
      integer month(12), iyr, imo, iday, ihour, min, length, i
c
      data month/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
c
      length = len(dtg)
      if(length.ne.10 .and. length.ne.12)then
        dtgchk = '************'
        go to 200
      endif

****************************************************************************
*        make sure every character in the dtg is a number.
****************************************************************************

      do 100 i = 1, length
        if(dtg(i:i).lt.'0' .or. dtg(i:i).gt.'9')then
c
          if(length.eq.10)then
            dtgchk = '**********'
          else
            dtgchk = '************'
          endif
c
          go to 200
        endif
  100 continue

****************************************************************************
*        month(2) will be changed to 29 later if the year is a leap year.
****************************************************************************

      month(2) = 28

****************************************************************************
*        get the integer versions of the year, month, day, and hour.
****************************************************************************

      read(dtg,'(i4,3i2)')iyr, imo, iday, ihour

****************************************************************************
*       get minutes if they're there.
****************************************************************************

      dtgchk(1:length) = ' '
c
CFV   if(length.eq.12)then
CFV     if((dtg(11:11).ge.'0') .and. (dtg(11:11).le.'9') .and.
CFV  &     (dtg(12:12).ge.'0') .and. (dtg(12:12).le.'9'))then
CFV       read(dtg,'(10x, i2)')min
CFV     else
CFV       min = 99
CFV     endif
CFV   endif

****************************************************************************
*        check out the dtg.
*          year...
****************************************************************************

      if((iyr.lt.1800) .or. (iyr.gt.2799))dtgchk(1:4) = '****'

****************************************************************************
*          month...
****************************************************************************

      if((imo.lt.1) .or. (imo.gt.12))dtgchk(5:6) = '**'

****************************************************************************
*          day...
*	     set february days to 29 if this is a leap year.
*
*	     if dtgchk(1:6) (year/month) already has asterisks, it was
*              invalid, so don't do the day check.  if the year was invalid,
*	       we can't see if it was a leap year.  if the month was
*	       invalid, we can't see if it was february.
*	     see if this is a leap year, i.e. evenly divisible by four or,
*	       if it's a century (dtg(3:4) = '00'), evenly divisible
*	       by 400.
****************************************************************************

      if(dtgchk(1:6).eq.'  ')then
        if((dtg(3:4).ne.'00') .and. (mod(iyr,4).eq.0) .or.
     &     ((dtg(3:4).eq.'00').and.(mod(iyr,400).eq.0)))month(2) = 29
c
        if((iday.lt.1) .or. (iday.gt.month(imo)))dtgchk(7:8) = '**'
      endif

****************************************************************************
*          hour...
****************************************************************************

      if((ihour.lt.0) .or. (ihour.gt.23))dtgchk(9:10) = '**'

****************************************************************************
*	   minutes...
****************************************************************************

CFV   if(length.eq.12)then
CFV     if((min.lt.0) .or. (min.gt.59))dtgchk(11:12) = '**'
CFV   endif
c
  200 continue
      return
      end
