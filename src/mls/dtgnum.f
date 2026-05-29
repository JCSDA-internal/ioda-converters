      subroutine dtgnum(indtg,iyr,imo,iday,ihour,iyrday,iyrhrs,istat)
C.........START PROLOGUE.........................................
C SCCS IDENTIFICATION:   $HeadURL$
C SCCS IDENTIFICATION:   @(#)$Id$
C                        15:19:10 /h/cm/library/nognav/src/sub/fcst/dtgnum.f_v
C
C  SUBPROGRAM NAME:  DTGNUM
C
C  DESCRIPTION:  Given a DTG (YYYYMMDDHH), return integer values for
C		 year, month, day, hour, days into the year, and hours
C		 into the year.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):   S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT )
C
C  INPUT PARAMETERS:
C
C    INDTG	C*10	    Day-time group, yyyymmddhh
C
C  OUTPUT PARAMETERS:
C
C    IYR	INT	    Year.
C    IMO	INT	    Month.
C    IDAY	INT         Day.
C    IHOUR	INT	    Hour.
C    IYRDAY	INT	    Days of the year.
C    IYRHRS	INT	    Hours of the year.
C    ISTAT	INT	    Error status return, = 0, OK; = -1,
C			     bad input DTG.
C
C  CALLS:
C
C    DTGCHK	A function that validates DTGs.  Output is a 10-character
C		  string that is blank if the DTG is valid.  It will
C		  contain asterisks corresponding to where the DTG is invalid
C		  otherwise.
C
C  ERRORS CONDITIONS:
C
C    Invalid DTG.  All integers will return with zero. ISTAT will be
C     set to -1.
C
C  EXAMPLE:
C
C    CHARACTER*10 INDTG
C
C    INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT
C    DATA INDTG / '1990120312' /
C
C    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
C   *              ISTAT )
C
C    		...
C
C    Values returned will be:
C	IYR     - 1990
C	IMO     -   12
C	IDAY    -   03
C	IHOUR	-   12
C	IYRDAY  -  337
C 	IYRHRS  - 8076
C       ISTAT   -    0
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    DTGERR	 C*10	Holds error return from DTGCHK.
C    IDX         INT    Set to 1 if the input year is not a leap
C			  year. If it is a leap year, it will be
C			  set to 2. Used as a subscript for the
C			  month array.
C    MONTH(12,2) INT	Number of days elapsed before getting to
C          	          this month.  There are two sets
C			  of these, one for non-leap years (second
C			  subscript = 1), one for leap years (second
C			  subscript = 2).  For example, MONTH(3,2),
C			  for March in a leap year, contains 60, the
C			  the number of days elapsed before March.
C			  The idea is that if IDAYS comes up with
C			  64 days in a leap year, we know the month
C			  should be March because it is greater than
C			  60 and less than the end of March, 91.
C
C  METHOD:
C
C    1.  Do an internal read of the input DTG to get year, month,
C	 day, hour.  Call DTGCHK to make sure it's valid first.
C	 If it is invalid, set ISTAT to 0 and return.
C    2.  Set leap year index, IDX.
C    3.  Calculate number of days in the year by adding IDAY to the
C	 number of days elapsed before the first of the month, IMO.
C	 Days elapsed is in the MONTH array.
C    4.  Calculate number of hours into the year by multiplying
C        whole days of the year (IYRDAY-1) by 24 hours and adding
C        IHOUR.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGNUM required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
c
c.........start prologue.........................................
c
c  subprogram name:  dtgnum
c
c  description:  given a dtg (yyyymmddhh), return integer values for
c		 year, month, day, hour, days into the year, and hours
c		 into the year.
c
c  original programmer, date:  s. glassner, 5 march 1991
c
c  current programmer (util*):   s. glassner
c
c  computer/operating system: sun/unix, cray/unicos
c
c  libraries of residence(util*):
c
c  classification:  unclas
c
c  usage (calling sequence):
c
c    call dtgnum ( indtg, iyr, imo, iday, ihour, iyrday, iyrhrs, istat )
c
c  input parameters:
c
c    indtg	c*10	    day-time group, yyyymmddhh
c
c  output parameters:
c
c    iyr	int	    year.
c    imo	int	    month.
c    iday	int         day.
c    ihour	int	    hour.
c    iyrday	int	    days of the year.
c    iyrhrs	int	    hours of the year.
c    istat	int	    error status return, = 0, ok; = -1,
c			     bad input dtg.
c
c  calls:
c
c    dtgchk	a function that validates dtgs.  output is a 10-character
c		  string that is blank if the dtg is valid.  it will
c		  contain asterisks corresponding to where the dtg is invalid
c		  otherwise.
c
c  errors conditions:
c
c    invalid dtg.  all integers will return with zero. istat will be
c     set to -1.
c
c  example:
c
c    character*10 indtg
c
c    integer iyr, imo, iday, ihour, iyrday, iyrhrs, istat
c    data indtg / '1990120312' /
c
c    call dtgnum ( indtg, iyr, imo, iday, ihour, iyrday, iyrhrs,
c   *              istat )
c
c    		...
c
c    values returned will be:
c	iyr     - 1990
c	imo     -   12
c	iday    -   03
c	ihour	-   12
c	iyrday  -  337
c 	iyrhrs  - 8076
c       istat   -    0
c
c.........maintenance section......................................
c
c  principal variables and arrays:
c
c    dtgerr	 c*10	holds error return from dtgchk.
c    idx         int    set to 1 if the input year is not a leap
c			  year. if it is a leap year, it will be
c			  set to 2. used as a subscript for the
c			  month array.
c    month(12,2) int	number of days elapsed before getting to
c          	          this month.  there are two sets
c			  of these, one for non-leap years (second
c			  subscript = 1), one for leap years (second
c			  subscript = 2).  for example, month(3,2),
c			  for march in a leap year, contains 60, the
c			  the number of days elapsed before march.
c			  the idea is that if idays comes up with
c			  64 days in a leap year, we know the month
c			  should be march because it is greater than
c			  60 and less than the end of march, 91.
c
c  method:
c
c    1.  do an internal read of the input dtg to get year, month,
c	 day, hour.  call dtgchk to make sure it's valid first.
c	 if it is invalid, set istat to 0 and return.
c    2.  set leap year index, idx.
c    3.  calculate number of days in the year by adding iday to the
c	 number of days elapsed before the first of the month, imo.
c	 days elapsed is in the month array.
c    4.  calculate number of hours into the year by multiplying
c        whole days of the year (iyrday-1) by 24 hours and adding
c        ihour.
c
c  language (util*):  fortran 77
c
c  record of changes:
c
c <<change notice>> version 1.0 (17 mar 1992) -- kunitani, c. (cri)
c                   changes to dtgnum required to port to cray:
c                      from implicit undefined ( a-z )
c                      to   implicit none
c
c.........end prologue..................................................

c
      implicit none

      character*10 indtg, dtgerr, dtgchk
c
      integer iyr, imo, iday, ihour, iyrday, iyrhrs, istat, idx
      integer month(12,2)
c
      data month/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334,
     &     0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/
c
      data dtgerr/'          '/
c
      iyr = 0
      imo = 0
      iday = 0
      ihour = 0
      iyrday = 0
      iyrhrs = 0
      istat = 0

************************************************************************
*	 call dtgchk to validate the dtg.
*	 if it's ok, do an internal read of the integer date parts.
************************************************************************

      dtgerr = dtgchk(indtg)
      if(dtgerr.ne.'          ')then
        istat = -1
        go to 100
      else
        read(indtg,'(i4, 3i2)')iyr, imo, iday, ihour
      endif

************************************************************************
*        set the leap year index, idx, to 1 if non-leap year and 2
*	   if leap year.
*	 if year is a century, it's a leap year if it's evenly divisible
*	   by 400. if it's not a century, it's a leap year if it's
*	   evenly divisible by 4.
************************************************************************

      if(mod(iyr,100).eq.0)then
        if(mod(iyr,400).eq.0)then
          idx = 2
        else
          idx = 1
        endif

      elseif(mod(iyr,4).eq.0)then
        idx = 2

      else
        idx = 1
      endif
c
      iyrday = month(imo,idx) + iday
      iyrhrs = (iyrday-1)*24 + ihour
c
  100 continue
      return
      end
