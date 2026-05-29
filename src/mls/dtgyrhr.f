      subroutine dtgyrhr(iyr,ihrs,newdtg,istat)
C.........START PROLOGUE.........................................
C
C SCCS IDENTIFICATION:   $HeadURL$
C SCCS IDENTIFICATION:   @(#)$Id$
C                        15:19:22 /h/cm/library/nognav/src/sub/fcst/dtgyrhr.f_v
C
C  SUBPROGRAM NAME:  DTGYRHR
C
C  DESCRIPTION:  Given a year and hours of the year, DTGYRHR returns
C                a DTG of format YYYYMMDDHH in NEWDTG.
C
C  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
C
C  CURRENT PROGRAMMER (UTIL*):  S. Glassner
C
C  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
C
C  LIBRARIES OF RESIDENCE(UTIL*):
C
C  CLASSIFICATION:  UNCLAS
C
C  USAGE (CALLING SEQUENCE):  CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C
C  INPUT PARAMETERS:
C
C    IYR	INT     4-digit year, e.g. 1995.
C    IHRS	INT	Hours into the year.
C
C  OUTPUT PARAMETERS:
C
C    NEWDTG	C*10	DTG of form YYYYMMDDHH, e.g. 1995041606.
C    ISTAT	INT     Status.
C			  =  0 - OK.
C			  = -1 - Invalid year.
C			  = -2 - Invalid hour-of-the-year value.
C
C  CALLED BY:  DTGMOD
C
C  CALLS:      None.
C
C  EXAMPLE:
C
C    CHARACTER NEWDTG*10
C    INTEGER IYR, IHRS, ISTAT
C
C    DATA IYR / 1991 /, IHRS / 674 /
C
C    CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
C    IF ( ISTAT .NE. 0 ) THEN
C	Error...
C
C    NEWDTG will contain 1991012902
C
C  ERROR CONDITIONS:
C
C    Negative year passed. Set NEWDTG to all asterisks, set ISTAT
C     to -1, and return.
C    Invalid hours value passed. Set NEWDTG to all asterisks,
C     set hours to max hours for that year, and set ISTAT to -2.
C
C.........MAINTENANCE SECTION......................................
C
C  PRINCIPAL VARIABLES AND ARRAYS:
C
C    BADDTG	  C*10  Returned in NEWDTG if an error occurred.
C    MONTH(12,2)  INT	Number of days elapsed before getting to
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
C    IDAYS	  INT	Number, of hours (IHRS) converted into whole
C			  days.
C    IHOURS       INT   Number of hours into the next day.
C
C    IDX	  INT	Subscript for month.
C			     =1, non-leap year
C			     =2, leap year
C
C    IMO	  INT   IMO is initially set to 0.  It is incremented
C			  by 1 as the month loop runs.  When the correct
C			  month is found, IMO will be one less than I.
C			  If no correct month is found, IMO is set to
C			  12 and in either case becomes the month in the
C			  new DTG.
C
C    MAXHRS(2)	  INT	Maximum number of hours in one year.  Used for
C			  checking hours argument.
C
C  METHOD:
C
C    1.  Calculate the number of the day by dividing the hours by 24
C	 and adding 1 (IDAYS).
C    2.  Find what month the day is in by finding where the day
C	 falls in the month array (see description of MONTH, above).
C    3.  The month array has two sections, one for leap years and
C	 one for non-leap years.  Get the subscript for this dimension
C	 by taking mod 4 of the year.
C    4.  Calculate the days in the retrieved month by subtracting the
C        the number of days up to the month from the days of the year
C	 (IDAYS).
C    5.  Calculate the hours in the retrieved day by taking mod 24 of
C	 the hours input argument (IHRS).
C    6.  Do an internal write of the given year and derived month,
C	 day and hour into the output DTG.
C
C  LANGUAGE (UTIL*):  FORTRAN 77
C
C  RECORD OF CHANGES:
C
C <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
C                   Changes to DTGYRHR required to port to Cray:
C                      from IMPLICIT UNDEFINED ( A-Z )
C                      to   IMPLICIT NONE
C
C.........END PROLOGUE..................................................
C
      implicit none
C
      character*10 newdtg, baddtg
C
      integer iyr, ihrs, istat, idx, mod, idays, imo, i, iday, ihours
      integer month(12,2), maxhrs(2)
C
      data month/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334,
     &     0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/
      data maxhrs/8759, 8783/
      data baddtg/'**********'/
C
      istat = 0

************************************************************************
*        Set the leap year index, IDX, to 1 if non-leap year and 2
*	   if leap year.
*	 If year is a century, it's a leap year if it's evenly divisible
*	   by 400. If it's not a century, it's a leap year if it's
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

***********************************************************************
*        Calculate number of whole days in IHRS.
*        Validate the hours argument.
***********************************************************************

      if(iyr.lt.0)then
        istat = -1
        newdtg = baddtg
        go to 300
      endif
C
      if((ihrs.lt.0) .or. (ihrs.gt.maxhrs(idx)))then
        istat = -2
        ihrs = maxhrs(idx)
        newdtg = baddtg
      endif
C
      idays = ihrs/24 + 1

***********************************************************************
*        Find the proper month by determining where this number of
*        days falls in the MONTH array.
***********************************************************************

      imo = 0
      do 100 i = 2, 12
        imo = imo + 1
        if(idays.le.month(i,idx))go to 200
  100 continue
      imo = imo + 1
C
  200 continue
      ihours = mod(ihrs,24)
      iday = idays - month(imo,idx)
      write(newdtg,'(I4.4, 3I2.2)')iyr, imo, iday, ihours
C
  300 continue
      return
      end
