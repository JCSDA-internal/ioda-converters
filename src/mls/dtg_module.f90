module dtg_module

CONTAINS

      subroutine dtgmod(indtg,idif,newdtg,istat)
!.........START PROLOGUE.........................................
!
! SCCS IDENTIFICATION:   $HeadURL$
! SCCS IDENTIFICATION:   @(#)$Id$
!                        15:19:02 /h/cm/library/nognav/src/sub/fcst/dtgmod.f_v
!
!  SUBPROGRAM NAME:  DTGMOD
!
!  DESCRIPTION:  Given base DTG and increment (+/- hours), return new DTG
!                 ( = indtg + idif ) and the status value.
!
!  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
!
!  CURRENT PROGRAMMER (UTIL!):   S. Glassner
!
!  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
!
!  LIBRARIES OF RESIDENCE(UTIL!):
!
!  CLASSIFICATION:  UNCLAS
!
!  USAGE (CALLING SEQUENCE):
!
!    CHARACTER!10 INDTG, NEWDTG
!
!    CALL DTGMOD ( INDTG, IDIF, NEWDTG, ISTAT )
!
!  INPUT PARAMETERS:
!
!    INDTG	C!10	Base DTG in the format YYYYMMDDHH.
!    IDIF       INT     Difference in hours
!		         (-8775216 through +4294967295).
!
!  OUTPUT PARAMETERS:
!
!    NEWDTG	C!10	New DTG, the sum of INDTG and IDIF. If one of
!			  the errors listed below occurrs, NEWDTG will
!			  be returned asterisk-filled.
!    ISTAT      INT     Error return.
!			  ISTAT =  0, ok.
!			  ISTAT = -1, Invalid DTG.
!			  ISTAT = -2, Invalid increment.
!			  ISTAT = -3, DTGYRHR returned a non-zero
!				      status.
!
!  CALLS:
!
!    DTGNUM		Get integer year, month, day, hour, days of the
!			  year and hours of the year from DTG.
!    DTGYRHR		Get DTG from julian date.
!
!  EXAMPLE:
!
!    CHARACTER!10 CURDTG, NEWDTG
!    INTEGER IDIF
!
!    DATA CURDTG / '1991030612' /`
!    DATA IDIF  / 2 /
!
!    CALL DTGMOD ( CURDTG, IDIF, NEWDTG, ISTAT )
!		...
!
!    NEWDTG will contain 1991030614.
!
!  ERROR CONDITIONS:
!
!    Invalid DTG.   ISTAT=-1 at return. NEWDTG asterisk-filled.
!    Invalid IDIF.  ISTAT=-2 at return. NEWDTG asterisk-filled.
!    Non-zero status return from DTGYRHR. ISTAT=-3 at return.
!      NEWDTG asterisk-filled.
!
!.........MAINTENANCE SECTION......................................
!
!  PRINCIPAL VARIABLES AND ARRAYS:
!
!    BADDTG     C!10    Value returned in NEWDTG if an error occured.
!    IDX        INT     Set to 1 if the input year is a leap
!			  year. Otherwise, it will be 2, 3 or 4
!			  (the remainder resulting from the mod
!			  function). Used as a subscript for the
!			  IHOURS array.
!    MONTH(12,2) INT	Number of days elapsed before getting to
!          	          this month.  There are two sets
!			  of these, one for non-leap years (second
!			  subscript = 1), one for leap years (second
!			  subscript = 2).  For example, MONTH(3,2),
!			  for March in a leap year, contains 60, the
!			  the number of days elapsed before March.
!			  The idea is that if IDAYS comes up with
!			  64 days in a leap year, we know the month
!			  should be March because it is greater than
!			  60 and less than the end of March, 91.
!     IHOURS(4)	INT	Number of hours in the year.  IHOURS(1) contains
!			  the number of hours in a leap year.
!     IYR	INT	Year, extracted from INDTG by DTGNUM.
!     IMO	INT	Month, extracted from INDTG by DTGNUM.
!     IDAY	INT	Day, extracted from INDTG by DTGNUM.
!     IHR	INT	Hour, extracted from INDTG by DTGNUM.
!     IDAOFYR	INT	Day of the year, extracted from INDTG by DTGNUM.
!     IHROFYR	INT	Hour of year, extracted from INDTG by DTGNUM.
!     JSTAT	INT	Error return from DTGNUM. If=0, ok.
!     KSTAT	INT	Error return from DTGYRHR. If=0, ok.
!
!  METHOD
!
!     1.  Call DTGNUM to get hours of the year.
!     2.  Add IDIF to the hours of the year.
!     3.  If the new sum is negative or has too many hours for one year,
!         adjust the sum and input year until the sum is positive and within
!         one year.
!     4.  Call DTGYRHR, passing it the year and hours of the year, to get
!         the new DTG.
!
!  LANGUAGE (UTIL!):  FORTRAN 77
!
!  RECORD OF CHANGES:
!
! <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
!                   Change to DTGMOD required to port to Cray:
!                      from IMPLICIT UNDEFINED ( A-Z )
!                      to   IMPLICIT NONE
!
!.........END PROLOGUE..................................................
!
      implicit none
!
      character*10 indtg, newdtg, baddtg
!
      integer ihours(4), iyr, imo, iday, ihr, idaofyr, ihrofyr, newhrs, &
              idif, idx, i, istat, jstat, kstat
!
      data ihours/8784, 3*8760/
      data baddtg/'**********'/
!
      istat = 0
!
      call dtgnum(indtg,iyr,imo,iday,ihr,idaofyr,ihrofyr,jstat)
      if(jstat.ne.0)then
        istat = -1
        newdtg = baddtg
        go to 300
      endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        Increment (decrement) hour of the year (IHROFYR) by IDIF,
!        test for change of year, adjust if necessary, reformat to NEWDTG.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      newhrs = ihrofyr + idif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         See if NEWHRS is negative.  If it is,  perform a loop that
!		Subtracts 1 from the year,
!               Adds a year's worth of hours to NEWHRS, the number of hours
!		  depending on whether the year is a leap year or not.
!		Sees if NEWHRS is still negative.  Leave the loop when it
!		  becomes zero or positive.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      if(newhrs.lt.0)then
!
        do 50 i = 1, 1000
          iyr = iyr - 1
!
          if(mod(iyr,100).ne.0)then
            idx = mod(iyr,4) + 1
          elseif(mod(iyr,400).eq.0)then
            idx = 1
          else
            idx = 2
          endif
!
          newhrs = newhrs + ihours(idx)
          if(newhrs.ge.0)go to 200
!
   50   continue

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!         NEWHRS is positive or 0.
!         Perform a loop until NEWHRS value is less than or equal to the
!           number of hours in a year.
!              See if there is more than one years worth of hours in NEWHRS.
!              If there is, perform a loop that
!              Subtracts a years worth of hours from NEWHRS, the number
!                of hours depending on whether the year is a leap year or
!                  not.
!           Adds 1 to the year.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      else
        do 100 i = 1, 1000
          if(mod(iyr,100).ne.0)then
            idx = mod(iyr,4) + 1
          elseif(mod(iyr,400).eq.0)then
            idx = 1
          else
            idx = 2
          endif
!
          if(newhrs.lt.ihours(idx))go to 200
          newhrs = newhrs - ihours(idx)
          iyr = iyr + 1
!
  100   continue
      endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Error.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      newdtg = baddtg
      istat = -2
      go to 300

  200 continue
      call dtgyrhr(iyr,newhrs,newdtg,kstat)
      if(kstat.ne.0)then
        newdtg = baddtg
        istat = -3
!
      endif
!
  300 continue
      return
      end  subroutine dtgmod
!------------------------------------------------------------------------------!
      subroutine dtgnum(indtg,iyr,imo,iday,ihour,iyrday,iyrhrs,istat)

!.........START PROLOGUE.........................................
! SCCS IDENTIFICATION:   $HeadURL$
! SCCS IDENTIFICATION:   @(#)$Id$
!                        15:19:10 /h/cm/library/nognav/src/sub/fcst/dtgnum.f_v
!
!  SUBPROGRAM NAME:  DTGNUM
!
!  DESCRIPTION:  Given a DTG (YYYYMMDDHH), return integer values for
!		 year, month, day, hour, days into the year, and hours
!		 into the year.
!
!  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
!
!  CURRENT PROGRAMMER (UTIL!):   S. Glassner
!
!  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
!
!  LIBRARIES OF RESIDENCE(UTIL!):
!
!  CLASSIFICATION:  UNCLAS
!
!  USAGE (CALLING SEQUENCE):
!
!    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT )
!
!  INPUT PARAMETERS:
!
!    INDTG	C!10	    Day-time group, yyyymmddhh
!
!  OUTPUT PARAMETERS:
!
!    IYR	INT	    Year.
!    IMO	INT	    Month.
!    IDAY	INT         Day.
!    IHOUR	INT	    Hour.
!    IYRDAY	INT	    Days of the year.
!    IYRHRS	INT	    Hours of the year.
!    ISTAT	INT	    Error status return, = 0, OK; = -1,
!			     bad input DTG.
!
!  CALLS:
!
!    DTGCHK	A function that validates DTGs.  Output is a 10-character
!		  string that is blank if the DTG is valid.  It will
!		  contain asterisks corresponding to where the DTG is invalid
!		  otherwise.
!
!  ERRORS CONDITIONS:
!
!    Invalid DTG.  All integers will return with zero. ISTAT will be
!     set to -1.
!
!  EXAMPLE:
!
!    CHARACTER!10 INDTG
!
!    INTEGER IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS, ISTAT
!    DATA INDTG / '1990120312' /
!
!    CALL DTGNUM ( INDTG, IYR, IMO, IDAY, IHOUR, IYRDAY, IYRHRS,
!   !              ISTAT )
!
!    		...
!
!    Values returned will be:
!	IYR     - 1990
!	IMO     -   12
!	IDAY    -   03
!	IHOUR	-   12
!	IYRDAY  -  337
! 	IYRHRS  - 8076
!       ISTAT   -    0
!
!.........MAINTENANCE SECTION......................................
!
!  PRINCIPAL VARIABLES AND ARRAYS:
!
!    DTGERR	 C!10	Holds error return from DTGCHK.
!    IDX         INT    Set to 1 if the input year is not a leap
!			  year. If it is a leap year, it will be
!			  set to 2. Used as a subscript for the
!			  month array.
!    MONTH(12,2) INT	Number of days elapsed before getting to
!          	          this month.  There are two sets
!			  of these, one for non-leap years (second
!			  subscript = 1), one for leap years (second
!			  subscript = 2).  For example, MONTH(3,2),
!			  for March in a leap year, contains 60, the
!			  the number of days elapsed before March.
!			  The idea is that if IDAYS comes up with
!			  64 days in a leap year, we know the month
!			  should be March because it is greater than
!			  60 and less than the end of March, 91.
!
!  METHOD:
!
!    1.  Do an internal read of the input DTG to get year, month,
!	 day, hour.  Call DTGCHK to make sure it's valid first.
!	 If it is invalid, set ISTAT to 0 and return.
!    2.  Set leap year index, IDX.
!    3.  Calculate number of days in the year by adding IDAY to the
!	 number of days elapsed before the first of the month, IMO.
!	 Days elapsed is in the MONTH array.
!    4.  Calculate number of hours into the year by multiplying
!        whole days of the year (IYRDAY-1) by 24 hours and adding
!        IHOUR.
!
!  LANGUAGE (UTIL!):  FORTRAN 77
!
!  RECORD OF CHANGES:
!
! <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
!                   Changes to DTGNUM required to port to Cray:
!                      from IMPLICIT UNDEFINED ( A-Z )
!                      to   IMPLICIT NONE
!
!.........END PROLOGUE..................................................
!
!.........start prologue.........................................
!
!  subprogram name:  dtgnum
!
!  description:  given a dtg (yyyymmddhh), return integer values for
!		 year, month, day, hour, days into the year, and hours
!		 into the year.
!
!  original programmer, date:  s. glassner, 5 march 1991
!
!  current programmer (util!):   s. glassner
!
!  computer/operating system: sun/unix, cray/unicos
!
!  libraries of residence(util!):
!
!  classification:  unclas
!
!  usage (calling sequence):
!
!    call dtgnum ( indtg, iyr, imo, iday, ihour, iyrday, iyrhrs, istat )
!
!  input parameters:
!
!    indtg	c!10	    day-time group, yyyymmddhh
!
!  output parameters:
!
!    iyr	int	    year.
!    imo	int	    month.
!    iday	int         day.
!    ihour	int	    hour.
!    iyrday	int	    days of the year.
!    iyrhrs	int	    hours of the year.
!    istat	int	    error status return, = 0, ok; = -1,
!			     bad input dtg.
!
!  calls:
!
!    dtgchk	a function that validates dtgs.  output is a 10-character
!		  string that is blank if the dtg is valid.  it will
!		  contain asterisks corresponding to where the dtg is invalid
!		  otherwise.
!
!  errors conditions:
!
!    invalid dtg.  all integers will return with zero. istat will be
!     set to -1.
!
!  example:
!
!    character*10 indtg
!
!    integer iyr, imo, iday, ihour, iyrday, iyrhrs, istat
!    data indtg / '1990120312' /
!
!    call dtgnum ( indtg, iyr, imo, iday, ihour, iyrday, iyrhrs,
!   !              istat )
!
!    		...
!
!    values returned will be:
!	iyr     - 1990
!	imo     -   12
!	iday    -   03
!	ihour	-   12
!	iyrday  -  337
! 	iyrhrs  - 8076
!       istat   -    0
!
!.........maintenance section......................................
!
!  principal variables and arrays:
!
!    dtgerr	 c*10	holds error return from dtgchk.
!    idx         int    set to 1 if the input year is not a leap
!			  year. if it is a leap year, it will be
!			  set to 2. used as a subscript for the
!			  month array.
!    month(12,2) int	number of days elapsed before getting to
!          	          this month.  there are two sets
!			  of these, one for non-leap years (second
!			  subscript = 1), one for leap years (second
!			  subscript = 2).  for example, month(3,2),
!			  for march in a leap year, contains 60, the
!			  the number of days elapsed before march.
!			  the idea is that if idays comes up with
!			  64 days in a leap year, we know the month
!			  should be march because it is greater than
!			  60 and less than the end of march, 91.
!
!  method:
!
!    1.  do an internal read of the input dtg to get year, month,
!	 day, hour.  call dtgchk to make sure it's valid first.
!	 if it is invalid, set istat to 0 and return.
!    2.  set leap year index, idx.
!    3.  calculate number of days in the year by adding iday to the
!	 number of days elapsed before the first of the month, imo.
!	 days elapsed is in the month array.
!    4.  calculate number of hours into the year by multiplying
!        whole days of the year (iyrday-1) by 24 hours and adding
!        ihour.
!
!  language (util!):  fortran 77
!
!  record of changes:
!
! <<change notice>> version 1.0 (17 mar 1992) -- kunitani, c. (cri)
!                   changes to dtgnum required to port to cray:
!                      from implicit undefined ( a-z )
!                      to   implicit none
!
!.........end prologue..................................................

!
      implicit none

      character*10 indtg, dtgerr
!
      integer iyr, imo, iday, ihour, iyrday, iyrhrs, istat, idx
      integer month(12,2)
!
      data month/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, &
           0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/
!
      data dtgerr/'          '/
!
      iyr = 0
      imo = 0
      iday = 0
      ihour = 0
      iyrday = 0
      iyrhrs = 0
      istat = 0

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!	 call dtgchk to validate the dtg.
!	 if it's ok, do an internal read of the integer date parts.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      dtgerr = dtgchk(indtg)
      if(dtgerr.ne.'          ')then
        istat = -1
        go to 100
      else
        read(indtg,'(i4, 3i2)')iyr, imo, iday, ihour
      endif

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!        set the leap year index, idx, to 1 if non-leap year and 2
!	   if leap year.
!	 if year is a century, it's a leap year if it's evenly divisible
!	   by 400. if it's not a century, it's a leap year if it's
!	   evenly divisible by 4.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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
!
      iyrday = month(imo,idx) + iday
      iyrhrs = (iyrday-1)!24 + ihour
!
  100 continue
      return
      end subroutine dtgnum
!------------------------------------------------------------------------------!
      subroutine dtgyrhr(iyr,ihrs,newdtg,istat)

!.........START PROLOGUE.........................................
!
! SCCS IDENTIFICATION:   $HeadURL$
! SCCS IDENTIFICATION:   @(#)$Id$
!                        15:19:22 /h/cm/library/nognav/src/sub/fcst/dtgyrhr.f_v
!
!  SUBPROGRAM NAME:  DTGYRHR
!
!  DESCRIPTION:  Given a year and hours of the year, DTGYRHR returns
!                a DTG of format YYYYMMDDHH in NEWDTG.
!
!  ORIGINAL PROGRAMMER, DATE:  S. Glassner, 5 March 1991
!
!  CURRENT PROGRAMMER (UTIL*):  S. Glassner
!
!  COMPUTER/OPERATING SYSTEM: SUN/UNIX, CRAY/UNICOS
!
!  LIBRARIES OF RESIDENCE(UTIL*):
!
!  CLASSIFICATION:  UNCLAS
!
!  USAGE (CALLING SEQUENCE):  CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
!
!  INPUT PARAMETERS:
!
!    IYR	INT     4-digit year, e.g. 1995.
!    IHRS	INT	Hours into the year.
!
!  OUTPUT PARAMETERS:
!
!    NEWDTG	C*10	DTG of form YYYYMMDDHH, e.g. 1995041606.
!    ISTAT	INT     Status.
!			  =  0 - OK.
!			  = -1 - Invalid year.
!			  = -2 - Invalid hour-of-the-year value.
!
!  CALLED BY:  DTGMOD
!
!  CALLS:      None.
!
!  EXAMPLE:
!
!    CHARACTER NEWDTG*10
!    INTEGER IYR, IHRS, ISTAT
!
!    DATA IYR / 1991 /, IHRS / 674 /
!
!    CALL DTGYRHR ( IYR, IHRS, NEWDTG, ISTAT )
!    IF ( ISTAT .NE. 0 ) THEN
!	Error...
!
!    NEWDTG will contain 1991012902
!
!  ERROR CONDITIONS:
!
!    Negative year passed. Set NEWDTG to all asterisks, set ISTAT
!     to -1, and return.
!    Invalid hours value passed. Set NEWDTG to all asterisks,
!     set hours to max hours for that year, and set ISTAT to -2.
!
!.........MAINTENANCE SECTION......................................
!
!  PRINCIPAL VARIABLES AND ARRAYS:
!
!    BADDTG	  C*10  Returned in NEWDTG if an error occurred.
!    MONTH(12,2)  INT	Number of days elapsed before getting to
!          	          this month.  There are two sets
!			  of these, one for non-leap years (second
!			  subscript = 1), one for leap years (second
!			  subscript = 2).  For example, MONTH(3,2),
!			  for March in a leap year, contains 60, the
!			  the number of days elapsed before March.
!			  The idea is that if IDAYS comes up with
!			  64 days in a leap year, we know the month
!			  should be March because it is greater than
!			  60 and less than the end of March, 91.
!
!    IDAYS	  INT	Number, of hours (IHRS) converted into whole
!			  days.
!    IHOURS       INT   Number of hours into the next day.
!
!    IDX	  INT	Subscript for month.
!			     =1, non-leap year
!			     =2, leap year
!
!    IMO	  INT   IMO is initially set to 0.  It is incremented
!			  by 1 as the month loop runs.  When the correct
!			  month is found, IMO will be one less than I.
!			  If no correct month is found, IMO is set to
!			  12 and in either case becomes the month in the
!			  new DTG.
!
!    MAXHRS(2)	  INT	Maximum number of hours in one year.  Used for
!			  checking hours argument.
!
!  METHOD:
!
!    1.  Calculate the number of the day by dividing the hours by 24
!	 and adding 1 (IDAYS).
!    2.  Find what month the day is in by finding where the day
!	 falls in the month array (see description of MONTH, above).
!    3.  The month array has two sections, one for leap years and
!	 one for non-leap years.  Get the subscript for this dimension
!	 by taking mod 4 of the year.
!    4.  Calculate the days in the retrieved month by subtracting the
!        the number of days up to the month from the days of the year
!	 (IDAYS).
!    5.  Calculate the hours in the retrieved day by taking mod 24 of
!	 the hours input argument (IHRS).
!    6.  Do an internal write of the given year and derived month,
!	 day and hour into the output DTG.
!
!  LANGUAGE (UTIL*):  FORTRAN 77
!
!  RECORD OF CHANGES:
!
! <<CHANGE NOTICE>> version 1.0 (17 Mar 1992) -- Kunitani, C. (CRI)
!                   Changes to DTGYRHR required to port to Cray:
!                      from IMPLICIT UNDEFINED ( A-Z )
!                      to   IMPLICIT NONE
!
!.........END PROLOGUE..................................................
!
      implicit none
!
      character*10 newdtg, baddtg
!
      integer iyr, ihrs, istat, idx, mod, idays, imo, i, iday, ihours
      integer month(12,2), maxhrs(2)
!
      data month/0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, &
           0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335/
      data maxhrs/8759, 8783/
      data baddtg/'**********'/
!
      istat = 0

!***********************************************************************
!        Set the leap year index, IDX, to 1 if non-leap year and 2
!	   if leap year.
!	 If year is a century, it's a leap year if it's evenly divisible
!	   by 400. If it's not a century, it's a leap year if it's
!	   evenly divisible by 4.
!***********************************************************************

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

!**********************************************************************
!        Calculate number of whole days in IHRS.
!        Validate the hours argument.
!**********************************************************************

      if(iyr.lt.0)then
        istat = -1
        newdtg = baddtg
        go to 300
      endif
!
      if((ihrs.lt.0) .or. (ihrs.gt.maxhrs(idx)))then
        istat = -2
        ihrs = maxhrs(idx)
        newdtg = baddtg
      endif
!
      idays = ihrs/24 + 1

!**********************************************************************
!        Find the proper month by determining where this number of
!        days falls in the MONTH array.
!**********************************************************************

      imo = 0
      do 100 i = 2, 12
        imo = imo + 1
        if(idays.le.month(i,idx))go to 200
  100 continue
      imo = imo + 1
!
  200 continue
      ihours = mod(ihrs,24)
      iday = idays - month(imo,idx)
      write(newdtg,'(I4.4, 3I2.2)')iyr, imo, iday, ihours
!
  300 continue
      return
      end subroutine dtgyrhr
!-------------------------------------------------------------------------------
      function dtgchk(dtg)
!.............................START PROLOGUE............................
!
! SCCS IDENTIFICATION:   $HeadURL$
! SCCS IDENTIFICATION:   @(#)$Id$
!                        15:18:48 /h/cm/library/nognav/src/sub/fcst/dtgchk.f_v
!
! CONFIGURATION IDENTIFICATION:  None
!
! MODULE NAME:   dtgchk
!
! DESCRIPTION:
!
!        This function checks to see if a dtg is valid.
!
! CONTRACT NUMBER AND TITLE:  None
!
! REFERENCES: Programmers Thomas E. Rosmond (mostly),
!             Timothy F. Hogan, Ron Gelaro,  and/or Jim Ridout (NRL)
!
! CLASSIFICATION:  Unclassified
!
! RESTRICTIONS: None
!
! COMPILER DEPENDENCIES: FORTRAN 77, FORTRAN 90
!
! COMPILE OPTIONS:
! Fortran 90: -O2 -mips4 -64 -i4 -r8  -LANG:recursive=on -OPT:IEEE_arith=2:ro=2:SWP=on:Oli
!             -LNO:prefetch=2:prefetch_ahead=2
! Fortran 90: -O2 -mips4 -mp  -64 -i4 -r8 -extend_source -OPT:IEEE_arith=2:ro=2:SWP=on:Oli
!             -LNO: prefetch=2:prefetch_ahead=2
!
! LIBRARIES OF RESIDENCE: /a/ops/lib/librt.a
!
! USAGE: dtgerr = dtgchk(dtg)
!
! PARAMETERS:
!      Name            Type         Usage            Description
!   ----------      ----------     -------  ----------------------------
!
! COMMON BLOCKS:
!      Block      Name     Type    Usage              Notes
!     --------  --------   ----    ------   ------------------------
!
! FILES: None
!
! DATA BASES: None
!
! NON-FILE INPUT/OUTPUT: None
!
! ERROR CONDITIONS: None
!
! ADDITIONAL COMMENTS: None
!
!.................MAINTENANCE SECTION................................
!
! MODULES CALLED:
!         Name           Description
!        -------     ----------------------
! LOCAL VARIABLES AND
!          STRUCTURES:
! Name                  Type    Description
! -------------------   ------  -----------
!
! METHOD:
!
! See the latest edition of
!                         The NOGAPS Forecast Model:
!                         A Technical Description
!                         by T.F. Hogan, T.E. Rosmond, and R. Gelaro
!
! FILES: None
!
! INCLUDE FILES: None
!
! MAKEFILE:
!
! <<CHANGE NOTICE>> version 1.0 (12 Jan 1994) -- Pauley, R.
!   Initial installation under configuration management.
! <<CHANGE NOTICE>> (25 Nov 1997) --  Caudle, K., Hogan, T.
!   Standardization modifications.
!..............................END PROLOGUE.............................
!
!.........start prologue.........................................
!
!  subprogram name:  dtgchk
!
!  description:  check to see if a dtg is valid.
!
!  original programmer, date:  s. glassner, 5 march 1991
!
!  current programmer (util*):  s. glassner
!
!  computer/operating system: sun/unix, cray/unicos
!
!  libraries of residence(util*):
!
!  classification:  unclas
!
!  usage (calling sequence):
!
!    character*10 dtg, errdtg
!    errdtg = dtgchk ( dtg )
!            or
!    character*12 dtg, errdtg
!    errdtg = dtgchk ( dtg )
!
!  input parameters:
!
!    dtg           	  date-time group in one of two formats:
!		c*10        yyyymmddhh
!		c*12	    yyyymmddhhmm
!
!  output parameters:
!
!    errdtg	c*10/12   error designator.  all blank if dtg was valid.
!			    characters 1-4   = **** if year was bad.
!		            characters 5-6   = **   if month was bad.
!			    characters 7-8   = **   if day was bad.
!			    characters 9-10  = **   if hour was bad.
!			    characters 11-12 = **   if minutes were bad.
!
!  calls:
!
!    len	returns the length of a string.
!
!  example:
!
!    1)  character*10 dtgchk, dtg, errdtg
!
!	 data dtg / '1995070312' /
!
!        errdtg = dtgchk ( dtg )
!	 if ( errdtg .eq. ' ' ) then
!	    no error...
!	 else
!	    error...
!	 endif
!
!        errdtg will be blank.
!
!    2)  character*12 dtgchk, dtg, errdtg
!
!	 data dtg / '198607031245' /
!
!        errdtg = dtgchk ( dtg )
!	 if ( errdtg .eq. ' ' ) then
!	    no error...
!	 else
!	    error...
!	 endif
!
!        errdtg will be blank.
!
!    3)  character*12 dtgchk, dtg, errdtg
!
!	 data dtg / '198677031290' /
!
!        errdtg = dtgchk ( dtg )
!	 if ( errdtg .eq. ' ' ) then
!	    no error...
!	 else
!	    error...
!	 endif
!
!        errdtg will look like this, where a period denotes a blank:
!	   ....**....**	 meaning that the month and minutes are invalid.
!
!  restrictions:
!
!    the dtg may not contain any blanks.
!    the dtg must be either 10 or 12 digits long.
!    dtgchk rejects dtg years that are outside the range 1800-2799.
!
!  error conditions:
!
!    non-numeric dtg. dtgchk will be asterisk-filled.
!
!.........maintenance section......................................
!
!  principal variables and arrays:
!
!    iday	int	integer day from the input dtg.
!    ihour	int	integer hour from the input dtg.
!    imo	int	integer month from the input dtg.
!    iyr	int	integer year from the input dtg.
!    length	int	gets the return from len, i.e. the length
!			  of the argument string.
!    min	int	integer minutes from the input dtg.
!    month(12)	int	an array containing the number of days in
!			  each month.
!
!  method:
!
!    check that each value is numeric and within the ranges:
!	year:  1800-2799.
!	month: 1-12.
!	day:   1-31 for jan, mar, may, july, aug, oct and dec.
!	       1-30 for apr, june, sep, nov.
!	       28 or 29 for feb, depending on whether year is a leap year.
!		  note that 1900 is not a leap year and 2000 is.
!	hour:  0-23
!	min:   0-59
!    whenever a value is outside its range, set the corresponding characters
!	of dtgchk to asterisks.
!
!  language (util*):  fortran 77
!
!  record of changes:
!
! <<change notice>> version 1.0 (17 mar 1992) -- kunitani, c. (cri)
!                   change to dtgchk required to port to cray:
!                      from implicit undefined ( a-z )
!                      to   implicit none
!
!.........end prologue..................................................
!
      implicit none
!
      character(len=12) dtgchk
      character(len=*), intent (in) :: dtg
!
      integer month(12), iyr, imo, iday, ihour, min, length, i
!
      data month/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/
!
      length = len(dtg)
      if(length.ne.10 .and. length.ne.12)then
        dtgchk = '************'
        go to 200
      endif

!***************************************************************************
!        make sure every character in the dtg is a number.
!***************************************************************************

      do 100 i = 1, length
        if(dtg(i:i).lt.'0' .or. dtg(i:i).gt.'9')then
!
          if(length.eq.10)then
            dtgchk = '**********'
          else
            dtgchk = '************'
          endif
!
          go to 200
        endif
  100 continue

!***************************************************************************
!        month(2) will be changed to 29 later if the year is a leap year.
!***************************************************************************

      month(2) = 28

!***************************************************************************
!        get the integer versions of the year, month, day, and hour.
!***************************************************************************

      read(dtg,'(i4,3i2)')iyr, imo, iday, ihour

!***************************************************************************
!       get minutes if they're there.
!***************************************************************************

      dtgchk(1:length) = ' '
!
      if(length.eq.12)then
        if((dtg(11:11).ge.'0') .and. (dtg(11:11).le.'9') .and. &
           (dtg(12:12).ge.'0') .and. (dtg(12:12).le.'9'))then
          read(dtg,'(10x, i2)')min
        else
          min = 99
        endif
      endif

!***************************************************************************
!        check out the dtg.
!          year...
!***************************************************************************

      if((iyr.lt.1800) .or. (iyr.gt.2799))dtgchk(1:4) = '****'

!***************************************************************************
!          month...
!***************************************************************************

      if((imo.lt.1) .or. (imo.gt.12))dtgchk(5:6) = '**'

!***************************************************************************
!          day...
!	     set february days to 29 if this is a leap year.
!
!	     if dtgchk(1:6) (year/month) already has asterisks, it was
!              invalid, so don't do the day check.  if the year was invalid,
!	       we can't see if it was a leap year.  if the month was
!	       invalid, we can't see if it was february.
!	     see if this is a leap year, i.e. evenly divisible by four or,
!	       if it's a century (dtg(3:4) = '00'), evenly divisible
!	       by 400.
!***************************************************************************

      if(dtgchk(1:6).eq.'  ')then
        if((dtg(3:4).ne.'00') .and. (mod(iyr,4).eq.0) .or. &
           ((dtg(3:4).eq.'00').and.(mod(iyr,400).eq.0)))month(2) = 29
!
        if((iday.lt.1) .or. (iday.gt.month(imo)))dtgchk(7:8) = '**'
      endif

!***************************************************************************
!          hour...
!***************************************************************************

      if((ihour.lt.0) .or. (ihour.gt.23))dtgchk(9:10) = '**'

!***************************************************************************
!	   minutes...
!***************************************************************************

      if(length.eq.12)then
        if((min.lt.0) .or. (min.gt.59))dtgchk(11:12) = '**'
      endif
!
  200 continue
      return
      end function dtgchk
!-------------------------------------------------------------------------------
End module dtg_module
