!------------------------------------------------------------------------------!
      subroutine slen (cstr,lenc)
    
      implicit none
    
      character(len=*), intent(in) :: cstr
      integer, intent(out) :: lenc

      character(len=1) tab, carriage_return, linefeed
      integer maxlen, i
       
      tab=char(9)
      linefeed=char(10)
      carriage_return=char(13)
         
!  Get the size of character string

      maxlen=len(cstr)
    
      lenc= 0
      do 10 i=1,maxlen
        if ( (cstr(i:i).eq.' ') .or. (cstr(i:i).eq.tab) .or. &
        (cstr(i:i).eq.carriage_return) .or. (cstr(i:i).eq.linefeed) ) &
        return

        lenc= i

 10   continue

      return
      end subroutine slen ! subroutine slen
!------------------------------------------------------------------------------!
