      subroutine get_ev(c_env, c_value)

      implicit none

      character(len=*), intent(in)  :: c_env
      character(len=*), intent(out) :: c_value

      integer :: ierr

      c_value = "missing"

      call get_environment_variable(TRIM(c_env),value=c_value,status=ierr)


      if (ierr == 0) then
        write(*,*) "The value of environment variable ",TRIM(c_env)," is: ", TRIM(c_value)
      else if (ierr == 1) then
        write(*,*) "The environment variable ",TRIM(c_env)," does not exist."
      else if (ierr == -1) then
        write(*,*) "The value of environment variable ",TRIM(c_env)," is too long for the allocated string c_value."
      else
        print *, "An unknown error occurred while parsing environment variable ",TRIM(c_env)," Status code: ", ierr
      end if

      return

      end ! subroutine get_ev
