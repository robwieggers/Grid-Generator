module ErrorHandlingMod
  implicit none
  private
  public exception

contains

  subroutine exception(message, file, line)
    implicit none
    character*(*), intent(in) :: message, file
    character(15) :: lineNumber
    integer, intent(in) :: line
    write(unit=lineNumber,fmt=*) line 

    print *, message
    print *, '        at line ' // &
         trim(adjustl(lineNumber)) // &
         ' of ' // file
    stop '    Execution will be stopped'
    
  end subroutine exception

  subroutine warning(message, file, line)
    implicit none
    character*(*), intent(in) :: message, file
    character(15) :: lineNumber
    integer, intent(in) :: line
    write(unit=lineNumber,fmt=*) line 

    print *, message
    print *, '        at line ' // &
         trim(adjustl(lineNumber)) // &
         ' of ' // file
    
  end subroutine warning

end module ErrorHandlingMod
