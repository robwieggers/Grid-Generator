module ExporterClass
  use TypesMod
  implicit none
  private
  
  type, public :: Exporter
     character(charLen) :: filename
     integer :: ioUnit
     character(charLen) :: extension
   contains
     procedure :: useFile => setFileName
     procedure :: useIOUnit => setIOUnit
  end type Exporter

  interface Exporter
     module procedure newExporter
  end interface Exporter

contains
  function newExporter()
    implicit none

    type(Exporter) :: newExporter
    newExporter%filename = ''
    newExporter%extension = ''
    newExporter%ioUnit = -1

  end function newExporter
  
  subroutine setIOUnit(this, ioUnit)
  use ErrorHandlingMod
    implicit none
    class(Exporter) :: this
    integer, intent(in) :: ioUnit
    logical :: ex

    if (ioUnit < 0) then
       call exception("unit for IO should be positive", &
            __FILENAME__, __LINE__)
    end if
    this%ioUnit = ioUnit
    
  end subroutine setIOUnit
  
  
  subroutine setFilename(this, filename)
    use ErrorHandlingMod
    implicit none
    class(Exporter) :: this
    character*(*), intent(in) :: filename
    logical :: ex

    if (trim(filename) == "") then
       call exception("filename is empty string", &
         __FILENAME__, __LINE__)
    end if
    if (len(trim(adjustl(filename))) > charLen) then
       call exception("filename " // trim(adjustl(filename)) //  " is too long", &
         __FILENAME__, __LINE__)
    end if

    if (index(filename, trim(this%extension), .true.) == 0) then
       call exception("filename " // trim(adjustl(filename)) // &
            " should have extension " // trim(this%extension), &
            __FILENAME__, __LINE__)
    end if
    
    inquire(file = trim(adjustl(filename)), exist = ex)
    if (ex) then
       call warning("file " // trim(adjustl(filename)) // " does already exist", &
         __FILENAME__, __LINE__)
    end if
    this%filename = trim(adjustl(filename))
    
  end subroutine setFilename
  
end module ExporterClass
