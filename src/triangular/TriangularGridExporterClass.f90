module TriangularGridExporterClass
  use TypesMod
  implicit none
  private
  
  type, public :: TriangularGridExporter
     character(charLen) :: filename
     integer :: ioUnit
   contains
     procedure :: useFile => setTriangularGridExporterFileName
     procedure :: useIOUnit => setTriangularGridExporterIOUnit
  end type TriangularGridExporter

  interface TriangularGridExporter
     module procedure newTriangularGridExporter
  end interface TriangularGridExporter

contains
  function newTriangularGridExporter()
    implicit none

    type(TriangularGridExporter) :: newTriangularGridExporter
    newTriangularGridExporter%filename = ''

  end function newTriangularGridExporter
  
  subroutine setTriangularGridExporterIOUnit(this, ioUnit)
  use ErrorHandlingMod
    implicit none
    class(TriangularGridExporter) :: this
    integer, intent(in) :: ioUnit
    logical :: ex

    if (ioUnit < 0) then
       call exception("unit for IO should be positive", &
            __FILENAME__, __LINE__)
    end if
    this%ioUnit = ioUnit
    
  end subroutine setTriangularGridExporterIOUnit
  
  
  subroutine setTriangularGridExporterFilename(this, filename)
    use ErrorHandlingMod
    implicit none
    class(TriangularGridExporter) :: this
    character*(*), intent(in) :: filename
    logical :: ex

    if (trim(filename) == "") then
       call exception("triangular grid filename is empty string", &
         __FILENAME__, __LINE__)
    end if
    if (len(trim(adjustl(filename))) > charLen) then
       call exception("triangular grid filename is too long", &
         __FILENAME__, __LINE__)
    end if

    if (index(filename, '.poly', .true.) == 0) then
       call exception("triangular grid filename should have extension '.poly'", &
         __FILENAME__, __LINE__)
    end if
    
    inquire(file = trim(adjustl(filename)), exist = ex)
    if (ex) then
       call warning("triangular grid file " // trim(adjustl(filename)) // " does already exist", &
         __FILENAME__, __LINE__)
    end if
    this%filename = trim(adjustl(filename))
    
  end subroutine setTriangularGridExporterFilename
  
end module TriangularGridExporterClass
