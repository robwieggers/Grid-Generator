module TriangularGridExporterClass
  use TypesMod
  implicit none

  type, public :: TriangularGridExporter
     character(charLen) :: filename
     integer :: unitIO
   contains
     procedure :: useFile => setTriangularGridExporterFileName
     procedure :: useUnitIO => setTriangularGridExporterUnitIO
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

  subroutine setTriangularGridExporterUnitIO(this, unitIO)
  use ErrorHandlingMod
    implicit none
    class(TriangularGridExporter) :: this
    integer, intent(in) :: unitIO
    logical :: ex

    if (unitIO < 0) then
       call exception("unit for IO should be positive", &
            __FILE__, __LINE__)
    end if
    this%unitIO = unitIO
    
  end subroutine setTriangularGridExporterUnitIO
  
  
  subroutine setTriangularGridExporterFilename(this, filename)
    use ErrorHandlingMod
    implicit none
    class(TriangularGridExporter) :: this
    character*(*), intent(in) :: filename
    logical :: ex

    if (trim(filename) == "") then
       call exception("triangular grid filename is empty string", &
         __FILE__, __LINE__)
    end if
    if (len(trim(adjustl(filename))) > charLen) then
       call exception("triangular grid filename is too long", &
         __FILE__, __LINE__)
    end if

    if (index(filename, '.poly', .true.) == 0) then
       call exception("triangular grid filename should have extension '.poly'", &
         __FILE__, __LINE__)
    end if
    
    inquire(file = trim(adjustl(filename)), exist = ex)
    if (ex) then
       call warning("triangular grid file " // trim(adjustl(filename)) // " does already exist", &
         __FILE__, __LINE__)
    end if
    this%filename = trim(adjustl(filename))
    
  end subroutine setTriangularGridExporterFilename
  
end module TriangularGridExporterClass
