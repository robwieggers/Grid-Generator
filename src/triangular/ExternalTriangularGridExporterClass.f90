module ExternalTriangularGridExporterClass
  use, intrinsic :: iso_fortran_env
  use TriangularGridExporterClass
  use NeutralGridConfigurationClass
  implicit none
  private

  type, extends(TriangularGridExporter), public :: ExternalTriangularGridExporter
     type(ExternalArea) :: externalArea
   contains
     
     procedure :: useExternalArea => setExternalArea
     procedure :: export => exportExternalTriangularGrid
  end type ExternalTriangularGridExporter
  
  interface ExternalTriangularGridExporter
     module procedure newExternalTriangularGridExporter
  end interface ExternalTriangularGridExporter

contains
  function newExternalTriangularGridExporter()
  use TriangularGridExporterClass
    implicit none
    type(ExternalTriangularGridExporter) :: newExternalTriangularGridExporter

    newExternalTriangularGridExporter%filename = ''

  end function newExternalTriangularGridExporter


  subroutine setExternalArea(this, extArea)
    use ErrorHandlingMod
    use NeutralGridConfigurationClass
    implicit none
    class(ExternalTriangularGridExporter) :: this
    type(ExternalArea), intent(in) :: extArea
    
    this%externalArea = extArea

  end subroutine setExternalArea

  
  subroutine exportExternalTriangularGrid(this)
    use ErrorHandlingMod
    implicit none
    class(ExternalTriangularGridExporter) :: this
    integer :: ix, iy, cntr, nx, ny, iounit

    iounit = this%ioUnit

    open(unit=iounit, file=this%filename, status='UNKNOWN')

!    write (iounit, *) '# Set the nodes of the external grid'
!    write (iounit, *) (nx + 1) * (ny + 1), 2, 0, 0

  end subroutine exportExternalTriangularGrid
  
end module ExternalTriangularGridExporterClass
