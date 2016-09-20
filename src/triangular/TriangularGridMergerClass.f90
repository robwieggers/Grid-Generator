module TriangularGridMergerClass
  use TypesMod
  use InternalTriangularGridExporterClass
  use ExternalTriangularGridExporterClass
  implicit none
  private

  type, public :: TriangularGridMerger
     type(InternalTriangularGridExporter) :: internalGridExporter
     type(ExternalTriangularGridExporter), allocatable, dimension(:) :: externalGridExporters
     character(charLen) :: filenameNode
     character(charLen) :: filenamePoly
     integer :: ioUnitNode
     integer :: ioUnitPoly
   contains
     procedure :: merge => mergeTriangularGrids
     procedure :: useFileWithoutExtension => setFilename
     procedure :: useIOUnits => setIOUnits
     procedure :: useInternalGridExporter => setInternalGridExporter
     procedure :: useExternalGridExporters => setExternalGridExporters
  end type TriangularGridMerger

  interface TriangularGridMerger
     module procedure newTriangularGridMerger
  end interface TriangularGridMerger

contains

  function newTriangularGridMerger()
    implicit none
    type(TriangularGridMerger) :: newTriangularGridMerger
    newTriangularGridMerger%ioUnitNode = -1
    newTriangularGridMerger%ioUnitPoly = -1
    newTriangularGridMerger%filenameNode = ''
    newTriangularGridMerger%filenamePoly = ''

  end function newTriangularGridMerger

  subroutine setExternalGridExporters(this, extGridExporters)
    use ExternalTriangularGridExporterClass
    implicit none
    class(TriangularGridMerger) :: this
    type (ExternalTriangularGridExporter), dimension(:) :: extGridExporters

    this%externalGridExporters = extGridExporters

  end subroutine setExternalGridExporters


  subroutine setInternalGridExporter(this, intGridExporter)
    use InternalTriangularGridExporterClass
    implicit none
    class(TriangularGridMerger) :: this
    type (InternalTriangularGridExporter) :: intGridExporter

    this%internalGridExporter = intGridExporter

  end subroutine setInternalGridExporter

  subroutine setFilename(this, fileWithoutExtension)
    use ErrorHandlingMod
    implicit none
    class(TriangularGridMerger) :: this
    character(charLen) :: fileWithoutExtension

    if (fileWithoutExtension == '') then
       call exception('file is empty', &
            __FILENAME__, __LINE__)
    end if
    this%filenameNode = fileWithoutExtension // '.node'
    this%filenamePoly = fileWithoutExtension // '.poly'
    
  end subroutine setFilename

  subroutine setIOUnits(this, ioUnitNode, ioUnitPoly)
    use ErrorHandlingMod
    implicit none
    class(TriangularGridMerger) :: this
    integer, intent(in) :: ioUnitNode, ioUnitPoly

    if (min(ioUnitNode, ioUnitPoly) < 0) then
       call exception('ioUnit should be positive', &
            __FILENAME__, __LINE__)
    end if
    if (ioUnitNode == ioUnitPoly) then
       call exception('ioUnits should have different values', &
            __FILENAME__, __LINE__)
    end if
    this%ioUnitNode = ioUnitNode
    this%ioUnitPoly = ioUnitPoly
    
  end subroutine setIOUnits


  subroutine mergeTriangularGrids(this)
    use ErrorHandlingMod
    implicit none
    class(TriangularGridMerger) :: this

    call exception('to be implemented', &
         __FILENAME__, __LINE__)
    
  end subroutine mergeTriangularGrids
  
end module TriangularGridMergerClass
