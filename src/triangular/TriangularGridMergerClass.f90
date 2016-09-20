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
    type (ExternalTriangularGridExporter), dimension(:), intent(in) :: extGridExporters

    this%externalGridExporters = extGridExporters

  end subroutine setExternalGridExporters


  subroutine setInternalGridExporter(this, intGridExporter)
    use InternalTriangularGridExporterClass
    implicit none
    class(TriangularGridMerger) :: this
    type (InternalTriangularGridExporter), intent(in) :: intGridExporter

    this%internalGridExporter = intGridExporter

  end subroutine setInternalGridExporter

  subroutine setFilename(this, fileWithoutExtension)
    use ErrorHandlingMod
    implicit none
    class(TriangularGridMerger) :: this
    character(charLen), intent(in) :: fileWithoutExtension

    if (fileWithoutExtension == '') then
       call exception('file is empty', &
            __FILENAME__, __LINE__)
    end if
    this%filenameNode = trim(fileWithoutExtension) // ".node"
    this%filenamePoly = trim(fileWithoutExtension) // ".poly"
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
    implicit none
    class(TriangularGridMerger), intent(in) :: this
    integer, allocatable, dimension(:) :: nrNodes, nrSides
    
    nrNodes = extractNrNodes(this)
    nrSides = extractNrSides(this)
    
    call mergeNodes(this, nrNodes)
    call mergePolygons(this, nrNodes, nrSides)

    call system('${TOPDIR}/bin/triangle -zepnjQ ' // this%filenamePoly)

    
  end subroutine mergeTriangularGrids

  
  function extractNrNodes(this) result(nrNodes)
    class(TriangularGridMerger), intent(in) :: this
    integer, allocatable, dimension(:) :: nrNodes
    integer :: i, indexDot
    integer, dimension(3) :: intArray
    character(charLen) :: filename

    allocate(nrNodes(0:size(this%externalGridExporters)))
    
    ! determine total number of nodes
    indexDot = index(this%internalGridExporter%filename, '.', .true.)
    filename = this%internalGridExporter%filename(:indexDot - 1) // '.1.node'
    open(unit = this%internalGridExporter%ioUnit, file = filename, status='UNKNOWN')
    read (this%internalGridExporter%ioUnit, *) nrNodes(0), intArray

    do i = 1, size(this%externalGridExporters)
       indexDot = index(this%externalGridExporters(i)%filename, '.', .true.)
       filename = this%externalGridExporters(i)%filename(:indexDot - 1) // '.1.node'
       open(unit = this%externalGridExporters(i)%ioUnit, file = filename, status='UNKNOWN')
       read (this%externalGridExporters(i)%ioUnit, *) nrNodes(i), intArray
    end do

  end function extractNrNodes


  function extractNrSides(this) result(nrSides)
    class(TriangularGridMerger), intent(in) :: this
    integer, allocatable, dimension(:) :: nrSides
    integer :: i, indexDot, tmp
    character(charLen) :: filename, line

    allocate(nrSides(0:size(this%externalGridExporters)))

    ! determine total number of sides
    indexDot = index(this%internalGridExporter%filename, '.', .true.)
    filename = this%internalGridExporter%filename(:indexDot - 1) // '.1.poly'
    open(unit = this%internalGridExporter%ioUnit, file = filename, status='UNKNOWN')
    ! skip 1 line
    read (this%internalGridExporter%ioUnit, *) line
    read (this%internalGridExporter%ioUnit, *) nrSides(0), tmp
    close (this%internalGridExporter%ioUnit)
    
    do i = 1, size(this%externalGridExporters)
       indexDot = index(this%externalGridExporters(i)%filename, '.', .true.)
       filename = this%externalGridExporters(i)%filename(:indexDot - 1) // '.1.poly'
       open(unit = this%externalGridExporters(i)%ioUnit, file = filename, status='UNKNOWN')
       ! skip 1 line
       read (this%externalGridExporters(i)%ioUnit, *) line
       read (this%externalGridExporters(i)%ioUnit, *) nrSides(i), tmp
       close (this%externalGridExporters(i)%ioUnit)
    end do


  end function extractNrSides


  subroutine mergeNodes(this, nrNodesIn)
    use, intrinsic :: iso_fortran_env
    implicit none
    class(TriangularGridMerger), intent(in) :: this
    integer, allocatable, dimension(:), intent(in) :: nrNodesIn
    integer, allocatable, dimension(:) :: nrNodes
    integer :: i, j, cntr, indexDot, ioUnit, wallIndex
    integer, dimension(3) :: intArray
    real(real64), dimension(2) :: realArray
    character(charLen) :: filename, line

    allocate(nrNodes(0:size(nrNodesIn) - 1))
    nrNodes = nrNodesIn

    ioUnit = this%ioUnitNode
    wallIndex = 1000
    cntr = 0
    open(unit = ioUnit, file = this%filenameNode, status='UNKNOWN')

    ! write nodes
    write (ioUnit, *) '# set the nodes of the merged grid'
    write (ioUnit, *) sum(nrNodes), 2, 0, 1

    ! nodes of internal region
    indexDot = index(this%internalGridExporter%filename, '.', .true.)
    filename = this%internalGridExporter%filename(:indexDot - 1) // '.1.node'
    open(unit = this%internalGridExporter%ioUnit, file = filename, status='UNKNOWN')
    ! skip line with nrNodes info
    read (this%internalGridExporter%ioUnit, *) line
    write (ioUnit, *) '# from ', trim(filename)
    do j = 1, nrNodes(0)
       read (this%internalGridExporter%ioUnit, *) intArray(1), realArray, intArray(2)
       write (ioUnit, *) cntr, realArray, wallIndex
       cntr = cntr + 1
       wallIndex = wallIndex + 1
    end do
    close(this%internalGridExporter%ioUnit)

    ! nodes of external regions
    do i = 1, size(this%externalGridExporters)
       indexDot = index(this%externalGridExporters(i)%filename, '.', .true.)
       filename = this%externalGridExporters(i)%filename(:indexDot - 1) // '.1.node'
       open(unit = this%externalGridExporters(i)%ioUnit, file = filename, status='UNKNOWN')
       ! skip line with nrNodes info
       read (this%externalGridExporters(i)%ioUnit, *) line
       write (ioUnit, *) '# from ', trim(filename)
       do j = 1, nrNodes(i)
          read (this%externalGridExporters(i)%ioUnit, *) intArray(1), realArray, intArray(2)
          write (ioUnit, *) cntr, realArray, wallIndex
          cntr = cntr + 1
          wallIndex = wallIndex + 1
       end do
       close(this%externalGridExporters(i)%ioUnit)
    end do

    close(this%ioUnitNode)
    
  end subroutine mergeNodes

  
  subroutine mergePolygons(this, nrNodesIn, nrSidesIn)
    use, intrinsic :: iso_fortran_env
    implicit none
    class(TriangularGridMerger) :: this
    integer :: i, j, cntr, indexDot, ioUnit, wallIndex, nrHoles
    integer, dimension(4) :: intArray
    real(real64), dimension(2) :: realArray
    integer, allocatable, dimension(:), intent(in) :: nrNodesIn, nrSidesIn
    integer, allocatable, dimension(:) :: nrSides, nrNodes
    character(charLen) :: filename, line

    allocate(nrSides(0:size(nrSidesIn) - 1))
    nrSides = nrSidesIn
    allocate(nrNodes(0:size(nrNodesIn) - 1))
    nrNodes = nrNodesIn
    
    ioUnit = this%ioUnitNode
    cntr = 0
    wallIndex = 1000

    open(unit = ioUnit, file = this%filenamePoly, status='UNKNOWN')
   
    indexDot = index(this%internalGridExporter%filename, '.', .true.)
    filename = this%internalGridExporter%filename(:indexDot - 1) // '.1.poly'
    open(unit = this%internalGridExporter%ioUnit, file = filename, status='UNKNOWN')

    ! skip the two header lines
    read (this%internalGridExporter%ioUnit, *) intArray
    read (this%internalGridExporter%ioUnit, *) line

    write (ioUnit, *) '# set the sides of the merged grid'

    write (ioUnit, *) intArray
    write (ioUnit, *) sum(nrSides), 1
    write (ioUnit, *) '# format: counter, 2 nodes, wall index'
    
    write (ioUnit, *) '# from ', trim(filename)
    
    do j = 1, nrSides(0)
       read (this%internalGridExporter%ioUnit, *) intArray
       write (ioUnit, *) cntr, intArray(2:3), wallIndex
       cntr = cntr + 1
       wallIndex = wallIndex + 1
    end do
    
    do i = 1, size(this%externalGridExporters)
       indexDot = index(this%externalGridExporters(i)%filename, '.', .true.)
       filename = this%externalGridExporters(i)%filename(:indexDot - 1) // '.1.poly'
       open(unit = this%externalGridExporters(i)%ioUnit, file = filename, status='UNKNOWN')
       ! skip the two header lines
       read (this%externalGridExporters(i)%ioUnit, *) line
       read (this%externalGridExporters(i)%ioUnit, *) line
       write (ioUnit, *) '# from ', trim(filename)
       do j = 1, nrSides(i)
          read (this%externalGridExporters(i)%ioUnit, *) intArray
          write (ioUnit, *) cntr, intArray(2:3) + sum(nrNodes(0:i-1)), wallIndex
          cntr = cntr + 1
          wallIndex = wallIndex + 1           
       end do
       close(this%externalGridExporters(i)%ioUnit)
    end do

    write (ioUnit, *) ' #set the hole (for tokamak geometry)'
      write (ioUnit, *) ' #format: counter, coordinate within the hole'
      read (this%internalGridExporter%ioUnit, *) nrHoles
      write (ioUnit, *) nrHoles
      do i = 1, nrHoles
         read (this%internalGridExporter%ioUnit, *) cntr, realArray(1:2)
         write (ioUnit, *) cntr, realArray(1:2)
      end do
      
      close(this%internalGridExporter%ioUnit)
      close(ioUnit)
    
  end subroutine mergePolygons

end module TriangularGridMergerClass
