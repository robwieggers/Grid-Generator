module ExternalTriangularGridExporterClass
  use QuadrangularGridClass
  use ExporterClass
  use NeutralGridConfigurationClass
  implicit none
  private

  type, extends(Exporter), public :: ExternalTriangularGridExporter
     type(ExternalArea) :: externalArea
     type(QuadrangularGrid) :: quadrangularGrid
   contains
     
     procedure :: useExternalArea => setExternalArea
     procedure :: useQuadrangularGrid => setQuadrangularGrid
     procedure :: export => exportExternalTriangularGrid
  end type ExternalTriangularGridExporter
  
  interface ExternalTriangularGridExporter
     module procedure newExternalTriangularGridExporter
  end interface ExternalTriangularGridExporter

contains
  function newExternalTriangularGridExporter()
    implicit none
    type(ExternalTriangularGridExporter) :: newExternalTriangularGridExporter

    newExternalTriangularGridExporter%extension = '.poly'

  end function newExternalTriangularGridExporter


  subroutine setExternalArea(this, extArea)
    use NeutralGridConfigurationClass
    implicit none
    class(ExternalTriangularGridExporter) :: this
    type (ExternalArea), intent(in) :: extArea

    this%externalArea = extArea

  end subroutine setExternalArea

  subroutine setQuadrangularGrid(this, quadGrid)
    use QuadrangularGridClass
    implicit none
    class(ExternalTriangularGridExporter) :: this
    type(QuadrangularGrid), intent(in) :: quadGrid

    this%quadrangularGrid = quadGrid

  end subroutine setQuadrangularGrid

  subroutine exportExternalTriangularGrid(this)
    use TypesMod
    use ErrorHandlingMod
    use QuadrangularBoundaryClass
    implicit none
    class(ExternalTriangularGridExporter), intent(in) :: this
    integer :: iounit, additionalNodes, step, cntr, i
    integer :: head, tail
    character (charLen) :: angleLimitChar, areaLimitChar, iChar
    type(QuadrangularBoundary) :: boundary
    logical :: found = .false.
    
    head = this%externalArea%quadrangularNodeHead
    tail = this%externalArea%quadrangularNodeTail
    if (this%ioUnit < 0) then
       call exception('ioUnit has not been set', &
            __FILENAME__, __LINE__)
    end if
    iounit = this%ioUnit

    do i = 1, size(this%quadrangularGrid%boundary)
       if (this%quadrangularGrid%boundary(i)%description == &
            this%externalArea%quadrangularBoundary) then
          boundary = this%quadrangularGrid%boundary(i)
          found = .true.
          exit
       end if
    end do
    if (.not.found) then
       call exception ('boundary ' // trim(this%externalArea%quadrangularBoundary) // &
            ' used for an external region is not found in the quadrangular grid', &
            __FILENAME__, __LINE__)
    end if
    if (max(head, tail) > size(boundary%polyline, 1) .or. &
         min(head, tail) < 1) then
       call exception &
            ('index of head and tail nodes should be indices within the quadrangular boundary', &
               __FILENAME__, __LINE__)
    end if

    open(iounit, file=this%filename, status='UNKNOWN')

    additionalNodes = tail - head
    write (*, *) 'additionalNodes: ', additionalNodes, head, tail
    if (additionalNodes .lt. 0) then
       additionalNodes = -additionalNodes
       step = -1 
    else
       step = 1 
    end if
    additionalNodes = additionalNodes + 1 ! adding 1 to include first and last node.

    write (iounit, *) '# Set the nodes of this external grid'

    write (iounit, *) size(this%externalArea%nodes, 1) + additionalNodes, 2, 0, 0
    cntr = 0

    ! write nodes external region
    do i = 1, size(this%externalArea%nodes, 1)
       write (iounit, *) cntr, this%externalArea%nodes(i, 1), this%externalArea%nodes(i, 2)
       cntr = cntr + 1
    end do

    ! write nodes connecting to internal region
    write (*, *) head, tail, step
    do i = head, tail, step
       write (iounit, *) cntr, boundary%polyline(i, 1), boundary%polyline(i, 2)
       cntr = cntr + 1
    end do

    ! add blank line
    write (iounit, *) ''

    ! add the list of quadrangular cell sides
    ! write comments line first
    write (iounit, *) '# set the sides'
    write (iounit, *) '# format: counter, 2 nodes, wall index'

    ! write sides of external grid
    ! number of sides equal number of nodes (for closed polygon)
    write (iounit, *) size(this%externalArea%nodes, 1) + additionalNodes, 1
    cntr = 0
    do i = 1, size(this%externalArea%nodes, 1) + additionalNodes - 1
       write (iounit, *) cntr, i - 1, i, cntr + 1000
       cntr = cntr + 1
    end do
    ! close polygon (last node to first node)
    write (iounit, *) cntr, size(this%externalArea%nodes, 1) + additionalNodes - 1, 0, cntr + 1000

    ! no holes
    write (iounit, *) ''
    write (iounit, *) 0

    close(iounit)

    call system('${TOPDIR}/bin/triangle -zepn -a' // this%externalArea%areaLimit // &
         ' -q' // this%externalArea%angleLimit // ' ' // this%filename)
 
  end subroutine exportExternalTriangularGrid

end module ExternalTriangularGridExporterClass
