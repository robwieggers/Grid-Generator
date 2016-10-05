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
    implicit none
    class(ExternalTriangularGridExporter), intent(in) :: this
    integer :: iounit, additionalNodes, step, cntr, i
    integer, dimension(2) :: head, tail
    character (charLen) :: angleLimitChar, areaLimitChar, iChar
    
    head = this%externalArea%quadrangularNodeHead
    tail = this%externalArea%quadrangularNodeTail
    if (this%ioUnit < 0) then
       call exception('ioUnit has not been set', &
            __FILENAME__, __LINE__)
    end if
    iounit = this%ioUnit
    do i = 1, 2
       write (iChar, *) i
       if (min(head(i), tail(i)) < lbound(this%quadrangularGrid%grid, i) .or. &
         max(head(i), tail(i)) > ubound(this%quadrangularGrid%grid, i)) then
          call exception ('coordinate ' // trim(adjustl(iChar)) // &
               ' of head and tail nodes should be indices within the quadrangular grid', &
               __FILENAME__, __LINE__)
       end if
    end do

    open(iounit, file=this%filename, status='UNKNOWN')

    additionalNodes = tail(1) + tail(2) - head(1) - head(2)
    if (additionalNodes .lt. 0) then
       additionalNodes = -additionalNodes
       step = 1 ! index of last < first, so loop from last node to first node is in positive direction
    else
       step = -1 ! index of last > first, so loop from last node to first node is in negative direction
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
    if (head(1) == tail(1)) then
       ! loop over nodes in between tail(2) and head(2)
       do i = tail(2), head(2), step
          write (iounit, *) cntr, &
               this%quadrangularGrid%grid(head(1), i)%cornersY(1), &
               this%quadrangularGrid%grid(head(1), i)%cornersX(1)
          cntr = cntr + 1
       end do
    else
       ! loop over nodes in between tail(2) and head(1)
       do i = tail(1), head(1), step
          write (iounit, *) cntr, &
               this%quadrangularGrid%grid(i, head(2))%cornersY(1), &
               this%quadrangularGrid%grid(i, head(2))%cornersX(1)
          cntr = cntr + 1
       end do
    end if

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
