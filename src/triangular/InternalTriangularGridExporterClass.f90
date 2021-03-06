module InternalTriangularGridExporterClass
  use, intrinsic :: iso_fortran_env
  use QuadrangularGridClass
  use QuadrangularCellClass
  use ExporterClass
  implicit none
  private

  type, extends(Exporter), public :: InternalTriangularGridExporter
     type(QuadrangularGrid) :: quadrangularGrid
     logical :: holeAtCentroid
   contains
     procedure :: useQuadrangularGrid => setQuadrangularGrid
     procedure :: useHoleAtCentroid => setHoleAtCentroid
     procedure :: export => exportInternalTriangularGrid
  end type InternalTriangularGridExporter
    
  interface InternalTriangularGridExporter
     module procedure newInternalTriangularGridExporter
  end interface InternalTriangularGridExporter

contains
  function newInternalTriangularGridExporter()
    implicit none
    type(InternalTriangularGridExporter) :: newInternalTriangularGridExporter

    newInternalTriangularGridExporter%extension = '.poly'
    newInternalTriangularGridExporter%holeAtCentroid = .false.

  end function newInternalTriangularGridExporter


  subroutine setHoleAtCentroid(this, holeAtCentroid)
    implicit none
    class(InternalTriangularGridExporter) :: this
    logical, intent(in) :: holeAtCentroid
    
    this%holeAtCentroid = holeAtCentroid

  end subroutine setHoleAtCentroid

  
  subroutine setQuadrangularGrid(this, grid)
    implicit none
    class(InternalTriangularGridExporter) :: this
    type(QuadrangularGrid), intent(in) :: grid
    
    this%quadrangularGrid = grid
    
  end subroutine setQuadrangularGrid

  
  subroutine exportInternalTriangularGrid(this)
    use ErrorHandlingMod
    implicit none
    class(InternalTriangularGridExporter), intent(in) :: this
    integer :: ix, iy, cntr, nx, ny, iounit, j

    iounit = this%ioUnit
    nx = size(this%quadrangularGrid%grid, 1) ! correction for the guard cells
    ny = size(this%quadrangularGrid%grid, 2) ! correction for the guard cells
    write (*, *) 'nx, ny = ', nx, ny
    open(unit=iounit, file=this%filename, status='UNKNOWN')
    write (iounit, *) '# Set the nodes of the internal grid'
    write (iounit, *) 4 * nx * ny, 2, 0, 0

    cntr = 0
    do iy = 0, ny - 1
       do ix = 0, nx - 1
          write (iounit, *) cntr, &
               this%quadrangularGrid%grid(ix, iy)%cornersX(1), &
               this%quadrangularGrid%grid(ix, iy)%cornersY(1)
          cntr = cntr + 1
          write (iounit, *) cntr, &
               this%quadrangularGrid%grid(ix, iy)%cornersX(2), &
               this%quadrangularGrid%grid(ix, iy)%cornersY(2)
          cntr = cntr + 1
          write (iounit, *) cntr, &
               this%quadrangularGrid%grid(ix, iy)%cornersX(3), &
               this%quadrangularGrid%grid(ix, iy)%cornersY(3)
          cntr = cntr + 1
          write (iounit, *) cntr, &
               this%quadrangularGrid%grid(ix, iy)%cornersX(4), &
               this%quadrangularGrid%grid(ix, iy)%cornersY(4)
          cntr = cntr + 1
       end do
    end do

    ! add blank line
    write (iounit, *) ''

    ! add the list of b2.5 cell sides
    ! write comments line first
    write (iounit, *) '# set the sides'
    write (iounit, *) '# format: counter, 2 nodes, wall index'
    write (iounit, *) 4 * nx * ny, 1
    ! horizontal sides
    cntr = 0
    do iy = 0, ny - 1
       do ix = 0, nx - 1
          do j = 0, 2
             write (iounit, *) cntr, &
                  4 * (ix + iy * nx) + j, &
                  4 * (ix + iy * nx) + j + 1, &
                  cntr + 1000
             cntr = cntr + 1
          end do
          write (iounit, *) cntr, &
               4 * (ix + iy * nx) + 3, &
               4 * (ix + iy * nx) + 0, &
               cntr + 1000
          cntr = cntr + 1
       end do
    end do

    write (iounit, *) ''
    write (iounit, *) '#set the hole (for tokamak geometry)'
    write (iounit, *) '#format: counter, coordinate within the hole'
    if (this%holeAtCentroid) then
       ! add hole for tokamak 
       write (iounit, *) 1
       write (iounit, *) 1, &
            0.5 * ( &
            minval(this%quadrangularGrid%grid(:, :)%cornersX(1)) + &
            maxval(this%quadrangularGrid%grid(:, :)%cornersX(1))), &
            0.5 * ( &
            minval(this%quadrangularGrid%grid(:, :)%cornersY(1)) + &
            maxval(this%quadrangularGrid%grid(:, :)%cornersY(1)))
    else
       ! no hole
       write (iounit, *) 0
    end if
    close(iounit)
    
    ! run triangle for the internal grid
    call system('${TOPDIR}/bin/triangle -zepnQ ' // this%filename)
     
  end subroutine exportInternalTriangularGrid
  
end module InternalTriangularGridExporterClass
