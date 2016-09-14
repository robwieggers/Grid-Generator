module TriangularGridClass
  use, intrinsic :: iso_fortran_env
  use QuadrangularGridClass
  use QuadrangularCellClass
  use TriangularGridExporterClass
  implicit none
  private

  type, public :: TriangularGrid
     type(TriangularGridExporter) :: exporter
     type(QuadrangularGrid) :: quadrangularGrid
     character(charLen) :: geometry
   contains
     procedure :: useExporter => setTriangularGridExporter
     procedure :: useQuadrangularGrid => setQuadrangularGrid
     procedure :: useGeometry => setGeometry
     procedure :: create => createInternalGrid
  end type TriangularGrid

  interface TriangularGrid
     module procedure newTriangularGrid
  end interface TriangularGrid

contains
  function newTriangularGrid()
    implicit none

    type(TriangularGrid) :: newTriangularGrid
    newTriangularGrid%geometry = ''

  end function newTriangularGrid

  subroutine setTriangularGridExporter(this, exporter)
    implicit none
    class(TriangularGrid) :: this
    type(TriangularGridExporter), intent(in) :: exporter
    
    this%exporter = exporter

  end subroutine setTriangularGridExporter

  
  subroutine setGeometry(this, geometry)
    implicit none
    class(TriangularGrid) :: this
    character(charLen), intent(in) :: geometry
    
    this%geometry = geometry

  end subroutine setGeometry

  
  subroutine setQuadrangularGrid(this, grid)
    implicit none
    class(TriangularGrid) :: this
    type(QuadrangularGrid) :: grid
    
    this%quadrangularGrid = grid
    
  end subroutine setQuadrangularGrid

  
  subroutine createInternalGrid(this)
    use ErrorHandlingMod
    implicit none
    class(TriangularGrid) :: this
    integer :: ix, iy, iounit, cntr, nx, ny

    iounit = this%exporter%unitIO
    nx = size(this%quadrangularGrid%grid, 1) - 2 ! correction for the guard cells
    ny = size(this%quadrangularGrid%grid, 2) - 2 ! correction for the guard cells

    open(unit=iounit, file=this%exporter%filename, status='UNKNOWN')
    write (iounit, *) '# Set the nodes of the internal grid'
    write (iounit, *) (nx + 1) * (ny + 1), 2, 0, 0

    cntr = 0
    do iy = 0, ny
       do ix = 0, nx
          write (iounit, *) cntr, &
               this%quadrangularGrid%grid(ix, iy)%cornersY(0), &
               this%quadrangularGrid%grid(ix, iy)%cornersX(0)
          cntr = cntr + 1
       end do
    end do

    ! add blank line
    write (iounit, *) ''

    ! add the list of b2.5 cell sides
    ! write comments line first
    write (iounit, *) '# set the sides'
    write (iounit, *) '# format: counter, 2 nodes, wall index'
    write (iounit, *) (nx)*(ny+1) + ny*(nx+1), 1
    ! horizontal sides
    cntr = 0
    do iy = 0, ny
       do ix = 0, nx - 1
          write (iounit, *) cntr, &
               ix + iy * (nx + 1), &
               ix + iy * (nx + 1) + 1, &
               cntr + 1000
          cntr = cntr + 1
        end do
      end do
      ! vertical sides
      do ix = 0, nx
         do iy = 0, ny - 1
            write (iounit, *) cntr, &
                 ix + iy * (nx + 1), &
                 ix + (iy + 1) * (nx + 1), &
                 cntr + 1000
            cntr = cntr + 1
        end do
      end do

      write (iounit, *) ''
      select case (this%geometry)
      case('linear device')
         ! no hole
         write (iounit, *) 0
      case('tokamak')
         ! add hole for tokamak 
         write (iounit, *) '#set the hole (for tokamak geometry)'
         write (iounit, *) '#format: counter, coordinate within the hole'
         write (iounit, *) 1
         write (iounit, *) 1, &
              0.5 * ( &
              minval(this%quadrangularGrid%grid(:, :)%cornersY(0)) + &
              maxval(this%quadrangularGrid%grid(:, :)%cornersY(0))), &
              0.5 * ( &
              minval(this%quadrangularGrid%grid(:, :)%cornersX(0)) + &
              maxval(this%quadrangularGrid%grid(:, :)%cornersX(0)))
      case default
         call exception('No valid geometry specified', &
              __FILE__, __LINE__)
      end select
      close(iounit)

      ! run triangle for the internal grid
      call system('${TOPDIR}/bin/triangle -zepn ' // this%exporter%filename)
     
    
  end subroutine createInternalGrid
  
end module TriangularGridClass
