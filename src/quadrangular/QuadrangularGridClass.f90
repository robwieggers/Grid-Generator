module QuadrangularGridClass
  use, intrinsic :: iso_fortran_env
  use QuadrangularGridImporterClass
  use QuadrangularCellClass
  use QuadrangularBoundaryClass
  implicit none
  private

  type, public :: QuadrangularGrid
     type(QuadrangularGridImporter) :: importer
     type(QuadrangularCell), pointer :: grid(:, :)
     type(QuadrangularBoundary), pointer :: boundary(:)
     
   contains
     procedure :: useImporter => setQuadrangularGridImporter
     procedure :: import => importGrid
  end type QuadrangularGrid

  interface QuadrangularGrid
     module procedure newQuadrangularGrid
  end interface QuadrangularGrid

contains
  
  function newQuadrangularGrid()
    implicit none

    type(QuadrangularGrid) :: newQuadrangularGrid

  end function newQuadrangularGrid

  subroutine setQuadrangularGridImporter(this, importer)
    implicit none
    class(QuadrangularGrid) :: this
    type(QuadrangularGridImporter), intent(in) :: importer
    this%importer = importer

  end subroutine setQuadrangularGridImporter

  subroutine importGrid(this)
    use QuadrangularCellClass
    use QuadrangularBoundaryClass
    implicit none
    
    class(QuadrangularGrid) :: this
    type(QuadrangularCell), allocatable :: cells(:)
    type(QuadrangularBoundary), allocatable :: boundary(:)
    integer :: i
    
    cells = this%importer%importCells()
    allocate(this%grid( &
         minval(cells(:)%x):maxval(cells(:)%x), &
         minval(cells(:)%y):maxval(cells(:)%y)))
    forall (i = 1:size(cells))
       this%grid(cells(i)%x, cells(i)%y) = cells(i)
    end forall
    boundary = this%importer%importBoundary()
    allocate(this%boundary(size(boundary)))
    forall (i = 1:size(boundary))
       this%boundary(i) = boundary(i)
    end forall
  end subroutine importGrid
  
end module QuadrangularGridClass
