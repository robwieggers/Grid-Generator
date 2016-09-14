module QuadrangularGridImporterClass
  use, intrinsic :: iso_fortran_env
  use TypesMod
  implicit none
  private

  type, public :: QuadrangularGridImporter
     character(charLen) :: filename
   contains
     procedure :: useFile => setQuadrangularGridImporterFileName
     procedure :: import => importFromJson
  end type QuadrangularGridImporter

  interface QuadrangularGridImporter
     module procedure newQuadrangularGridImporter
  end interface QuadrangularGridImporter

contains
  function newQuadrangularGridImporter()
    implicit none

    type(QuadrangularGridImporter) :: newQuadrangularGridImporter
    newQuadrangularGridImporter%filename = ''

  end function newQuadrangularGridImporter


  subroutine setQuadrangularGridImporterFilename(this, filename)
    use ErrorHandlingMod
    implicit none
    class(QuadrangularGridImporter) :: this
    character*(*), intent(in) :: filename
    logical :: ex

    if (trim(filename) == "") then
       call exception("quadrangular grid filename is empty string", &
         __FILE__, __LINE__)
    end if
    if (len(trim(adjustl(filename))) > charLen) then
       call exception("quadrangular grid filename is too long", &
         __FILE__, __LINE__)
    end if
    inquire(file = trim(adjustl(filename)), exist = ex)
    if (.not.ex) then
       call exception("file " // trim(adjustl(filename)) // " does not exist", &
         __FILE__, __LINE__)
    end if
    this%filename = trim(adjustl(filename))
    
  end subroutine setQuadrangularGridImporterFilename


  function importFromJson(this) result(cells)
    use json_module !IGNORE
    use json_file_module !IGNORE
    use QuadrangularCellClass
    implicit none
    type(json_file) :: json
    type(json_core) :: core
    type(json_value),pointer :: p !! a pointer for low-level manipulations
    logical :: found
    class(QuadrangularGridImporter) :: this
    type(QuadrangularCell), allocatable :: cells(:)
    integer(json_IK) :: var_type,n_children
    integer :: i, x, y
    real(real64), dimension(:), allocatable :: coord
    character(charLen) :: index
    
    call json%initialize()
    call json%load_file(filename = this%filename)
    call json%get(p)
    call core%initialize()
    ! obtain number of cells, so we can allocate the array
    call core%info(p, json_cdk_'features', found, var_type, n_children)
    if (.not.found) then
       print *, "'features' not found, not a valid quadrangular grid"
    end if

    allocate(cells(n_children))
    
    ! loop over all the cells, and extract the data
    do i = 1, n_children
       write(index, *) i
       index = adjustl(index)
       call json%get('features('//trim(index)//').properties.x', x)
       call json%get('features('//trim(index)//').properties.y', y)
       cells(i)%x = x
       cells(i)%y = y      
       call json%get('features('//trim(index)//').geometry.coordinates(1)', coord)
       cells(i)%cornersX(0) = coord(1)
       cells(i)%cornersY(0) = coord(2)
       call json%get('features('//trim(index)//').geometry.coordinates(2)', coord)
       cells(i)%cornersX(1) = coord(1)
       cells(i)%cornersY(1) = coord(2)
       call json%get('features('//trim(index)//').geometry.coordinates(3)', coord)
       cells(i)%cornersX(2) = coord(1)
       cells(i)%cornersY(2) = coord(2)
       call json%get('features('//trim(index)//').geometry.coordinates(4)', coord)
       cells(i)%cornersX(3) = coord(1)
       cells(i)%cornersY(3) = coord(2)
    end do
    
  end function importFromJson

end module QuadrangularGridImporterClass
