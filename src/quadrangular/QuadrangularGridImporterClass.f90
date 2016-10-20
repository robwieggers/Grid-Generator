module QuadrangularGridImporterClass
  use, intrinsic :: iso_fortran_env
  use TypesMod
  implicit none
  private

  type, public :: QuadrangularGridImporter
     character(charLen) :: filename
   contains
     procedure :: useFile => setQuadrangularGridImporterFileName
     procedure :: importCells => importCellsFromJson
     procedure :: importBoundary => importBoundaryFromJson
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
         __FILENAME__, __LINE__)
    end if
    if (len(trim(adjustl(filename))) > charLen) then
       call exception("quadrangular grid filename is too long", &
         __FILENAME__, __LINE__)
    end if
    inquire(file = trim(adjustl(filename)), exist = ex)
    if (.not.ex) then
       call exception("file " // trim(adjustl(filename)) // " does not exist", &
         __FILENAME__, __LINE__)
    end if
    this%filename = trim(adjustl(filename))
    
  end subroutine setQuadrangularGridImporterFilename


  function importCellsFromJson(this) result(cells)
    use json_module !IGNORE
    use json_file_module !IGNORE
    use ErrorHandlingMod
    use QuadrangularCellClass
    implicit none
    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: p !! a pointer for low-level manipulations
    logical :: found
    class(QuadrangularGridImporter) :: this
    type(QuadrangularCell), allocatable :: cells(:)
    integer(json_IK) :: var_type,n_children
    integer :: i, j, x, y, n_computational, cntr
    real(real64), dimension(:), allocatable :: coord
    character(charLen) :: index, nodeIndex
    character(kind=json_ck,len=:), allocatable :: type
    
    call json%initialize()
    call json%load_file(filename = this%filename)
    call json%get(p)
    call core%initialize()
    ! obtain number of cells, so we can allocate the array
    call core%info(p, json_cdk_'features', found, var_type, n_children)
    if (.not.found) then
       print *, "'features' not found, not a valid quadrangular grid"
    end if

    n_computational = 0
    do i = 1, n_children
       write(index, *) i
       index = adjustl(index)
       call json%get('features('//trim(index)//').properties.type', type)
       if (trim(adjustl(type)) == 'computational') then 
          n_computational = n_computational + 1
       end if
    end do
    
    allocate(cells(n_computational))

    cntr = 1
    ! loop over all the cells, and extract the data
    do i = 1, n_children
       write(index, *) i
       index = adjustl(index)
       call json%get('features('//trim(index)//').properties.type', type)
       if (type == 'computational') then
          call json%get('features('//trim(index)//').properties.x', x)
          call json%get('features('//trim(index)//').properties.y', y)
          cells(cntr)%x = x
          cells(cntr)%y = y

          do j = 1, 4
             write(nodeIndex, *) j
             nodeIndex = adjustl(nodeIndex)
             
             call json%get('features('//trim(index)//').geometry.coordinates(1)(' // &
                  trim(nodeIndex) //')', coord)
             cells(cntr)%cornersX(j) = coord(1)
             cells(cntr)%cornersY(j) = coord(2)
          end do
          cntr = cntr + 1
       end if
    end do

  end function importCellsFromJson


  function importBoundaryFromJson(this) result(boundary)
    use json_module !IGNORE
    use json_file_module !IGNORE
    use ErrorHandlingMod
    use QuadrangularBoundaryClass
    implicit none
    type(json_file) :: json
    type(json_core) :: core
    type(json_value), pointer :: p !! a pointer for low-level manipulations
    logical :: found
    class(QuadrangularGridImporter) :: this
    type(QuadrangularBoundary), allocatable :: boundary(:)
    integer(json_IK) :: var_type, n_children
    integer :: i, j, x, y, n_boundary, cntr, n_nodes
    real(real64), dimension(:), allocatable :: coord
    character(charLen) :: index, nodeIndex
    character(kind=json_ck,len=:), allocatable :: type, description
    
    call json%initialize()
    call json%load_file(filename = this%filename)
    call json%get(p)
    call core%initialize()
    ! obtain number of cells, so we can allocate the array
    call core%info(p, json_cdk_'features', found, var_type, n_children)
    if (.not.found) then
       print *, "'features' not found, not a valid quadrangular grid"
    end if

    n_boundary = 0
    do i = 1, n_children
       write(index, *) i
       index = adjustl(index)
       call json%get('features('//trim(index)//').properties.type', type)
       if (trim(adjustl(type)) == 'boundary') then 
          n_boundary = n_boundary + 1
       end if
    end do
    
    allocate(boundary(n_boundary))

    cntr = 1
    ! loop over all the cells, and extract the data
    do i = 1, n_children
       write(index, *) i
       index = adjustl(index)
       call json%get('features('//trim(index)//').properties.type', type)
       if (type == 'boundary') then
          call json%get('features('//trim(index)//').properties.description', description)
          boundary(cntr)%description = description

          call core%info(p, json_cdk_'features('//trim(index)//').geometry.coordinates', &
               found, var_type, n_nodes)
          allocate(boundary(cntr)%polyline(n_nodes, 2))
          do j = 1, n_nodes
             write(nodeIndex, *) j
             nodeIndex = adjustl(nodeIndex)
             call json%get('features('//trim(index)//').geometry.coordinates(' // &
                  trim(nodeIndex) // ')', coord)
             boundary(cntr)%polyline(j, :) = coord
          end do
          cntr = cntr + 1
       end if
    end do
  end function importBoundaryFromJson

end module QuadrangularGridImporterClass
