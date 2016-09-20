module QuadrangularCellClass
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  type, public :: QuadrangularCell
     integer :: x
     integer :: y
     real(kind=real64) :: cornersX(4)
     real(kind=real64) :: cornersY(4)
  end type QuadrangularCell

  
end module QuadrangularCellClass
