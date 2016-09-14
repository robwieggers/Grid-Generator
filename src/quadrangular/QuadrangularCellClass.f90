module QuadrangularCellClass
  use, intrinsic :: iso_fortran_env
  implicit none
  private

  type, public :: QuadrangularCell
     integer :: x
     integer :: y
     real(kind=real64) :: cornersX(0:3)
     real(kind=real64) :: cornersY(0:3)
  end type QuadrangularCell

  
end module QuadrangularCellClass
