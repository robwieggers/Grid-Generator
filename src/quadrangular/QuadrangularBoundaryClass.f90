module QuadrangularBoundaryClass
  use, intrinsic :: iso_fortran_env
  use TypesMod
  implicit none
  private

  type, public :: QuadrangularBoundary
     real(kind=real64), dimension(:, :), allocatable :: polyline
     character (charlen) :: description
  end type QuadrangularBoundary
  
end module QuadrangularBoundaryClass
