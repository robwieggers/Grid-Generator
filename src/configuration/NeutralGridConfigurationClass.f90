module NeutralGridConfigurationClass
  use, intrinsic :: iso_fortran_env
  use TypesMod
  implicit none
  private
  public NeutralGridConfiguration
  
  type, public :: ExternalArea
     character(charLen) :: areaLimit
     character(charLen) :: angleLimit
     real(real64), dimension(:, :), allocatable :: nodes
     integer :: quadrangularNodeHead
     integer :: quadrangularNodeTail
     character(charLen) :: quadrangularBoundary     
  end type ExternalArea

  type, public :: NeutralGridConfiguration
     logical :: createGrid
     character(charLen) :: filenameWithoutExtension
     type(ExternalArea), dimension(:), allocatable :: externalAreas
  end type NeutralGridConfiguration

  
contains
  
end module NeutralGridConfigurationClass
