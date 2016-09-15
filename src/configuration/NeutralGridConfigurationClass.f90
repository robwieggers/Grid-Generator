module NeutralGridConfigurationClass
  use, intrinsic :: iso_fortran_env
  implicit none
  private
  public NeutralGridConfiguration
  
  type :: ExternalArea
     real(real64) :: areaLimit
     real(real64) :: angleLimit
     integer, dimension(:, :), allocatable :: nodes
     integer, dimension(2) :: quadrangularNodeHead
     integer, dimension(2) :: quadrangularNodeTail
  end type ExternalArea

  type, public :: NeutralGridConfiguration
     logical :: createGrid
     type(ExternalArea), dimension(:), allocatable :: externalAreas
  end type NeutralGridConfiguration

  
contains
  
end module NeutralGridConfigurationClass
