module GeoJsonWallExporterClass
  use QuadrangularGridClass
  use ExporterClass
  use NeutralGridConfigurationClass
  implicit none
  private
  
  type, extends(Exporter), public :: GeoJsonWallExporter
     type(ExternalArea), dimension(:), allocatable :: externalAreas
     type(QuadrangularGrid) :: quadrangularGrid
     logical :: externalAreasProvided
     logical :: quadrangularGridProvided
   contains
     procedure :: useExternalAreas => setExternalAreas
     procedure :: useQuadrangularGrid => setQuadrangularGrid
     procedure :: export => exportWalls
  end type GeoJsonWallExporter

  interface GeoJsonWallExporter
     module procedure newGeoJsonWallExporter
  end interface GeoJsonWallExporter

contains

  function newGeoJsonWallExporter()
    implicit none

    type(GeoJsonWallExporter) :: newGeoJsonWallExporter
    newGeoJsonWallExporter%extension = '.json'
    newGeoJsonWallExporter%externalAreasProvided = .false.
    newGeoJsonWallExporter%quadrangularGridProvided = .false.
    
  end function newGeoJsonWallExporter

  
  subroutine setExternalAreas(this, extAreas)
    implicit none
    class(GeoJsonWallExporter) :: this
    type (ExternalArea), dimension(:), intent(in) :: extAreas
    this%externalAreasProvided = .true.
    this%externalAreas = extAreas

  end subroutine setExternalAreas

  
  subroutine setQuadrangularGrid(this, quadGrid)
    use QuadrangularGridClass
    implicit none
    class(GeoJsonWallExporter) :: this
    type(QuadrangularGrid), intent(in) :: quadGrid

    this%quadrangularGrid = quadGrid
    this%quadrangularGridProvided = .true.
    
  end subroutine setQuadrangularGrid

  
  subroutine exportWalls(this)
    use,intrinsic :: iso_fortran_env, only: wp => real64
    use json_module !IGNORE
    use json_file_module !IGNORE
    use ErrorHandlingMod
    implicit none
    class(GeoJsonWallExporter), intent(in) :: this
    type(json_core) :: json
    type(json_value),pointer :: p, features !! a pointer for low-level manipulations
    integer :: i

    if (.not.this%externalAreasProvided) then
       call exception('No external areas defined, so information to export walls is incomplete', &
            __FILENAME__, __LINE__)
    end if
    if (.not.this%quadrangularGridProvided) then
       call exception('No quadrangularGrid provided, so information to export walls is incomplete', &
            __FILENAME__, __LINE__)
    end if
    
! initialize the class
    call json%initialize()

    ! initialize the structure:
    call json%create_object(p,'')
    call json%add(p, 'type', 'FeatureCollection')
    call json%create_array(features, 'features')
    call json%add(p, features)

    do i = 1, size(this%externalAreas)
       call addWall(json, features, this%externalAreas(i), this%quadrangularGrid)
    end do

    call json%print(p, this%filename)

    ! cleanup:
    call json%destroy(p)
    if (json%failed()) then
       call exception("failed to export to json", &
            __FILENAME__, __LINE__)
    end if

  end subroutine exportWalls

  subroutine addWall(json, parent, extArea, quadGrid)
    use json_module !IGNORE
    use json_file_module !IGNORE
    use QuadrangularGridClass
    IMPLICIT NONE
    type(json_core) :: json
    type(json_value),pointer :: parent, coordinates, feature, geometry, properties
    type(ExternalArea) :: extArea
    type(QuadrangularGrid) :: quadGrid
    integer :: i

    call json%create_object(feature, '')
    call json%add(parent, feature)
    call json%add(feature, 'type', 'Feature')
    call json%create_object(geometry, 'geometry')
    call json%add(feature, geometry)
    call json%add(geometry, 'type', 'LineString')
    call json%create_array(coordinates, 'coordinates')
    call json%add(geometry, coordinates)
    call json%add(coordinates, '', [ &
         quadGrid%grid( &
         extArea%quadrangularNodeHead, &
         extArea%quadrangularNodeHead)%cornersX(1), &
         quadGrid%grid( &
         extArea%quadrangularNodeHead, &
         extArea%quadrangularNodeHead)%cornersY(1) &
         ])
    do i = 1, size(extArea%nodes, 1)
       call json%add(coordinates, '', &
         [extArea%nodes(i,2), extArea%nodes(i,1)])
    end do
    call json%add(coordinates, '', [ &
         quadGrid%grid( &
         extArea%quadrangularNodeTail, &
         extArea%quadrangularNodeTail)%cornersX(1), &
         quadGrid%grid( &
         extArea%quadrangularNodeTail, &
         extArea%quadrangularNodeTail)%cornersY(1) &
         ])
    nullify(coordinates)
    nullify(geometry)
    
    call json%create_object(properties, 'properties')
    call json%add(feature, properties)
    call json%add(properties, 'type', 'wall')
    nullify(properties)
    nullify(feature)
    
  end subroutine addWall
  
  
end module GeoJsonWallExporterClass
