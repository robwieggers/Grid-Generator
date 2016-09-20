program gridGenerator
  use TypesMod
  use ConfigurationClass
  use InternalTriangularGridExporterClass
  use ExternalTriangularGridExporterClass
  use TriangularGridExporterClass
  use TriangularGridMergerClass
  use QuadrangularGridClass
  use QuadrangularGridImporterClass

  implicit none

  integer :: i
  character(charLen) :: iChar
  type(Configuration) :: conf
  type(QuadrangularGrid) :: plasmaGrid
  type(QuadrangularGridImporter) :: plasmaGridImporter
  type(InternalTriangularGridExporter) :: internalGridExporter
  type(ExternalTriangularGridExporter), allocatable, dimension(:) :: externalGridExporters
  type(TriangularGridMerger) :: gridMerger
  
  conf = Configuration()
  call conf%useFile('input.json')
  call conf%load()

  plasmaGridImporter = QuadrangularGridImporter()
  call plasmaGridImporter%useFile(conf%plasmaGridConf%filename)
  
  plasmaGrid = QuadrangularGrid()
  call plasmaGrid%useImporter(plasmaGridImporter)
  call plasmaGrid%import()
  if (conf%neutralGridConf%createGrid) then
     internalGridExporter = InternalTriangularGridExporter()
     call internalGridExporter%useFile('internal.poly')
     call internalGridExporter%useIOUnit(10)
     
     call internalGridExporter%useQuadrangularGrid(plasmaGrid)
     
     if (conf%geometry == 'tokamak') then
        call internalGridExporter%useHoleAtCentroid(.true.)
     end if
     call internalGridExporter%export()

     allocate(externalGridExporters(size(conf%neutralGridConf%externalAreas)))
     do i = 1, size(conf%neutralGridConf%externalAreas)
        write(iChar, *) i
        iChar = adjustl(iChar)
        externalGridExporters(i) = ExternalTriangularGridExporter()
        
        call externalGridExporters(i)%useQuadrangularGrid(plasmaGrid)
        call externalGridExporters(i)%useFile('external'// trim(iChar) //'.poly')
        call externalGridExporters(i)%useIOUnit(10+i)
        call externalGridExporters(i)%useExternalArea(conf%neutralGridConf%externalAreas(i))
        call externalGridExporters(i)%export()
     end do    

     gridMerger = TriangularGridMerger()
     call gridMerger%useInternalGridExporter(internalGridExporter)
     call gridMerger%useExternalGridExporters(externalGridExporters)
     call gridMerger%useIOUnits(8, 9)
     call gridMerger%useFileWithoutExtension(conf%neutralGridConf%filenameWithoutExtension)
     call gridMerger%merge()
     
  end if
  
end program gridGenerator
  
