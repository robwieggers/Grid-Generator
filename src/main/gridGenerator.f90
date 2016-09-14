program gridGenerator
  use ConfigurationClass
  use TriangularGridClass
  use TriangularGridExporterClass
  use QuadrangularGridClass
  use QuadrangularGridImporterClass

  implicit none

  type(Configuration) :: conf
  type(QuadrangularGrid) :: plasmaGrid
  type(QuadrangularGridImporter) :: plasmaGridImporter
  type(TriangularGrid) :: internalGrid
  type(TriangularGridExporter) :: internalGridExporter
  
  conf = Configuration()
  call conf%useFile('input.json')
  call conf%load()

  plasmaGridImporter = QuadrangularGridImporter()
  call plasmaGridImporter%useFile(conf%plasmaGridConf%filename)

  plasmaGrid = QuadrangularGrid()
  call plasmaGrid%useImporter(plasmaGridImporter)
  call plasmaGrid%import()

  if (conf%neutralGridConf%createGrid) then
     internalGridExporter = TriangularGridExporter()
     call internalGridExporter%useFile('internal.poly')
     call internalGridExporter%useUnitIO(10)
     
     internalGrid = TriangularGrid()
     call internalGrid%useExporter(internalGridExporter)
     call internalGrid%useQuadrangularGrid(plasmaGrid)
     call internalGrid%useGeometry(conf%geometry)
     call internalGrid%create()
  end if
  
end program gridGenerator
  
