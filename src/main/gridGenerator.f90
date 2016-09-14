program gridGenerator
  use ConfigurationClass
  use QuadrangularGridClass
  use QuadrangularGridImporterClass

  implicit none

  type(Configuration) :: conf
  type(QuadrangularGrid) :: plasmaGrid
  type(QuadrangularGridImporter) :: plasmaGridImporter

  conf = Configuration()
  call conf%useFile('input.json')
  call conf%load()

  plasmaGridImporter = QuadrangularGridImporter()
  call plasmaGridImporter%useFile(conf%plasmaGridConf%filename)

  plasmaGrid = QuadrangularGrid()
  call plasmaGrid%useImporter(plasmaGridImporter)
  call plasmaGrid%import()

  
end program gridGenerator
  
