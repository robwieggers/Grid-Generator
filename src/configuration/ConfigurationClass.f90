module ConfigurationClass
  use TypesMod
  use ErrorHandlingMod
  use PlasmaGridConfigurationClass
  use NeutralGridConfigurationClass
  
  implicit none
  private
  
  type, public :: Configuration
     character(charLen) :: filename
     type(PlasmaGridConfiguration) :: plasmaGridConfiguration
     type(NeutralGridConfiguration) :: neutralGridConfiguration
   contains
     procedure :: load => loadConfiguration
     procedure :: usingFile => setInputFileName
  end type Configuration

  interface Configuration
     module procedure newConfiguration
  end interface Configuration
  
contains

  function newConfiguration()
    implicit none
    
    type(Configuration) :: newConfiguration
    newConfiguration%filename = ''
    
  end function newConfiguration

  
  subroutine loadConfiguration(this)   
    implicit none
    class(Configuration), intent(in) :: this

    call readJson(this)
        
  end subroutine LoadConfiguration

  subroutine readJson(this)
    use json_module !IGNORE
    class(Configuration) :: this
    type(json_file) :: json

    call json%initialize()
    call json%load_file(filename = this%filename)
    call json%print_file()
    print *, 'done reading ' // this%filename
    
  end subroutine readJson
  
  function setInputFilename(this, filename) result (conf)
    use ErrorHandlingMod
    implicit none
    class(Configuration) :: this
    type(Configuration) :: conf
    character*(*), intent(in) :: filename
    logical :: ex

    if (trim(filename) == "") then
       call exception("inputfile is empty string", &
         __FILE__, __LINE__)
    end if
    if (len(trim(adjustl(filename))) > charLen) then
       call exception("inputfile is too long", &
         __FILE__, __LINE__)
    end if
    inquire(file = trim(adjustl(filename)), exist = ex)
    if (.not.ex) then
       call exception("inputfile " // trim(adjustl(filename)) // " does not exist", &
         __FILE__, __LINE__)
    end if
    conf = this
    conf%filename = trim(adjustl(filename))
    
  end function setInputFilename
  
end module ConfigurationClass
