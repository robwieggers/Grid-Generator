module ConfigurationClass
  use TypesMod
  use ErrorHandlingMod
  use PlasmaGridConfigurationClass
  use NeutralGridConfigurationClass
  
  implicit none
  private
  
  type, public :: Configuration
     character(charLen) :: filename
     type(PlasmaGridConfiguration) :: plasmaGridConf
     type(NeutralGridConfiguration) :: neutralGridConf
   contains
     procedure :: load => loadConfiguration
     procedure :: useFile => setFileName
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
    logical :: found
    character(charLen) :: filename
    character(kind=json_CK,len=:), allocatable :: cval
    
    call json%initialize()
    call json%load_file(filename = this%filename)
!    call json%print_file()

    call json%get('plasma.filename', cval, found)
    if (.not.found) then
       call exception('plasmaGridFile not found in json input', &
         __FILE__, __LINE__)
    end if
    this%plasmaGridConf%filename = trim(adjustl(cval))
    
  end subroutine readJson
  
  subroutine setFilename(this, filename)
    use ErrorHandlingMod
    implicit none
    class(Configuration) :: this
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
    this%filename = trim(adjustl(filename))
    
  end subroutine setFilename
  
end module ConfigurationClass
