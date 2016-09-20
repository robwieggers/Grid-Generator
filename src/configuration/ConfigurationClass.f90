module ConfigurationClass
  use TypesMod
  use ErrorHandlingMod
  use PlasmaGridConfigurationClass
  use NeutralGridConfigurationClass
  
  implicit none
  private
  
  type, public :: Configuration
     character(charLen) :: filename
     character(charLen) :: geometry
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
    newConfiguration%geometry = ''
    
  end function newConfiguration

  
  subroutine loadConfiguration(this)   
    implicit none
    class(Configuration), intent(in) :: this

    call readJson(this)
        
  end subroutine LoadConfiguration

  
  subroutine readJson(this)
    use, intrinsic :: iso_fortran_env
    use json_module !IGNORE
    class(Configuration) :: this
    type(json_file) :: json
    integer :: i, j, nrExternalRegions, nrNodes
    character(charLen) :: iChar, jChar
    
    call json%initialize()
    call json%load_file(filename = this%filename)
    !    call json%print_file()

    ! import general configuration
    this%geometry = jsonExtractString(json, 'geometry')
    if (.not.&
         (this%geometry == 'tokamak') .or. &
         (this%geometry == 'linear device')) then
       call exception('unknown geometry in json input: ' // trim(this%geometry), &
            __FILENAME__, __LINE__)
    end if
       
    ! import plasma configuration
    this%plasmaGridConf%filename = jsonExtractString(json, 'plasma.filename')

    ! import neutral configuration
    this%neutralGridConf%createGrid = jsonExtractLogical(json, 'neutrals.createGrid')

    if (this%neutralGridConf%createGrid) then
       this%neutralGridConf%filenameWithoutExtension = &
            jsonExtractString(json, 'neutrals.filenameWithoutExtension') 
       if (jsonContainsName(json, 'neutrals.externalRegions')) then

          call json%info('neutrals.externalRegions', n_children = nrExternalRegions)
          allocate(this%neutralGridConf%externalAreas(nrExternalRegions))

          do i = 1, nrExternalRegions
             write(iChar, *) i
             iChar = adjustl(iChar)

             this%neutralGridConf%externalAreas(i)%areaLimit = &
                  jsonExtractReal(json, 'neutrals.externalRegions('//trim(iChar)//').areaLimit')
             this%neutralGridConf%externalAreas(i)%angleLimit = &
                  jsonExtractReal(json, 'neutrals.externalRegions('//trim(iChar)//').angleLimit')

             this%neutralGridConf%externalAreas(i)%quadrangularNodeHead = &
                  jsonExtractIntegerArray(json, &
                  'neutrals.externalRegions('//trim(iChar)//').connectToQuadrangularNodeHead')
             this%neutralGridConf%externalAreas(i)%quadrangularNodeTail = &
                  jsonExtractIntegerArray(json, &
                  'neutrals.externalRegions('//trim(iChar)//').connectToQuadrangularNodeTail')
             if (this%neutralGridConf%externalAreas(i)%quadrangularNodeHead(1) /= &
                  this%neutralGridConf%externalAreas(i)%quadrangularNodeTail(1) .and. &
                 this%neutralGridConf%externalAreas(i)%quadrangularNodeHead(2) /= &
                 this%neutralGridConf%externalAreas(i)%quadrangularNodeTail(2)) then
                call exception('first and last node should be on same boundary of the quadrangular', &
                     __FILENAME__, __LINE__)
             end if
             if (this%neutralGridConf%externalAreas(i)%quadrangularNodeHead(1) == &
                  this%neutralGridConf%externalAreas(i)%quadrangularNodeTail(1) .and. &
                 this%neutralGridConf%externalAreas(i)%quadrangularNodeHead(2) == &
                 this%neutralGridConf%externalAreas(i)%quadrangularNodeTail(2)) then
                call exception('first and last node should be different nodes on a single boundary of the quadrangular', &
                     __FILENAME__, __LINE__)
             end if
             
             call json%info('neutrals.externalRegions('//trim(iChar)//').nodes', n_children = nrNodes)
             allocate(this%neutralGridConf%externalAreas(i)%nodes(nrNodes, 2))
             do j = 1, nrNodes
                write(jChar, *) j
                jChar = adjustl(jChar)

                this%neutralGridConf%externalAreas(i)%nodes(j, :) = &
                     jsonExtractRealArray(json, &
                     'neutrals.externalRegions('//trim(iChar)//').nodes('//trim(jChar)//')')
             end do
          end do
       end if
    end if
  end subroutine readJson

  
  subroutine jsonShouldContainName(json, name)
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    character(kind=json_CK,len=:),allocatable :: foo
    logical :: found
    
    call json%info(name,found)
    if (.not.found) then
       call exception(name // ' not specified', &
            __FILENAME__, __LINE__)
    end if
  end subroutine jsonShouldContainName

  
  function jsonContainsName(json, name) result(found)
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    logical :: found
    
    call json%info(name,found)
  end function jsonContainsName

  
  function jsonExtractString(json, name) result(value)
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    character(kind=json_CK,len=:), allocatable :: value
    logical :: found

    call jsonShouldContainName(json, name)  
    call json%get(name, value, found)
    if (.not.found) then
       call exception(name // ' not specified in correct type', &
            __FILENAME__, __LINE__)
    end if
  end function jsonExtractString


    function jsonExtractInteger(json, name) result(value)
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    integer :: value
    logical :: found

    call jsonShouldContainName(json, name)  
    call json%get(name, value, found)
    if (.not.found) then
       call exception(name // ' not specified in correct type', &
            __FILENAME__, __LINE__)
    end if    
  end function jsonExtractInteger


  function jsonExtractIntegerArray(json, name) result(value)
    use, intrinsic :: iso_fortran_env
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    integer, dimension(:), allocatable :: value
    logical :: found

    call jsonShouldContainName(json, name)
    call json%get(name, value, found)
    if (.not.found) then
       call exception(name // ' not specified in correct type', &
            __FILENAME__, __LINE__)
    end if
  end function jsonExtractIntegerArray
  

  function jsonExtractReal(json, name) result(value)
    use, intrinsic :: iso_fortran_env
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    real(real64) :: value
    logical :: found

    call jsonShouldContainName(json, name)
    call json%get(name, value, found)
    if (.not.found) then
       call exception(name // ' not specified in correct type', &
            __FILENAME__, __LINE__)
    end if
  end function jsonExtractReal


  function jsonExtractRealArray(json, name) result(value)
    use, intrinsic :: iso_fortran_env
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    real(real64), dimension(:), allocatable :: value
    logical :: found

    call jsonShouldContainName(json, name)
    call json%get(name, value, found)
    if (.not.found) then
       call exception(name // ' not specified in correct type', &
            __FILENAME__, __LINE__)
    end if
  end function jsonExtractRealArray


  function jsonExtractLogical(json, name) result(value)
    use json_module !IGNORE
    type(json_file) :: json
    character*(*), intent(in) :: name
    logical :: found, value

    call jsonShouldContainName(json, name)  
    call json%get(name, value, found)
    if (.not.found) then
       call exception(name // ' not specified in correct type', &
            __FILENAME__, __LINE__)
    end if
  end function jsonExtractLogical
  
  
  subroutine setFilename(this, filename)
    use ErrorHandlingMod
    implicit none
    class(Configuration) :: this
    character*(*), intent(in) :: filename
    logical :: ex

    if (trim(filename) == "") then
       call exception("inputfile is empty string", &
         __FILENAME__, __LINE__)
    end if
    if (len(trim(adjustl(filename))) > charLen) then
       call exception("inputfile is too long", &
         __FILENAME__, __LINE__)
    end if
    inquire(file = trim(adjustl(filename)), exist = ex)
    if (.not.ex) then
       call exception("inputfile " // trim(adjustl(filename)) // " does not exist", &
         __FILENAME__, __LINE__)
    end if
    this%filename = trim(adjustl(filename))
    
  end subroutine setFilename
  
end module ConfigurationClass
