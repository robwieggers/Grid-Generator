program gridGenerator
  use ConfigurationClass

  implicit none

  type(Configuration) :: conf
  conf = Configuration()
  conf = conf%usingFile('input.json')
  call conf%load()

  

end program gridGenerator
  
