use MyClass_mod

type(MyClass) :: obj

call init_myclass(obj, 99.9)
call obj%reveal_data() ! Output: secret = 99.9
! print *, obj%secret_data ! ERROR: private access

