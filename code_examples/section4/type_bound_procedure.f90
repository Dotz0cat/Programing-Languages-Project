type :: MyClass
    real :: data
contains
    procedure :: print_data
end type MyClass

subroutine print_data(this)
    class(MyClass), intent(inout) :: this

    print *, 'data= ', this%data
end subroutine print_data

