module MyClass_mod
    implicit none

    private

    public :: MyClass, init_myclass

    type :: MyClass
        private
        
        real :: secret_data
    contains
        procedure :: reveal_data
    end type MyClass

contains
    module subroutine init_myclass(this, value)
        class(MyClass), intent(inout) :: this
        real, intent(in) :: value

        this%secret_data = value
    end subroutine init_myclass

    module subroutine reveal_data(this)
        class(MyClass), intent(inout) :: this
        
        print *, 'Secret = ', this%secret_data
    end subroutine reveal_data
end module MyClass_mod

