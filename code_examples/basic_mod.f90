module numbers
    implicit none
    
    public :: add_numbers

    interface add_numbers
        module integer function add_numbers(a, b) result(answer)
            integer, intent(in) :: a
            integer, intent(in) :: b
        end function add_numbers
    end interface

contains
    module procedure add_numbers
        answer = a + b
    end procedure add_numbers
end module numbers
