module numbers_p
    implicit none
    
    private

    public :: add_numbers

    interface add_numbers
        integer function add_numbers(a, b) result(total)
            integer, intent(in) :: a
            integer, intent(in) :: b
        end function add_numbers
    end interface add_numbers
end module numbers_p

