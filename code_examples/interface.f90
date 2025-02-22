interface add_numbers
    integer function add_numbers(a, b) result(sum)
        integer, intent(in) :: a
        integer, intent(in) :: b
    end function add_numbers
end interface add_numbers
