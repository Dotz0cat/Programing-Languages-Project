program calculator
    use input
    use processing
    implicit none

    real :: a
    real :: b

    real :: after

    integer :: operation

    call get_number(a)
    call get_number(b)

    call get_operation(operation)
    
    select case(operation)
        case (1)
            after = add_numbers(a, b)
        case (2)
            after = subtract_numbers(a, b)
        case (3)
            after = multiply_numbers(a, b)
        case (4)
            after = divide_numbers(a, b)
        case default
            print *, "invalid operation"
    end select

    if (after .eq. huge(0.0)) then
        return
    end if

    print *, "The result of the calculation is: ", after

end program calculator

