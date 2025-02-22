! Somethiing something comment
program calculator
    use input
    use processing
    implicit none

    real :: a
    real :: b

    real :: after

    integer :: operation

    call get_operation(operation)

    call get_number(a)
    call get_number(b)

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

    print *, "the end is ", after

end program calculator

