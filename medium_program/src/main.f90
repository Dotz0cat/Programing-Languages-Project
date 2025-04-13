program calculator
    use input
    use processing
    use stack_mod, only: stack
    use parse_mod
    implicit none

    !real :: a
    !real :: b

    !real :: after

    !integer :: operation

    class(stack), allocatable :: token_stack

    type(parse), allocatable :: parser

    call lex('(123+ 456 -  2 * sin(3))', token_stack)

    allocate(parser)

    call parser%set_input_stack(token_stack)

    call parser%parse()

    !call get_number(a)
    !call get_number(b)

    !call get_operation(operation)
    
    !select case(operation)
    !    case (1)
    !        after = add_numbers(a, b)
    !    case (2)
    !        after = subtract_numbers(a, b)
    !    case (3)
    !        after = multiply_numbers(a, b)
    !    case (4)
    !        after = divide_numbers(a, b)
    !    case default
    !        print *, "invalid operation"
    !end select

    !if (after .eq. huge(0.0)) then
    !    return
    !end if

    !print *, "The result of the calculation is: ", after

end program calculator

