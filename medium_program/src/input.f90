module input
    use token_mod, only: token_type
    use stack_mod, only: stack
    implicit none
    
    private

    public :: get_number
    public :: get_operation
    public :: lex

    interface get_number
        module subroutine get_number(num)
            real, intent(out) :: num
        end subroutine get_number
    end interface get_number

    interface get_operation
        module subroutine get_operation(operation)
            integer, intent(out) :: operation
        end subroutine get_operation
    end interface get_operation

    interface lex
        module subroutine lex(line, output_stack)
            character(*), intent(in) :: line
            class(stack), intent(out), allocatable :: output_stack
        end subroutine lex
    end interface lex

    interface op_lookup
        pure module function op_lookup(op) result(type)
            character(*), intent(in) :: op
            integer(token_type) :: type
        end function op_lookup
    end interface op_lookup
    
    interface function_lookup
        pure module function function_lookup(func) result(type)
            character(*), intent(in) :: func
            integer(token_type) :: type
        end function function_lookup
    end interface function_lookup
end module input
