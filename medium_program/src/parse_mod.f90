module parse_mod
    use iso_fortran_env
    use stack_mod, only: stack
    use terminal_mod, only: term_type
    use state_mod, only: state_type
    use action_mod, only: action_type
    implicit none

    private

    public :: set_input_stack
    public :: parse_tokens

    type, public :: parse
        class(stack), allocatable :: input_token_stack
        class(stack), allocatable :: state_stack
        class(stack), allocatable :: working_token_stack
    contains
        procedure, public :: set_input_stack
        procedure, public :: parse => parse_tokens
        procedure, private :: do_action
    end type parse

    interface set_input_stack
        module subroutine set_input_stack(this, input_stack)
            class(parse), intent(inout) :: this
            class(stack), intent(in) :: input_stack
        end subroutine set_input_stack
    end interface set_input_stack

    interface parse_tokens
        module subroutine parse_tokens(this)
            class(parse), intent(inout) :: this
        end subroutine parse_tokens
    end interface parse_tokens

    interface do_action
       module subroutine do_action(this, state, action)
            class(parse), intent(inout) :: this
            integer(state_type), intent(inout) :: state
            integer(action_type), intent(in) :: action
        end subroutine do_action
    end interface do_action
end module parse_mod

