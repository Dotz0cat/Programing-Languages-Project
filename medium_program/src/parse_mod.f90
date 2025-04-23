module parse_mod
  use iso_fortran_env
  use stack_mod, only: stack
  use terminal_mod, only: term_type
  use state_mod, only: state_type
  use action_mod, only: action_type
  use ast_mod, only: ast
  implicit none

  private

  public :: set_input_stack
  public :: parse_tokens
  public :: perform_evaluation
  public :: parse_success

  type, public :: parse
    class(stack), allocatable :: input_token_stack
    class(stack), allocatable :: state_stack
    class(stack), allocatable :: working_token_stack
    class(ast), pointer :: ast
    logical :: accepted
  contains
    procedure, public :: set_input_stack
    procedure, public :: parse => parse_tokens
    procedure, private :: do_action
    procedure, public :: perform_evaluation
    procedure, public :: parse_success
    final :: free_parse
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

  interface perform_evaluation
    module function perform_evaluation(this) result(value)
      class(parse), intent(inout) :: this
      real :: value
    end function perform_evaluation
  end interface perform_evaluation

  interface free_parse
    module subroutine free_parse(this)
      type(parse), intent(inout) :: this
    end subroutine free_parse
  end interface free_parse

  interface parse_success
    module function parse_success(this)
      class(parse), intent(inout) :: this
      logical :: parse_success
    end function parse_success
  end interface parse_success

end module parse_mod

