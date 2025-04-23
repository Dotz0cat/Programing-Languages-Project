module input
  use token_mod, only: token_type
  use stack_mod, only: stack
  implicit none
  
  private

  public :: get_user_input
  public :: lex

  interface get_user_input
    module subroutine get_user_input(line)
      character(*), intent(out), allocatable :: line
    end subroutine get_user_input
  end interface get_user_input

  interface lex
    module subroutine lex(line, values)
      character(*), intent(in) :: line
      class(stack), intent(out), allocatable :: values
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
