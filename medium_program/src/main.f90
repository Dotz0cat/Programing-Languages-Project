program calculator
  use input
  use stack_mod, only: stack
  use parse_mod
  implicit none

  class(stack), allocatable :: token_stack
  type(parse), allocatable :: parser

  character(len=2048), allocatable :: user_input

  call get_user_input(user_input) 

  call lex(user_input, token_stack)

  deallocate(user_input)

  allocate(parser)

  call parser%set_input_stack(token_stack)

  call parser%parse()

  if (parser%parse_success()) then
    print *, 'Your result is', parser%perform_evaluation()
  else
    print *, 'error parsing your expression'
  end if

  deallocate(parser)

end program calculator

