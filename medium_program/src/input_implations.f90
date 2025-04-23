submodule (input) input_imp
  use token_mod
  use stack_mod
  implicit none

  enum, bind(c)
    enumerator :: accepting
    enumerator :: accepting_number
    enumerator :: accepting_function
    enumerator :: accepted_open
    enumerator :: accepted_close
    enumerator :: accepted_operator
    enumerator :: done_number
    enumerator :: done_function
    enumerator :: error
  end enum
contains
  module procedure get_user_input
    allocate(line)
    print '(A$)', "Enter calculation to perform: "
    read (*, '(A)') line
  end procedure get_user_input

  module procedure lex
    integer :: i
    integer(kind(accepting)) :: state
    integer :: open_parns, close_parns
    character(len=256), allocatable :: holder
    integer :: holder_index
    logical :: decimal_point
    integer(token_type) :: token
    integer :: line_len
    
    state = accepting
    i = 1
    open_parns = 0
    close_parns = 0
    holder_index = 1
    decimal_point = .false.
    line_len = len(line)
    allocate(holder)
    allocate(values)

    ! a fix for my grammar that reconizes (a + b) but not a + b
    call values%push('(', OPEN)

    do
      if (i .le. line_len) then
        select case(line(i:i))
          case ('(')
            select case (state)
              case (accepting_number)
                state = done_number
              case (accepting_function)
                state = done_function
              case (accepting)
                state = accepted_open
              case default
                state = error
            end select
          case (')')
            select case (state)
              case (accepting_number)
                state = done_number
              case (accepting_function)
                state = done_function
              case (accepting)
                state = accepted_close
              case default
                state = error
            end select
          case ('0':'9')
            select case(state)
              case (accepting_number)
                holder_index = holder_index + 1
                holder(holder_index:holder_index) = line(i:i)
              case (accepting)
                holder = ''
                holder_index = 1
                holder(holder_index:holder_index) = line(i:i)
                state = accepting_number
              case (accepting_function)
                state = done_function
              case default
                state = error
            end select
          case ('a':'z', 'A':'Z')
            select case(state)
              case (accepting)
                holder = ''
                holder_index = 1
                holder(holder_index:holder_index) = line(i:i)
                state = accepting_function
              case (accepting_number)
                state = done_number
              case (accepting_function)
                holder_index = holder_index + 1
                holder(holder_index:holder_index) = line(i:i)
              case default
                state = error
            end select
          case ('.')
            select case(state)
              case (accepting)
                holder = '0'
                holder_index = 2
                holder(holder_index:holder_index) = line(i:i)
                decimal_point = .true.
                state = accepting_number
              case (accepting_number)
                if (.not. decimal_point) then
                  holder_index = holder_index + 1
                  holder(holder_index:holder_index) = line(i:i)
                  decimal_point = .true.
                else
                  state = error
                end if
              case (accepting_function)
                state = done_function
              case default
                state = error
            end select
          case ('+', '-', '*', '/', '%')
            select case (state)
              case (accepting)
                state = accepted_operator
              case (accepting_number)
                state = done_number
              case (accepting_function)
                state = done_function
              case default
                state = error
            end select
          case (' ', '\n', '\t')
            select case (state)
              case (accepting_number)
                state = done_number
              case (accepting_function)
                state = done_function
              case (accepting)
                state = accepting
            end select
        end select
      end if
      
      select case (state)
        case (done_number)
          token = NUMBER
          call values%push(holder(1:holder_index), token)
          ! don't increment as we reached the end of this
          state = accepting
        case (done_function)
          token = function_lookup(holder(1:holder_index))
          call values%push(holder(1:holder_index), token)
          state = accepting
        case (accepted_open)
          token = OPEN
          call values%push('(', token)
          open_parns = open_parns + 1
          i = i + 1
          state = accepting
        case (accepted_close)
          token = CLOSE
          call values%push(')', token)
          close_parns = close_parns + 1
          i = i + 1
          state = accepting
        case (accepted_operator)
          token = op_lookup(line(i:i))
          call values%push(line(i:i), token)
          i = i + 1
          state = accepting
        case (accepting, accepting_number, accepting_function)
          i = i + 1
        case (error)
          exit
      end select

      if (i .gt. line_len) then 
        select case (state)
          case (accepting_number)
            state = done_number
          case (accepting_function)
            state = done_function
          case default
            exit
        end select
      end if
    end do

    if (state .eq. error) then
      print *, 'there was an error'
    else if (open_parns .ne. close_parns) then
      print *, 'There was an issue with the parthacies'
    end if

    ! a fix for my grammar that reconizes (a + b) but not a + b
    call values%push(')', CLOSE)

    call values%push('$', END)
    
    deallocate(holder)
  end procedure lex

  module procedure op_lookup

    select case(op)
      case ('+')
        type = PLUS
      case ('-')
        type = MINUS
      case ('*')
        type = TIMES
      case ('/')
        type = DIVIDE
      case ('%')
        type = MODULO
      case default
        type = OPERATOR
    end select
  end procedure op_lookup

  module procedure function_lookup
    select case (func)
      case ('sin')
        type = SIN
      case ('cos')
        type = COS
      case ('tan')
        type = TAN
      case ('log')
        type = LOG
      case default
        type = FUNCTION
    end select
  end procedure function_lookup
end submodule input_imp
