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
