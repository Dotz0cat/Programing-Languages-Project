module terminal_mod
  implicit none

  public

  enum, bind(c)
    enumerator :: NON_TERMINAL_padding
    enumerator :: TERMINAL_END
    enumerator :: TERMINAL_MODULO
    enumerator :: TERMINAL_OPEN
    enumerator :: TERMINAL_CLOSE
    enumerator :: TERMINAL_TIMES
    enumerator :: TERMINAL_PLUS
    enumerator :: TERMINAL_MINUS
    enumerator :: TERMINAL_DIVIDE
    enumerator :: TERMINAL_COS
    enumerator :: TERMINAL_LOG
    enumerator :: TERMINAL_NUMBER
    enumerator :: TERMINAL_SIN
    enumerator :: TERMINAL_SQRT
    enumerator :: TERMINAL_TAN
    enumerator :: NON_TERMINAL_error
    enumerator :: NON_TERMINAL_expr
    enumerator :: NON_TERMINAL_factor
    enumerator :: NON_TERMINAL_funct
    enumerator :: NON_TERMINAL_stmt
    enumerator :: NON_TERMINAL_term
  end enum

  integer, parameter :: term_type = KIND(TERMINAL_END)
end module terminal_mod