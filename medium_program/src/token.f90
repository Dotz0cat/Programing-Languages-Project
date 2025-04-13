module token_mod
    implicit none

    public

    enum, bind(c)
        enumerator :: OPEN
        enumerator :: CLOSE
        enumerator :: NUMBER
        enumerator :: FUNCTION
        enumerator :: OPERATOR
        enumerator :: PLUS
        enumerator :: MINUS
        enumerator :: TIMES
        enumerator :: DIVIDE
        enumerator :: MODULO
        enumerator :: SIN
        enumerator :: COS
        enumerator :: TAN
        enumerator :: SQRT
        enumerator :: LOG
        enumerator :: END
    end enum

    integer, parameter :: token_type = kind(OPEN)
end module token_mod

