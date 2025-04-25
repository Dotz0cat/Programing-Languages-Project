enum, bind(c)
    enumerator :: STATE_PADDING
    enumerator :: STATE_0
    .
    .
    .
    enumerator :: STATE_29
    enumerator :: STATE_ACCEPT
    enumerator :: STATE_REJECT
end enum

integer, parameter :: state_type = KIND(STATE_0)
