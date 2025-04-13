module action_mod
    implicit none

    public

    enum, bind(c)
        enumerator :: REDUCE_1
        enumerator :: REDUCE_2
        enumerator :: REDUCE_3
        enumerator :: REDUCE_4
        enumerator :: REDUCE_5
        enumerator :: REDUCE_6
        enumerator :: REDUCE_7
        enumerator :: REDUCE_8
        enumerator :: REDUCE_9
        enumerator :: REDUCE_10
        enumerator :: REDUCE_11
        enumerator :: REDUCE_12
        enumerator :: REDUCE_13
        enumerator :: REDUCE_14
        enumerator :: REDUCE_15
        enumerator :: REDUCE_16
        enumerator :: REDUCE_17
        enumerator :: SHIFT_5
        enumerator :: SHIFT_7
        enumerator :: SHIFT_8
        enumerator :: SHIFT_9
        enumerator :: SHIFT_10
        enumerator :: SHIFT_11
        enumerator :: SHIFT_12
        enumerator :: SHIFT_13
        enumerator :: SHIFT_14
        enumerator :: SHIFT_15
        enumerator :: SHIFT_16
        enumerator :: SHIFT_17
        enumerator :: SHIFT_18
        enumerator :: SHIFT_20
        enumerator :: SHIFT_28
        enumerator :: SHIFT_29
        enumerator :: GOTO_1
        enumerator :: GOTO_2
        enumerator :: GOTO_3
        enumerator :: GOTO_4
        enumerator :: GOTO_6
        enumerator :: GOTO_19
        enumerator :: GOTO_21
        enumerator :: GOTO_22
        enumerator :: GOTO_23
        enumerator :: GOTO_24
        enumerator :: GOTO_25
        enumerator :: GOTO_26
        enumerator :: GOTO_27
        enumerator :: REJECT
        enumerator :: ACCEPT
    end enum

    integer, parameter :: action_type = KIND(REDUCE_1)
end module action_mod