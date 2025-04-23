module state_mod
  implicit none

  public

  enum, bind(c)
    enumerator :: STATE_PADDING
    enumerator :: STATE_0
    enumerator :: STATE_1
    enumerator :: STATE_2
    enumerator :: STATE_3
    enumerator :: STATE_4
    enumerator :: STATE_5
    enumerator :: STATE_6
    enumerator :: STATE_7
    enumerator :: STATE_8
    enumerator :: STATE_9
    enumerator :: STATE_10
    enumerator :: STATE_11
    enumerator :: STATE_12
    enumerator :: STATE_13
    enumerator :: STATE_14
    enumerator :: STATE_15
    enumerator :: STATE_16
    enumerator :: STATE_17
    enumerator :: STATE_18
    enumerator :: STATE_19
    enumerator :: STATE_20
    enumerator :: STATE_21
    enumerator :: STATE_22
    enumerator :: STATE_23
    enumerator :: STATE_24
    enumerator :: STATE_25
    enumerator :: STATE_26
    enumerator :: STATE_27
    enumerator :: STATE_28
    enumerator :: STATE_29
    enumerator :: STATE_ACCEPT
    enumerator :: STATE_REJECT
  end enum

  integer, parameter :: state_type = KIND(STATE_0)
end module state_mod