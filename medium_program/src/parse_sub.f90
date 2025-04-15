submodule (parse_mod) parse_sub
    use token_mod
    use stack_mod
    use terminal_mod
    use state_mod
    use action_mod
    use ast_mod
    implicit none
    
    ! beware there are dragons

    type :: lr_table
        integer(action_type), dimension(20) :: action_table
    end type

    type(lr_table), dimension(32) :: parse_table = &
    & [ lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, & 
    &               SHIFT_10, SHIFT_13, SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, &
    &               GOTO_2, GOTO_4, GOTO_6, GOTO_1, GOTO_3] ), &
    &   lr_table( [ ACCEPT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_1, REJECT, REJECT, REJECT, REJECT, SHIFT_13, SHIFT_15, REJECT, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, &
    &               REJECT ] ), &
    &   lr_table( [ REDUCE_4, SHIFT_18, REJECT, REDUCE_4, SHIFT_16, REDUCE_4, REDUCE_4, SHIFT_17, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, &
    &               REDUCE_5, REDUCE_5, REDUCE_5, REDUCE_5, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, REJECT, GOTO_19, GOTO_6, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_20, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, GOTO_21, GOTO_4, GOTO_6, REJECT, GOTO_3 ] ), &
    &   lr_table( [ REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, &
    &               REDUCE_6, REDUCE_6, REDUCE_6, REDUCE_6, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, &
    &               REDUCE_7, REDUCE_7, REDUCE_7, REDUCE_7, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, &
    &               REDUCE_8, REDUCE_8, REDUCE_8, REDUCE_8, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, &
    &               REDUCE_9, REDUCE_9, REDUCE_9, REDUCE_9, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, &
    &               REDUCE_10, REDUCE_10, REDUCE_10, REDUCE_10, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, &
    &               REDUCE_11, REDUCE_11, REDUCE_11, REDUCE_11, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, REJECT, GOTO_4, GOTO_6, REJECT, GOTO_22 ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, REJECT, GOTO_4, GOTO_6, REJECT, GOTO_23 ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, REJECT, GOTO_24, GOTO_6, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, REJECT, GOTO_25, GOTO_6, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, REJECT, GOTO_26, GOTO_6, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, &
    &               REDUCE_12, REDUCE_12, REDUCE_12, REDUCE_12, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, SHIFT_7, REJECT, REJECT, REJECT, SHIFT_5, REJECT, SHIFT_10, SHIFT_13, &
    &               SHIFT_8, SHIFT_9, SHIFT_12, SHIFT_11, REJECT, GOTO_27, GOTO_4, GOTO_6, REJECT, GOTO_3 ] ), &
    &   lr_table( [ REJECT, REJECT, REJECT, SHIFT_28, REJECT, SHIFT_14, SHIFT_15, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_2, SHIFT_18, REJECT, REDUCE_2, SHIFT_16, REDUCE_2, REDUCE_2, SHIFT_17, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_2, SHIFT_18, REJECT, REDUCE_3, SHIFT_16, REDUCE_3, REDUCE_3, SHIFT_17, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, &
    &               REDUCE_13, REDUCE_13, REDUCE_13, REDUCE_13, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, &
    &               REDUCE_14, REDUCE_14, REDUCE_14, REDUCE_14, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, &
    &               REDUCE_15, REDUCE_15, REDUCE_15, REDUCE_15, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, REJECT, SHIFT_29, REJECT, SHIFT_14, SHIFT_15, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, &
    &               REDUCE_16, REDUCE_16, REDUCE_16, REDUCE_16, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, &
    &               REDUCE_17, REDUCE_17, REDUCE_17, REDUCE_17, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ), &
    &   lr_table( [ REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, &
    &               REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT, REJECT ] ) &
    & ]

    type :: goto_table
        integer(state_type), dimension(20) :: goto_states
    end type goto_table

    type(goto_table), dimension(32) :: goto_state_table = &
    & [ goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, & 
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_2, STATE_4, STATE_6, STATE_1, STATE_3] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_19, STATE_6, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_21, STATE_4, STATE_6, STATE_REJECT, STATE_3 ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_4, STATE_6, STATE_REJECT, STATE_22 ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_4, STATE_6, STATE_REJECT, STATE_23 ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_24, STATE_6, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_25, STATE_6, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_26, STATE_6, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_27, STATE_4, STATE_6, STATE_REJECT, STATE_3 ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ), &
    &   goto_table( [ STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, &
    &               STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT, STATE_REJECT ] ) &
    & ]

contains
    module procedure set_input_stack
        this%input_token_stack = input_stack
        allocate(this%working_token_stack)
        allocate(this%state_stack)
        allocate(this%ast)
    end procedure set_input_stack

    module procedure parse_tokens
        type(token), allocatable :: peek_tok
        integer(state_type) :: state
        integer(term_type) :: la_tok
        integer(action_type) :: action
        
        state = STATE_0
        call this%state_stack%push('I0', state)

        call this%input_token_stack%reverse()

        do
            if (this%input_token_stack%is_empty()) exit
            if (state .eq. STATE_REJECT .OR. state .eq. STATE_ACCEPT) exit
            call this%input_token_stack%peek(peek_tok)
            la_tok = token_to_term(peek_tok%meaning)
            deallocate(peek_tok)
            action = lookup_action(la_tok, state)
            call this%do_action(state, action)
        end do

        if (state .eq. STATE_ACCEPT) then
            this%accepted = .true.
        end if

    end procedure parse_tokens

    pure module function token_to_term(tok) result(value)
        integer(token_type), intent(in) :: tok
        integer(term_type) :: value

        select case (tok)
            case (END)
                value = TERMINAL_END
            case (MODULO)
                value = TERMINAL_MODULO
            case (OPEN)
                value = TERMINAL_OPEN
            case (CLOSE)
                value = TERMINAL_CLOSE
            case (TIMES)
                value = TERMINAL_TIMES
            case (PLUS)
                value = TERMINAL_PLUS
            case (MINUS)
                value = TERMINAL_MINUS
            case (DIVIDE)
                value = TERMINAL_DIVIDE
            case (COS)
                value = TERMINAL_COS
            case (LOG)
                value = TERMINAL_LOG
            case (NUMBER)
                value = TERMINAL_NUMBER
            case (SIN)
                value = TERMINAL_SIN
            case (SQRT)
                value = TERMINAL_SQRT
            case (TAN)
                value = TERMINAL_TAN
            case (FUNCTION, OPERATOR)
                value = NON_TERMINAL_error
            case default
                value = NON_TERMINAL_error
        end select
    end function token_to_term

    pure function lookup_action(token, state) result(action)
        integer(term_type), intent(in) :: token
        integer(state_type), intent(in) :: state
        integer(action_type) :: action

        action = parse_table(state)%action_table(token)
    end function lookup_action

    pure function lookup_goto(token, state) result(new_state)
        integer(term_type), intent(in) :: token
        integer(state_type), intent(in) :: state
        integer(state_type) :: new_state

        new_state = goto_state_table(state)%goto_states(token)
    end function lookup_goto

    module subroutine do_action(this, state, action)
        class(parse), intent(inout) :: this
        integer(state_type), intent(inout) :: state
        integer(action_type), intent(in) :: action

        integer(state_type) :: new_state
        integer(state_type) :: former_state

        class(token), allocatable :: tok
        class(ast_node_base), pointer :: ast_node
        class(ast_node_base), pointer :: ast_node_2

        select case (action)
            case (REDUCE_1)
                ! pop expr reduce to stmt
                allocate(ast_node_stmt :: ast_node)
                
                ! expr
                call this%working_token_stack%pop(tok)
                select type (st => ast_node)
                    class is (ast_node_stmt)
                        select type (e => tok%value)
                            class is (ast_node_expr)
                                st%expr => e
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_stmt
                call this%working_token_stack%push(tok)
                deallocate(tok)
                
                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_stmt, former_state)
                state = new_state

                call this%state_stack%push('R1', new_state)
            case (REDUCE_2)
                ! pop expr PLUS term reduce to expr
                allocate(ast_node_plus :: ast_node)

                ! term
                call this%working_token_stack%pop(tok)
                select type(ast_node)
                    class is (ast_node_plus)
                        select type (t => tok%value)
                            class is (ast_node_term)
                                ast_node%right => t
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! PLUS
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! expr
                call this%working_token_stack%pop(tok)
                select type(ast_node)
                    class is (ast_node_plus)
                        select type (e => tok%value)
                            class is (ast_node_expr)
                                ast_node%left => e
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_expr
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_expr, former_state)
                state = new_state

                call this%state_stack%push('R2', new_state)
            case (REDUCE_3)
                ! pop expr MINUS term reduce to expr
                allocate(ast_node_minus :: ast_node)

                ! term
                call this%working_token_stack%pop(tok)
                select type(ast_node)
                    class is (ast_node_minus)
                        select type (t => tok%value)
                            class is (ast_node_term)
                                ast_node%right => t
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! MINUS
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! expr
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_minus)
                        select type (e => tok%value)
                            class is (ast_node_expr)
                                ast_node%left => e
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_expr
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_expr, former_state)
                state = new_state

                call this%state_stack%push('R3', new_state)
            case (REDUCE_4)
                ! pop term reduce to expr
                allocate(ast_node_expr :: ast_node)

                ! term
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_expr)
                        select type (t => tok%value)
                            class is (ast_node_term)
                                ast_node%expr => t
                        end select
                end select
                deallocate(tok)
                
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_expr
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_expr, former_state)
                state = new_state

                call this%state_stack%push('R4', new_state)
            case (REDUCE_5)
                ! pop factor reduce to term
                allocate(ast_node_term :: ast_node)

                ! factor
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_term)
                        select type (f => tok%value)
                            class is (ast_node_factor)
                                ast_node%factor => f
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_term
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_term, former_state)
                state = new_state

                call this%state_stack%push('P5', new_state)
            case (REDUCE_6)
                ! pop NUMBER reduce to factor
                allocate(ast_node_factor :: ast_node)

                ! NUMBER
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_factor)
                        select type (n => tok%value)
                            class is (ast_node_number)
                                ast_node%expr => n
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_factor
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_factor, former_state)
                state = new_state

                call this%state_stack%push('P6', new_state)
            case (REDUCE_7)
                ! pop SIN reduce to funct
                allocate(ast_node_funct :: ast_node)

                ! SIN
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                select type (ast_node)
                    class is (ast_node_funct)
                        ast_node%funct = funct_SIN
                end select

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_funct
                call this%working_token_stack%push(tok)
                deallocate(tok)
                
                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_funct, former_state)
                state = new_state

                call this%state_stack%push('P7', new_state)
            case (REDUCE_8)
                ! pop COS reduce to funct
                allocate(ast_node_funct :: ast_node)

                ! COS
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                select type (ast_node)
                    class is (ast_node_funct)
                        ast_node%funct = funct_COS
                end select

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_funct
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_funct, former_state)
                state = new_state

                call this%state_stack%push('P8', new_state)
            case (REDUCE_9)
                ! pop TAN reduce to funct
                allocate(ast_node_funct :: ast_node)

                ! TAN
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                select type (ast_node)
                    class is (ast_node_funct)
                        ast_node%funct = funct_TAN
                end select

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_funct
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_funct, former_state)
                state = new_state

                call this%state_stack%push('P9', new_state)
            case (REDUCE_10)
                ! pop SQRT reduce to funct
                allocate(ast_node_funct :: ast_node)

                ! SQRT
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                select type (ast_node)
                    class is (ast_node_funct)
                        ast_node%funct = funct_SQRT
                end select

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_funct
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_funct, former_state)
                state = new_state

                call this%state_stack%push('P10', new_state)
            case (REDUCE_11)
                ! pop LOG reduce to term
                allocate(ast_node_funct :: ast_node)

                ! LOG
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                select type (ast_node)
                    class is (ast_node_funct)
                        ast_node%funct = funct_LOG
                end select

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_funct
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_funct, former_state)
                state = new_state

                call this%state_stack%push('P11', new_state)
            case (REDUCE_12)
                ! pop MINUS factor reduce to factor
                allocate(ast_node_uniminus :: ast_node)

                ! factor
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_uniminus)
                        select type (f => tok%value)
                            class is (ast_node_factor)
                                ast_node%value => f
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! MINUS
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! uniminus is a factor
                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_factor
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_factor, former_state)
                state = new_state

                call this%state_stack%push('P12', new_state)
            case (REDUCE_13)
                ! pop term TIMES factor reduce to term
                allocate(ast_node_times :: ast_node)

                ! factor
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_times)
                        select type(f => tok%value)
                            class is (ast_node_factor)
                                ast_node%right => f
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! times
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! term
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_times)
                        select type(t => tok%value)
                            class is (ast_node_term)
                                ast_node%left => t
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! ast_node_times is a term
                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_term
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_term, former_state)
                state = new_state

                call this%state_stack%push('P13', new_state)
            case (REDUCE_14)
                ! pop term DIVIDE factor reduce to term
                allocate(ast_node_divide :: ast_node)

                ! factor
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_divide)
                        select type(f => tok%value)
                            class is (ast_node_factor)
                                ast_node%right => f
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! DIVIDE
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! term
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_divide)
                        select type(t => tok%value)
                            class is (ast_node_term)
                                ast_node%left => t
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! ast_node_divide is a term
                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_term
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_term, former_state)
                state = new_state

                call this%state_stack%push('P14', new_state)
            case (REDUCE_15)
                ! pop term MODULO factor reduce to term
                allocate(ast_node_modulo :: ast_node)

                ! factor
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_modulo)
                        select type(f => tok%value)
                            class is (ast_node_factor)
                                ast_node%right => f
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! MODULO
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! term
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_modulo)
                        select type(t => tok%value)
                            class is (ast_node_term)
                                ast_node%left => t
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! ast_node_modulo is a term
                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_term
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_term, former_state)
                state = new_state

                call this%state_stack%push('P15', new_state)
            case (REDUCE_16)
                ! pop OPEN expr CLOSE reduce to factor
                allocate(ast_node_factor :: ast_node)

                ! CLOSE
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! expr
                call this%working_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_factor)
                        select type (e => tok%value)
                            class is (ast_node_expr)
                                ast_node%expr => e
                        end select
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! OPEN
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                allocate(tok)
                tok%value => ast_node
                tok%meaning = NON_TERMINAL_factor
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_factor, former_state)
                state = new_state

                call this%state_stack%push('P16', new_state)
            case (REDUCE_17)
                ! pop funct OPEN expr CLOSE reduce to factor

                ! CLOSE
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! expr
                call this%working_token_stack%pop(tok)
                select type (e => tok%value)
                    class is (ast_node_expr)
                        ast_node => e
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)
                
                ! OPEN
                call this%working_token_stack%pop(tok)
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                ! funct
                call this%working_token_stack%pop(tok)
                select type (func => tok%value)
                    class is (ast_node_funct)
                        ast_node_2 => func 
                end select
                deallocate(tok)
                call this%state_stack%pop(tok)
                deallocate(tok)

                select type (ast_node_2)
                    class is (ast_node_funct)
                        select type (ast_node)
                            class is (ast_node_expr)
                                ast_node_2%inner => ast_node
                        end select
                end select

                ! funct is a factor
                allocate(tok)
                tok%value => ast_node_2
                tok%meaning = NON_TERMINAL_factor
                call this%working_token_stack%push(tok)
                deallocate(tok)

                call this%state_stack%peek(tok)
                former_state = tok%meaning
                deallocate(tok)

                new_state = lookup_goto(NON_TERMINAL_factor, former_state)
                state = new_state

                call this%state_stack%push('P17', new_state)
            case (SHIFT_5)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)

                state = STATE_5
                call this%state_stack%push('5', state)
            case (SHIFT_7)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)

                state = STATE_7
                call this%state_stack%push('7', state)
            case (SHIFT_8)
                ! SHIFT NUMBER
                allocate(ast_node_number :: ast_node)
                call this%input_token_stack%pop(tok)
                select type (ast_node)
                    class is (ast_node_number)
                        select type (str => tok%value)
                            type is (character(*))
                                read(str, *) ast_node%number
                        end select
                end select
                deallocate(tok)
                
                allocate(tok)
                tok%value => ast_node
                tok%meaning = TERMINAL_NUMBER
                call this%working_token_stack%push(tok)
                deallocate(tok)

                state = STATE_8
                call this%state_stack%push('8', state)
            case (SHIFT_9)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_9
                call this%state_stack%push('9', state)
            case (SHIFT_10)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_10
                call this%state_stack%push('10', state)
            case (SHIFT_11)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_11
                call this%state_stack%push('11', state)
            case (SHIFT_12)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_12
                call this%state_stack%push('12', state)
            case (SHIFT_13)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_13
                call this%state_stack%push('13', state)
            case (SHIFT_14)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_14
                call this%state_stack%push('14', state)
            case (SHIFT_15)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_15
                call this%state_stack%push('15', state)
            case (SHIFT_16)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_16
                call this%state_stack%push('16', state)
            case (SHIFT_17)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_17
                call this%state_stack%push('17', state)
            case (SHIFT_18)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_18
                call this%state_stack%push('18', state)
            case (SHIFT_20)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_20
                call this%state_stack%push('20', state)
            case (SHIFT_28)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_28
                call this%state_stack%push('28', state)
            case (SHIFT_29)
                call this%input_token_stack%pop(tok)
                call this%working_token_stack%push(tok)
                deallocate(tok)
                state = STATE_29
                call this%state_stack%push('29', state)
            case (GOTO_1)
                state = STATE_1
                call this%state_stack%push('1', state)
            case (GOTO_2)
                state = STATE_2
                call this%state_stack%push('2', state)
            case (GOTO_3)
                state = STATE_3
                call this%state_stack%push('3', state)
            case (GOTO_4)
                state = STATE_4
                call this%state_stack%push('4', state)
            case (GOTO_6)
                state = STATE_6
                call this%state_stack%push('6', state)
            case (GOTO_19)
                state = STATE_19
                call this%state_stack%push('19', state)
            case (GOTO_21)
                state = STATE_21
                call this%state_stack%push('21', state)
            case (GOTO_22)
                state = STATE_22
                call this%state_stack%push('22', state)
            case (GOTO_23)
                state = STATE_23
                call this%state_stack%push('23', state)
            case (GOTO_24)
                state = STATE_24
                call this%state_stack%push('24', state)
            case (GOTO_25)
                state = STATE_25
                call this%state_stack%push('25', state)
            case (GOTO_26)
                state = STATE_26
                call this%state_stack%push('26', state)
            case (GOTO_27)
                state = STATE_27
                call this%state_stack%push('27', state)
            case (REJECT)

                state = STATE_REJECT
            case (ACCEPT)
                ! pop stmt assign to ast
                call this%working_token_stack%pop(tok)
                select type (st => tok%value)
                    class is (ast_node_stmt)
                        this%ast%root => st
                end select

                state = STATE_ACCEPT
            case default
                print *, 'unknown error'
                state = STATE_REJECT
        end select
    end subroutine do_action

    module procedure perform_evaluation
        value = this%ast%root%eval()
    end procedure perform_evaluation

    module procedure free_parse
        deallocate(this%input_token_stack)
        deallocate(this%working_token_stack)
        deallocate(this%state_stack)
        deallocate(this%ast)
    end procedure free_parse

    module procedure parse_success
            parse_success = this%accepted
    end procedure parse_success

end submodule parse_sub
