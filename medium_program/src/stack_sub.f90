submodule (stack_mod) stack_sub
    implicit none
contains
    module procedure stack_push
        class(node), pointer :: new_head
        if (.not. associated(this%head)) then
            allocate(this%head)
            call this%head%push(i, meaning)
            return
        end if
        call this%head%push(i, meaning)
        new_head => this%head%get_next()
        this%head => new_head
    end procedure stack_push
    
    module procedure stack_token_push
        class(node), pointer :: new_head
        if (.not. associated(this%head)) then
            allocate(this%head)
            call this%head%push(tok)
            return
        end if
        call this%head%push(tok)
        new_head => this%head%get_next()
        this%head => new_head
    end procedure stack_token_push

    module procedure node_push
        if (.not. allocated(this%token)) then
            allocate(this%token)
            allocate(this%token%value, source=i)
            this%token%meaning = meaning
            return
        end if
        allocate(this%next)
        this%next%prev => this
        allocate(this%next%token)
        this%next%token%value = i
        this%next%token%meaning = meaning
    end procedure node_push
    
    module procedure node_token_push
        if (.not. allocated(this%token)) then
            !select type ( q => tok%value )
            !    type is (character(*))
            !        this%value = q
            !end select
            !this%meaning = tok%meaning
            allocate(this%token, source=tok)
            !this%token = tok
            return
        end if
        allocate(this%next)
        this%next%prev => this
        !select type ( q => tok%value )
        !    type is (character(*))
        !        this%next%value = q
        !end select
        !this%next%value = tok%value
        !this%next%meaning = tok%meaning

        !this%next%token = tok
        allocate(this%next%token, source=tok)
        !this%next%token = tok
    end procedure node_token_push

    module procedure stack_pop
        class(node), pointer :: former_head
        class(node), pointer :: new_head
        if (.not. associated(this%head)) return
        former_head => this%head
        call this%head%pop(i, meaning)
        new_head => this%head%get_prev()
        this%head => new_head
        deallocate(former_head)
    end procedure stack_pop

    module procedure stack_token_pop
        class(node), pointer :: former_head
        class(node), pointer :: new_head
        if (.not. associated(this%head)) return
        former_head => this%head
        call this%head%pop(tok)
        new_head => this%head%get_prev()
        this%head => new_head
        deallocate(former_head)
    end procedure stack_token_pop

    module procedure node_pop
        !i = this%value
        !meaning = this%meaning

        select type (q => this%token%value)
            type is (character(*))
                i = q
        end select
        meaning = this%token%meaning
        
        if (associated(this%prev)) then
            this%prev%next => this%next
        end if
        if (associated(this%next)) then
            this%next%prev => this%prev
        end if
    end procedure node_pop

    module procedure node_token_pop
        !tok%value = this%token%value
        !tok%meaning = this%token%meaning

        allocate(tok, source=this%token)

        if (associated(this%prev)) then
            this%prev%next => this%next
        end if
        if (associated(this%next)) then
            this%next%prev => this%prev
        end if
    end procedure node_token_pop

    module procedure node_peek
        !i = this%value
        !meaning = this%meaning
        select type (q => this%token%value)
            type is (character(*))
                i = q
        end select
        meaning = this%token%meaning
    end procedure node_peek

    module procedure node_token_peek
        !tok%value = this%value
        !tok%meaning = this%meaning
        allocate(tok, source=this%token)
    end procedure node_token_peek

    module procedure stack_peek
        call this%head%peek(i, meaning)
    end procedure stack_peek

    module procedure stack_token_peek
        call this%head%peek(tok)
    end procedure stack_token_peek

    module procedure stack_print
        if (associated(this%head)) then
            call this%head%print
        end if
    end procedure stack_print

    module procedure node_print
        select type (q => this%token%value)
            type is (character(*))
                if (.not. associated(this%prev)) then
                    print *, 'value=', q, ' token=', this%token%meaning
                else
                    call this%prev%print()
                    print *, 'value=', q, ' token=', this%token%meaning
                end if
            type is (real)
                if (.not. associated(this%prev)) then
                    print *, 'value=', q, ' token=', this%token%meaning
                else
                    call this%prev%print()
                    print *, 'value=', q, ' token=', this%token%meaning
                end if
        end select
    end procedure node_print

    module procedure get_next
        next => this%next
    end procedure get_next

    module procedure get_prev
        prev => this%prev
    end procedure get_prev

    module procedure stack_free
        type(token), allocatable :: tok

        do
            if (.not. associated(this%head)) exit
            call this%pop(tok)
            deallocate(tok)
        end do
    end procedure stack_free

    module procedure node_free
        if (allocated(this%token)) then
            deallocate(this%token)
        end if
    end procedure node_free

    module procedure stack_reverse
        class(token), allocatable :: tok
        type(stack) :: temp_stack

        if (.not. associated(this%head)) return
        
        do
            if (this%is_empty()) exit
            call this%pop(tok)
            call temp_stack%push(tok)
        end do

        this%head => temp_stack%head
        nullify(temp_stack%head)
    end procedure stack_reverse

    module procedure stack_is_empty
        stack_is_empty = .not. associated(this%head)
    end procedure stack_is_empty
end submodule stack_sub

