module ast_mod
    implicit none
    private
    public :: ast, &
            & ast_node_base,        ast_node_expr,        ast_node_stmt,       &
            & ast_node_factor,      ast_node_term,        ast_node_number,     &
            & ast_node_funct,       ast_node_plus,        ast_node_minus,      &
            & ast_node_times,       ast_node_divide,      ast_node_modulo,     &
            & ast_node_uniminus

    public :: funct_SIN, funct_COS, funct_TAN, funct_SQRT, funct_LOG, funct_type

    enum, bind(c)
        enumerator :: funct_SIN
        enumerator :: funct_COS
        enumerator :: funct_TAN
        enumerator :: funct_SQRT
        enumerator :: funct_LOG
    end enum

    integer, parameter :: funct_type = KIND(funct_LOG)

    !-------------------------------------------------------------------
    ! Base class for all AST nodes
    !-------------------------------------------------------------------
    type, abstract, public :: ast_node_base
    contains
        procedure(eval), public, deferred :: eval
    end type ast_node_base

    !-------------------------------------------------------------------
    ! Abstract interface for the deferred eval()
    !-------------------------------------------------------------------
    abstract interface
        module function eval(this)
            class(ast_node_base), intent(inout) :: this
            real :: eval
        end function eval
    end interface

    !-------------------------------------------------------------------
    ! Top‐level AST container
    !-------------------------------------------------------------------
    type, public :: ast
        class(ast_node_base), pointer :: root => null()
    contains
        final :: free_ast
    end type ast

    !-------------------------------------------------------------------
    ! Expression node (unary, plus/minus)
    !-------------------------------------------------------------------
    type, public, extends(ast_node_base) :: ast_node_expr
        class(ast_node_base), pointer :: expr => null()
    contains
        procedure, public :: eval => eval_expr
        final :: free_ast_node_expr
    end type ast_node_expr

    !-------------------------------------------------------------------
    ! Statement node (e.g. expression‐statement)
    !-------------------------------------------------------------------
    type, public, extends(ast_node_base) :: ast_node_stmt
        class(ast_node_expr), pointer :: expr => null()
    contains
        procedure, public :: eval => eval_stmt
        final :: free_ast_node_stmt
    end type ast_node_stmt

    !-------------------------------------------------------------------
    ! Factor node (parentheses, numbers, functions, unary minus)
    !-------------------------------------------------------------------
    type, public, extends(ast_node_base) :: ast_node_factor
        class(ast_node_base), pointer :: expr => null()
    contains
        procedure, public :: eval => eval_factor
        final :: free_ast_node_factor
    end type ast_node_factor

    !-------------------------------------------------------------------
    ! Term node (products, division, modulo)
    !-------------------------------------------------------------------
    type, public, extends(ast_node_base) :: ast_node_term
        class(ast_node_factor), pointer :: factor => null()
    contains
        procedure, public :: eval => eval_term
        final :: free_ast_node_term
    end type ast_node_term

    !-------------------------------------------------------------------
    ! Numeric literal
    !-------------------------------------------------------------------
    type, public, extends(ast_node_factor) :: ast_node_number
        real :: number = 0.0
    contains
        procedure, public :: eval => eval_number
    end type ast_node_number

    !-------------------------------------------------------------------
    ! Function call node
    !-------------------------------------------------------------------
    type, public, extends(ast_node_factor) :: ast_node_funct
        integer(funct_type) :: funct
        class(ast_node_expr), pointer :: inner => null()
    contains
        procedure, public :: eval => eval_funct
        final :: free_ast_node_funct
    end type ast_node_funct

    !-------------------------------------------------------------------
    ! Binary plus
    !-------------------------------------------------------------------
    type, public, extends(ast_node_expr) :: ast_node_plus
        class(ast_node_expr), pointer :: left  => null()
        class(ast_node_term), pointer :: right => null()
    contains
        procedure, public :: eval => eval_plus
        final :: free_ast_node_plus
    end type ast_node_plus

    !-------------------------------------------------------------------
    ! Binary minus
    !-------------------------------------------------------------------
    type, public, extends(ast_node_expr) :: ast_node_minus
        class(ast_node_expr), pointer :: left  => null()
        class(ast_node_term), pointer :: right => null()
    contains
        procedure, public :: eval => eval_minus
        final :: free_ast_node_minus
    end type ast_node_minus

    !-------------------------------------------------------------------
    ! Multiplication
    !-------------------------------------------------------------------
    type, public, extends(ast_node_term) :: ast_node_times
        class(ast_node_term),   pointer :: left  => null()
        class(ast_node_factor), pointer :: right => null()
    contains
        procedure, public :: eval => eval_times
        final :: free_ast_node_times
    end type ast_node_times

    !-------------------------------------------------------------------
    ! Division
    !-------------------------------------------------------------------
    type, public, extends(ast_node_term) :: ast_node_divide
        class(ast_node_term),   pointer :: left  => null()
        class(ast_node_factor), pointer :: right => null()
    contains
        procedure, public :: eval => eval_divide
        final :: free_ast_node_divide
    end type ast_node_divide

    !-------------------------------------------------------------------
    ! Modulo
    !-------------------------------------------------------------------
    type, public, extends(ast_node_term) :: ast_node_modulo
        class(ast_node_term),   pointer :: left  => null()
        class(ast_node_factor), pointer :: right => null()
    contains
        procedure, public :: eval => eval_modulo
        final :: free_ast_node_modulo
    end type ast_node_modulo

    !-------------------------------------------------------------------
    ! Unary minus
    !-------------------------------------------------------------------
    type, public, extends(ast_node_factor) :: ast_node_uniminus
        class(ast_node_factor), pointer :: value => null()
    contains
        procedure, public :: eval => eval_uniminus
        final :: free_ast_node_uniminus
    end type ast_node_uniminus

    !-------------------------------------------------------------------
    ! Interfaces for each concrete eval
    !-------------------------------------------------------------------
    interface eval_expr
        recursive module function eval_expr(this)
            class(ast_node_expr), intent(inout) :: this
            real :: eval_expr
        end function eval_expr
    end interface eval_expr

    interface eval_stmt
        module function eval_stmt(this)
            class(ast_node_stmt), intent(inout) :: this
            real :: eval_stmt
        end function eval_stmt
    end interface eval_stmt

    interface eval_factor
        module function eval_factor(this)
            class(ast_node_factor), intent(inout) :: this
            real :: eval_factor
        end function eval_factor
    end interface eval_factor

    interface eval_term
        module function eval_term(this)
            class(ast_node_term), intent(inout) :: this
            real :: eval_term
        end function eval_term
    end interface eval_term

    interface eval_number
        module function eval_number(this)
            class(ast_node_number), intent(inout) :: this
            real :: eval_number
        end function eval_number
    end interface eval_number

    interface eval_funct
        module function eval_funct(this)
            class(ast_node_funct), intent(inout) :: this
            real :: eval_funct
        end function eval_funct
    end interface eval_funct

    interface eval_plus
        module function eval_plus(this)
            class(ast_node_plus), intent(inout) :: this
            real :: eval_plus
        end function eval_plus
    end interface eval_plus

    interface eval_minus
        module function eval_minus(this)
            class(ast_node_minus), intent(inout) :: this
            real :: eval_minus
        end function eval_minus
    end interface eval_minus

    interface eval_times
        module function eval_times(this)
            class(ast_node_times), intent(inout) :: this
            real :: eval_times
        end function eval_times
    end interface eval_times

    interface eval_divide
        module function eval_divide(this)
            class(ast_node_divide), intent(inout) :: this
            real :: eval_divide
        end function eval_divide
    end interface eval_divide

    interface eval_modulo
        module function eval_modulo(this)
            class(ast_node_modulo), intent(inout) :: this
            real :: eval_modulo
        end function eval_modulo
    end interface eval_modulo

    interface eval_uniminus
        module function eval_uniminus(this)
            class(ast_node_uniminus), intent(inout) :: this
            real :: eval_uniminus
        end function eval_uniminus
    end interface eval_uniminus

    interface free_ast
        module subroutine free_ast(this)
            type(ast), intent(inout) :: this
        end subroutine free_ast
    end interface free_ast

    interface free_ast_node_expr
        module subroutine free_ast_node_expr(this)
            type(ast_node_expr), intent(inout) :: this
        end subroutine free_ast_node_expr
    end interface free_ast_node_expr

    interface free_ast_node_stmt
        module subroutine free_ast_node_stmt(this)
            type(ast_node_stmt), intent(inout) :: this
        end subroutine free_ast_node_stmt
    end interface free_ast_node_stmt

    interface free_ast_node_factor
        module subroutine free_ast_node_factor(this)
            type(ast_node_factor), intent(inout) :: this
        end subroutine free_ast_node_factor
    end interface free_ast_node_factor

    interface free_ast_node_term
        module subroutine free_ast_node_term(this)
            type(ast_node_term), intent(inout) :: this
        end subroutine free_ast_node_term
    end interface free_ast_node_term

    interface free_ast_node_funct
        module subroutine free_ast_node_funct(this)
            type(ast_node_funct), intent(inout) :: this
        end subroutine free_ast_node_funct
    end interface free_ast_node_funct

    interface free_ast_node_plus
        module subroutine free_ast_node_plus(this)
            type(ast_node_plus), intent(inout) :: this
        end subroutine free_ast_node_plus
    end interface free_ast_node_plus

    interface free_ast_node_minus
        module subroutine free_ast_node_minus(this)
            type(ast_node_minus), intent(inout) :: this
        end subroutine free_ast_node_minus
    end interface free_ast_node_minus

    interface free_ast_node_times
        module subroutine free_ast_node_times(this)
            type(ast_node_times), intent(inout) :: this
        end subroutine free_ast_node_times
    end interface free_ast_node_times

    interface free_ast_node_divide
        module subroutine free_ast_node_divide(this)
            type(ast_node_divide), intent(inout) :: this
        end subroutine free_ast_node_divide
    end interface free_ast_node_divide

    interface free_ast_node_modulo
        module subroutine free_ast_node_modulo(this)
            type(ast_node_modulo), intent(inout) :: this
        end subroutine free_ast_node_modulo
    end interface free_ast_node_modulo

    interface free_ast_node_uniminus
        module subroutine free_ast_node_uniminus(this)
            type(ast_node_uniminus), intent(inout) :: this
        end subroutine free_ast_node_uniminus
    end interface free_ast_node_uniminus

end module ast_mod
