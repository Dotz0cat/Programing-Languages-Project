submodule (ast_mod) ast_sub
  implicit none

contains

  module procedure eval_stmt
    eval_stmt = this%expr%eval()
  end procedure eval_stmt

  module procedure eval_factor
    eval_factor = this%expr%eval()
  end procedure eval_factor

  module procedure eval_term
    eval_term = this%factor%eval()
  end procedure eval_term

  module procedure eval_number
    eval_number = this%number
  end procedure eval_number

  module procedure eval_expr
    eval_expr = this%expr%eval()
  end procedure eval_expr

  module procedure eval_funct
    select case (this%funct)
      case (funct_SIN)
        eval_funct = SIN(this%inner%eval())
      case (funct_COS)
        eval_funct = COS(this%inner%eval())
      case (funct_TAN)
        eval_funct = TAN(this%inner%eval())
      case (funct_SQRT)
        eval_funct = SQRT(this%inner%eval())
      case (funct_LOG)
        eval_funct = LOG(this%inner%eval())
      case default
        eval_funct = 0.0
    end select
  end procedure eval_funct

  module procedure eval_plus
    eval_plus = this%left%eval() + this%right%eval()
  end procedure eval_plus

  module procedure eval_minus
    eval_minus = this%left%eval() - this%right%eval()
  end procedure eval_minus

  module procedure eval_times
    eval_times = this%left%eval() * this%right%eval()
  end procedure eval_times

  module procedure eval_divide
    eval_divide = this%left%eval() / this%right%eval()
  end procedure eval_divide

  module procedure eval_modulo
    eval_modulo = mod(this%left%eval(), this%right%eval())
  end procedure eval_modulo

  module procedure eval_uniminus
    eval_uniminus = this%value%eval() * (-1.0)
  end procedure eval_uniminus

  module procedure free_ast
    if (associated(this%root)) then
      deallocate(this%root)
    end if
  end procedure free_ast

  module procedure free_ast_node_expr
    if (associated(this%expr)) then
      deallocate(this%expr)
    end if
  end procedure free_ast_node_expr

  module procedure free_ast_node_stmt
    if (associated(this%expr)) then
      deallocate(this%expr)
    end if
  end procedure free_ast_node_stmt

  module procedure free_ast_node_factor
    if (associated(this%expr)) then
      deallocate(this%expr)
    end if
  end procedure free_ast_node_factor

  module procedure free_ast_node_term
    if (associated(this%factor)) then
      deallocate(this%factor)
    end if
  end procedure free_ast_node_term

  module procedure free_ast_node_funct
    if (associated(this%inner)) then
      deallocate(this%inner)
    end if
  end procedure free_ast_node_funct

  module procedure free_ast_node_plus
    if (associated(this%left)) then
      deallocate(this%left)
    end if
    if (associated(this%right)) then
      deallocate(this%right)
    end if
  end procedure free_ast_node_plus

  module procedure free_ast_node_minus
    if (associated(this%left)) then
      deallocate(this%left)
    end if
    if (associated(this%right)) then
      deallocate(this%right)
    end if
  end procedure free_ast_node_minus

  module procedure free_ast_node_times
    if (associated(this%left)) then
      deallocate(this%left)
    end if
    if (associated(this%right)) then
      deallocate(this%right)
    end if
  end procedure free_ast_node_times

  module procedure free_ast_node_divide
    if (associated(this%left)) then
      deallocate(this%left)
    end if
    if (associated(this%right)) then
      deallocate(this%right)
    end if
  end procedure free_ast_node_divide

  module procedure free_ast_node_modulo
    if (associated(this%left)) then
      deallocate(this%left)
    end if
    if (associated(this%right)) then
      deallocate(this%right)
    end if
  end procedure free_ast_node_modulo

  module procedure free_ast_node_uniminus
    if (.not. associated(this%value)) return
    deallocate(this%value)
  end procedure free_ast_node_uniminus
end submodule ast_sub