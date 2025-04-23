module stack_mod
  use token_mod, only: token_type 
  implicit none

  private

  public :: stack_push
  public :: stack_pop
  public :: stack_peek
  public :: stack_token_push
  public :: stack_token_pop
  public :: stack_token_peek
  public :: stack_print
  public :: stack_reverse
  public :: stack_is_empty
  
  public :: node_push
  public :: node_pop
  public :: node_peek
  public :: node_token_push
  public :: node_token_pop
  public :: node_token_peek
  public :: node_print
  public :: get_next
  public :: get_prev

  type, public :: stack
    private
      class(node), pointer :: head => null()
  contains
    procedure, private :: stack_push
    procedure, private :: stack_token_push
    procedure, private :: stack_pop
    procedure, private :: stack_token_pop
    procedure, private :: stack_peek
    procedure, private :: stack_token_peek
    generic, public :: push => stack_push, stack_token_push
    generic, public :: pop => stack_pop, stack_token_pop
    generic, public :: peek => stack_peek, stack_token_peek
    procedure, public :: print => stack_print
    procedure, public :: reverse => stack_reverse
    procedure, public :: is_empty => stack_is_empty
    final :: stack_free
  end type

  type, public :: node
    private
    class(node), pointer :: next => null()
    class(node), pointer :: prev => null()
    !class(*), allocatable :: value
    !integer(token_type) :: meaning
    class(token), allocatable :: token
  contains
    procedure, private :: node_push
    procedure, private :: node_token_push
    procedure, private :: node_pop
    procedure, private :: node_token_pop
    procedure, private :: node_peek
    procedure, private :: node_token_peek
    generic, public :: push => node_push, node_token_push
    generic, public :: pop => node_pop, node_token_pop
    generic, public :: peek => node_peek, node_token_peek
    procedure, public :: print => node_print
    procedure, public :: get_next
    procedure, public :: get_prev
    final :: node_free
  end type

  type, public :: token
    class(*), pointer :: value
    integer(token_type) :: meaning
  contains
    final :: free_token
  end type token

  interface stack_push
    module subroutine stack_push(this, i, meaning)
      class(stack), intent(inout) :: this
      character(*), intent(in) :: i
      integer(token_type), intent(in) :: meaning
    end subroutine stack_push
  end interface stack_push

  interface stack_token_push
    module subroutine stack_token_push(this, tok)
      class(stack), intent(inout) :: this
      class(token), intent(in), allocatable :: tok
    end subroutine stack_token_push
  end interface stack_token_push
  
  interface node_push
    module subroutine node_push(this, i, meaning)
      class(node), intent(inout), target :: this
      character(*), intent(in) :: i
      integer(token_type), intent(in) :: meaning
    end subroutine node_push
  end interface node_push

  interface node_token_push
    module subroutine node_token_push(this, tok)
      class(node), intent(inout), target :: this
      class(token), intent(in), allocatable :: tok
    end subroutine node_token_push
  end interface node_token_push

  interface stack_pop
    module subroutine stack_pop(this, i, meaning)
      class(stack), intent(inout) :: this
      character(*), intent(out), allocatable :: i
      integer(token_type), intent(out) :: meaning
    end subroutine stack_pop
  end interface stack_pop

  interface stack_token_pop
    module subroutine stack_token_pop(this, tok)
      class(stack), intent(inout) :: this
      type(token), intent(out), allocatable :: tok
    end subroutine stack_token_pop
  end interface stack_token_pop

  interface node_pop
    module subroutine node_pop(this, i, meaning)
      class(node), intent(inout) :: this
      character(*), intent(out), allocatable :: i
      integer(token_type), intent(out) :: meaning
    end subroutine node_pop
  end interface node_pop

  interface node_token_pop
    module subroutine node_token_pop(this, tok)
      class(node), intent(inout) :: this
      type(token), intent(out), allocatable :: tok
    end subroutine node_token_pop
  end interface node_token_pop

  interface stack_peek
    module subroutine stack_peek(this, i, meaning)
      class(stack), intent(inout) :: this
      character(*), intent(out) :: i
      integer(token_type), intent(out) :: meaning
    end subroutine stack_peek
  end interface stack_peek

  interface stack_token_peek
    module subroutine stack_token_peek(this, tok)
      class(stack), intent(inout) :: this
      type(token), intent(out), allocatable :: tok
    end subroutine stack_token_peek
  end interface stack_token_peek
  
  interface node_peek
    module subroutine node_peek(this, i, meaning)
      class(node), intent(inout) :: this
      character(*), intent(out) :: i
      integer(token_type), intent(out) :: meaning
    end subroutine node_peek
  end interface node_peek

  interface node_token_peek 
    module subroutine node_token_peek(this, tok)
      class(node), intent(inout) :: this
      type(token), intent(out), allocatable :: tok
    end subroutine node_token_peek
  end interface node_token_peek

  interface stack_print
    module subroutine stack_print(this)
      class(stack), intent(in) :: this
    end subroutine stack_print
  end interface stack_print

  interface node_print
    recursive module subroutine node_print(this)
      class(node), intent(in) :: this
    end subroutine node_print
  end interface node_print

  interface get_next
    module function get_next(this) result (next)
      class(node), intent(in) :: this
      class(node), pointer :: next
    end function get_next
  end interface get_next
  
  interface get_prev
    module function get_prev(this) result(prev) 
      class(node), intent(in) :: this
      class(node), pointer :: prev
    end function get_prev
  end interface get_prev

  interface stack_free
    module subroutine stack_free(this)
      type(stack), intent(inout) :: this
    end subroutine stack_free
  end interface stack_free

  interface node_free
    module subroutine node_free(this)
      type(node), intent(inout) :: this
    end subroutine node_free
  end interface node_free

  interface stack_reverse
    recursive module subroutine stack_reverse(this)
      class(stack), intent(inout) :: this
    end subroutine stack_reverse
  end interface stack_reverse

  interface stack_is_empty
    pure module function stack_is_empty(this)
      class(stack), intent(in) :: this
      logical :: stack_is_empty
    end function stack_is_empty
  end interface stack_is_empty

  interface free_token
    module subroutine free_token(this)
      type(token), intent(inout) :: this
    end subroutine free_token
  end interface free_token

end module stack_mod

