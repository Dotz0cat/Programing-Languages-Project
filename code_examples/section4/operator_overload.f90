module vector_ops
    implicit none

    type :: Vector
        real :: x, y
    end type Vector

    interface operator(+)
        procedure :: vector_add
    end interface operator(+)
contains
    function vector_add(v1, v2) result(v3)
        type(Vector), intent(in) :: v1, v2
        type(Vector) :: v3

        v3%x = v1%x + v2%x
        v3%y = v1%y + v2%y
    end function vector_add
end module vector_ops

