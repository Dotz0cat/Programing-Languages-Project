submodule (AbstractShape_mod) Circle_sub_mod
    implicit none

    type, extends(AbstractShape) :: Circle
        real :: radius
    contains
        procedure :: area => circle_area
    end type Circle

contains
    module function circle_area(this) result(a)
        class(Circle), intent(inout) :: this
        real :: a

        real, save :: PI = 4.0 * atan(1.0)

        a = PI * this%radius ** 2
    end function circle_area
end submodule Circle_sub_mod

