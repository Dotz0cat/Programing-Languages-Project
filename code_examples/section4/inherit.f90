type, extends(Shape) :: Circle
    real :: radius
contains
    procedure :: compute_area => circle_area
end type Circle

subroutine circle_area(this)
    class(Circle), intent(inout) :: this
    real, save :: PI = 4.0 * atan(1.0)

    this%area = PI * this%radius ** 2
end subroutine circle_area

