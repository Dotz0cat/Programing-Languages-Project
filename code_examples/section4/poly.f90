subroutine print_area(shape_obj)
    class(Shape), intent(inout) :: shape_obj

    print *, 'Area= ', shape_obj%area
end subroutine print_area

