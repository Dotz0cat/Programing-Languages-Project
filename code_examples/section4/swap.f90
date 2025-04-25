subroutine swap(a, b)
    real, intent(inout) :: a, b
    real :: temp

    temp = a
    a = b
    b = temp
end subroutine swap

