module input
    implicit none
    
    private

    interface get_number
        module subroutine get_number(num)
            real, intent(out) :: num
        end subroutine get_number
    end interface get_number

    interface get_operation
        module subroutine get_operation(operation)
            integer, intent(out) :: operation
        end subroutine get_operation
    end interface get_operation
end module input
