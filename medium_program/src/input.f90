module input
    implicit none
    
    private

    interface get_type
        module subroutine get_type(type)
            integer, intent(out) :: type
        end subroutine get_type
    end interface get_type

    interface get_number
        module subroutine get_number(num)

        end subroutine get_number
    end interface get_number
end module input
