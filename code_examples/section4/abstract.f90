module AbstractShape_mod
    implicit none

    type, abstract :: AbstractShape
    contains
        procedure(calculate_area_iface), deferred :: area
    end type AbstractShape

    abstract interface
        function calcuate_area_iface(this) result(a)
            import :: AbstractShape
            
            class(AbstractShape), intent(inout) :: this
            real :: a
        end function claculate_area_iface
    end interface
end module AbstractShape_mod

