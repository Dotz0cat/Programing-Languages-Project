submodule (processing) processing_sub
    implicit none
contains
    module procedure add_numbers
        sum = a + b
    end procedure add_numbers
    
    module procedure subtract_numbers
        diff = a - b
    end procedure subtract_numbers

    module procedure multiply_numbers
        product = a * b
    end procedure multiply_numbers

    module procedure divide_numbers
        if (b .eq. 0) then
            print *, "error: cannot divide by zero"
            quotient = huge(0.0)
            return
        end if
        quotient = a / b
    end procedure divide_numbers
end submodule processing_sub

