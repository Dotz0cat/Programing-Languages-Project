module procedure divide_numbers
    if (b .eq. 0) then
        print *, "error: cannot divide by zero"
        quotient = huge(0.0)
        return
    end if
    .
    .
    .
end procedure divide_numbers

