recursive function factorial(n) result(res)
    real, intent(in) :: n
    real :: res

    if (n .le. 1) then
        res = 1
    else
        res = n * factorial(n - 1)
    end if
end function factorial

