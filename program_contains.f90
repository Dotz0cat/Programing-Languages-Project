program container
    implicit none

    integer :: num1
    integer :: num2
    integer :: total

    print *, "Give First Number: "
    read (*, *) num1
    print *, "Give Second Number: "
    read (*, *) num2

    total = add_numbers(num1, num2)

    print *, "The total is: ", total

contains
    integer function add_numbers(a, b) result(answer)
        integer, intent(in) :: a
        integer, intent(in) :: b

        answer = a + b
    end function
end program container

