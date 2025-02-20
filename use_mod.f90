program use_mod
    use numbers
    implicit none

    integer :: total
    integer :: num1
    integer :: num2

    print *, "Enter a number"
    read (*, *) num1
    print *, "Enter another number"
    read (*, *) num2

    total = add_numbers(num1, num2)

    print *, "The total is ", total
end program use_mod
