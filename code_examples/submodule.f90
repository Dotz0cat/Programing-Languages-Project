submodule (numbers_p) numbers_sub
    implicit none
contains
    module procedure add_numbers
        total = a + b
    end procedure add_numbers
end submodule numbers_sub

