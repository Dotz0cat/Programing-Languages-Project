submodule (input) input_imp
    implicit none
contains
    module procedure get_number
        print '(A$)', "Enter a number: "
        read (*, *) num
    end procedure get_number

    module procedure get_operation
        print *, "1. add"
        print *, "2. subtract"
        print *, "3. multiply"
        print *, "4. divide"
        print '(A$)', "Choose operation: "
        read (*, *) operation
    end procedure get_operation
end submodule input_imp
