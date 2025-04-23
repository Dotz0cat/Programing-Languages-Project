module processing
  implicit none

  private

  public :: add_numbers
  public :: subtract_numbers
  public :: multiply_numbers
  public :: divide_numbers

  interface add_numbers
    real module function add_numbers(a, b) result(sum)
      real, intent(in) :: a
      real, intent(in) :: b
    end function add_numbers
  end interface add_numbers

  interface subtract_numbers
    real module function subtract_numbers(a, b) result(diff)
      real, intent(in) :: a
      real, intent(in) :: b
    end function subtract_numbers
  end interface subtract_numbers

  interface multiply_numbers
    real module function multiply_numbers(a, b) result(product)
      real, intent(in) :: a
      real, intent(in) :: b
    end function multiply_numbers
  end interface multiply_numbers

  interface divide_numbers
    real module function divide_numbers(a, b) result(quotient)
      real, intent(in) :: a
      real, intent(in) :: b
    end function divide_numbers
  end interface divide_numbers
end module processing

