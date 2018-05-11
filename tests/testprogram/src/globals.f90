MODULE globals

PRIVATE

INTEGER, PARAMETER :: base = 23
REAL :: number = 0

PUBLIC :: set, get

CONTAINS

SUBROUTINE set(new)
  REAL, INTENT(in) :: new
  number = number + base + new
END SUBROUTINE set

REAL FUNCTION get()
  get = number
END FUNCTION get

END MODULE globals
