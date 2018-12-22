MODULE globals

IMPLICIT NONE

PRIVATE

INTEGER, PARAMETER :: base = 23
REAL :: number = 0

PUBLIC :: set, get


! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

PUBLIC :: number

! ================= END FORTRAN TEST GENERATOR (FTG) =========================

CONTAINS

SUBROUTINE set(new)
  REAL, INTENT(in) :: new
  number = number + base + new
END SUBROUTINE set

REAL FUNCTION get()
  get = number
END FUNCTION get

END MODULE globals
