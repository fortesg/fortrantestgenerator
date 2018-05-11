MODULE sub

USE types
USE globals, ONLY : set, get

IMPLICIT NONE

PRIVATE

INTEGER, PARAMETER :: base = 23
REAL :: number = 0

PUBLIC :: testsub

CONTAINS

SUBROUTINE testsub(ra, rlog, oreal, oa)

  TYPE(testa), INTENT(inout) :: ra
  LOGICAL, INTENT(in) :: rlog
  REAL, INTENT(out), OPTIONAL :: oreal
  TYPE(testa), INTENT(inout), OPTIONAL :: oa

  IF (rlog) THEN
    ra%c%r2(:,:) = ra%b(1)%i2 * ra%b(1)%i0 + ra%c%r2
  END IF

  IF (PRESENT(oa)) THEN
    oa%c%r2(:,:) = oa%b(1)%i2 * oa%b(1)%i0 + oa%c%r2
  END IF

  CALL set(ra%c%r2(1,1))
  IF (PRESENT(oreal)) THEN
    oreal = get()
  END IF

END SUBROUTINE testsub

END MODULE sub
