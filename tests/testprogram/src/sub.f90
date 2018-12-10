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
  LOGICAL, INTENT(in) :: rlog(4)
  REAL, INTENT(out), OPTIONAL :: oreal
  TYPE(testa), INTENT(inout), OPTIONAL :: oa
  TYPE(jmodel) :: jm
#ifdef _OPENACC
  LOGICAL :: openacc
#endif

  IF (rlog(1)) THEN
    ra%c%r2(:,:) = ra%b(1)%i2(1:2, 1:2) * ra%b(1)%i0 + ra%c%r2(:,:)
  END IF

  IF (PRESENT(oa)) THEN
    oa%c%r2(:,:) = oa%b(1)%i2(1:2, 1:2) * oa%b(1)%i0 + oa%c%r2(:,:)
  END IF

  CALL set(ra%c%r2(1,1))
  IF (PRESENT(oreal)) THEN
    oreal = get()
  END IF

  jm = get_jmodel(1)
  WRITE (*,*) jm%name, ' : ', jm%id

#ifdef _OPENACC
  WRITE (*,*) 'OPENACC enabled'
#endif
END SUBROUTINE testsub

END MODULE sub
