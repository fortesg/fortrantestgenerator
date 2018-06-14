MODULE types

IMPLICIT NONE

TYPE testc
  REAL, ALLOCATABLE :: r2(:,:)
END TYPE testc

TYPE testb
  INTEGER :: i0
  INTEGER :: i1(3)
  INTEGER, ALLOCATABLE :: i2(:,:)
  INTEGER, POINTER :: i3(:,:,:) => NULL()
END TYPE testb

TYPE testa
  TYPE(testb), POINTER :: b(:) => NULL()
  TYPE(testc) :: c
END TYPE testa

END MODULE types
