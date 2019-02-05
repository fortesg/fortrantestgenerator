PROGRAM maingeneric

USE types
USE globals, ONLY : set
USE sub, ONLY : testsub, init

IMPLICIT NONE

include 'mpif.h'

INTEGER rank, size, ierror, tag, status(MPI_STATUS_SIZE), u
TYPE(testa) :: a, oa
TYPE(testb), TARGET :: b(3)
LOGICAL :: flag(4)
REAL :: out, result

CALL MPI_INIT(ierror)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

CALL init(42)

a%b => b
oa%b => b

DO u = 1, 3
  a%b(u)%i0 = rank
  a%b(u)%i1(:) = rank * 10
  ALLOCATE(a%b(u)%i2(8, rank + 1))
  a%b(u)%i2(:,:) = 42
  ALLOCATE(a%b(u)%i3(rank + 1, rank + 1, rank + 1))
  a%b(u)%i3(:,:,:) = rank * 100 + 42
END DO

ALLOCATE(a%c%r2(2,2))
a%c%r2(1,1) = rank + 0.11
a%c%r2(1,2) = rank + 0.12
a%c%r2(2,1) = rank + 0.21
a%c%r2(2,2) = rank + 0.22

ALLOCATE(oa%c%r2(2,2))
oa%c%r2(1,1) = rank + 0.11
oa%c%r2(1,2) = rank + 0.12
oa%c%r2(2,1) = rank + 0.21
oa%c%r2(2,2) = rank + 0.22

CALL init_jmodels(1)

flag(:) = .TRUE.

CALL set(109.0)

IF (MOD(rank, 2) == 0) THEN
  result = testsub(a, flag, oa = oa)
ELSE
  result = testsub(a, flag, out, oa)
  PRINT*, 'node', rank, ' out: ', out
END IF

PRINT*, 'node', rank, ' a%c%r2: ', a%c%r2
PRINT*, 'node', rank, ' result: ', result

CALL MPI_FINALIZE(ierror)

END PROGRAM maingeneric
