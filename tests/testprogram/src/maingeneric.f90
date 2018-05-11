PROGRAM maingeneric

USE types
USE globals, ONLY : set
USE sub, ONLY : testsub

IMPLICIT NONE

include 'mpif.h'

INTEGER rank, size, ierror, tag, status(MPI_STATUS_SIZE), u
TYPE(testa) :: a
TYPE(testb), TARGET :: b(42)
LOGICAL :: flag
REAL :: out

CALL MPI_INIT(ierror)
CALL MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
CALL MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)

a%b => b

DO u = 1, 42
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

flag = .TRUE.

CALL set(109.0)

IF (MOD(rank, 2) == 0) THEN
  CALL testsub(a, flag)
ELSE
  CALL testsub(a, flag, out)
  PRINT*, 'node', rank, ': ', out
END IF

PRINT*, 'node', rank, ': ', a%c%r2

CALL MPI_FINALIZE(ierror)

END PROGRAM maingeneric
