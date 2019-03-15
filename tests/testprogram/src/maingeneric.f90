PROGRAM maingeneric

USE types
USE globals, ONLY : set, init_comm_variable, bv
USE sub, ONLY : wrapper, init

IMPLICIT NONE

include 'mpif.h'

INTEGER rank, size, ierror, tag, status(MPI_STATUS_SIZE), u, v
TYPE(testa) :: a, oa
TYPE(testb), TARGET :: b(3)
LOGICAL :: flag(4)
REAL :: out, result
CLASS(abstr), ALLOCATABLE, TARGET :: av1, av2(:)

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
CALL init_comm_variable()

ALLOCATE(concr::av1)
SELECT TYPE (av1)
  TYPE IS (concr)
    ALLOCATE(av1%multi(19))
    av1%multi(:) = 4321
END SELECT

ALLOCATE(concr::av2(6))
SELECT TYPE (av2)
  TYPE IS (concr)
    DO u = 1, 6
      ALLOCATE(av2(u)%multi(19))
      av2(u)%multi(:) = 8654
  END DO
END SELECT

bv(1)%bb(1)%p => av2(1)
bv(1)%bb(2)%p => av2(2)
bv(1)%bb(3)%p => av2(3)
bv(2)%bb(1)%p => av2(4)
bv(2)%bb(2)%p => av2(5)
bv(2)%bb(3)%p => av2(6)

flag(:) = .TRUE.

CALL set(109.0)

IF (MOD(rank, 2) == 0) THEN
  result = wrapper(a, flag, av1, oa = oa)
ELSE
  result = wrapper(a, flag, av1, out, oa)
  PRINT*, 'node', rank, ' out: ', out
END IF

PRINT*, 'node', rank, ' a%c%r2: ', a%c%r2
PRINT*, 'node', rank, ' result: ', result

CALL MPI_FINALIZE(ierror)

END PROGRAM maingeneric
