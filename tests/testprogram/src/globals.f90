MODULE globals

USE types

IMPLICIT NONE

PRIVATE

INTEGER, PARAMETER :: base = 23
REAL :: number = 0
TYPE(t_comm_variable), ALLOCATABLE :: comm_variable(:)

PUBLIC :: set, get, init_comm_variable, comm_variable

CONTAINS

SUBROUTINE set(new)
  REAL, INTENT(in) :: new
  number = number + base + new
END SUBROUTINE set

REAL FUNCTION get()
  get = number
END FUNCTION get

SUBROUTINE init_comm_variable()

  INTEGER :: i, j

  ALLOCATE(comm_variable(2))
  DO i = 1, 2
    ALLOCATE(comm_variable(i)%grid_comm_pattern)
    ALLOCATE(comm_variable(i)%grid_comm_pattern%send(2))
    DO j = 1, 2
      ALLOCATE(comm_variable(i)%grid_comm_pattern%send(j)%index_no(2))
    END DO
  END DO
END SUBROUTINE init_comm_variable

END MODULE globals
