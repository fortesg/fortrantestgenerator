PROGRAM testbed

USE mo_mpi,              ONLY: get_my_mpi_all_id, start_mpi, stop_mpi
USE mo_ftg_testsub_test, ONLY: ftg_test_testsub

IMPLICIT NONE

CALL start_mpi('testprogram:testbed')

CALL ftg_test_testsub()

CALL stop_mpi()

END PROGRAM testbed
