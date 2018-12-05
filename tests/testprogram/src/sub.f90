MODULE sub

USE types
USE globals, ONLY : set, get

IMPLICIT NONE

PRIVATE

INTEGER, PARAMETER :: base = 23
REAL :: number = 0

PUBLIC :: testsub


! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

LOGICAL :: ftg_testsub_capture_input_enabled = .TRUE.
LOGICAL :: ftg_testsub_capture_output_enabled = .TRUE.
INTEGER :: ftg_testsub_capture_round = 1
INTEGER :: ftg_testsub_round = 0
CHARACTER(len=1024) :: ftg_testsub_input_dir = &
&  '/home/christian/workspaces/eclipse/fortrantestgenerator/tests/testprogram/data/ftg_testsub_test/input'
CHARACTER(len=1024) :: ftg_testsub_output_dir = &
&  '/home/christian/workspaces/eclipse/fortrantestgenerator/tests/testprogram/data/ftg_testsub_test/output'

PUBLIC :: ftg_testsub_capture_input_enabled, ftg_testsub_capture_output_enabled, ftg_testsub_capture_round, ftg_testsub_input_dir, &
&  ftg_testsub_output_dir




! ================= END FORTRAN TEST GENERATOR (FTG) =========================

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

! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

ftg_testsub_round = ftg_testsub_round + 1
CALL ftg_testsub_capture_input(ra, rlog, oreal, oa)

! ================= END FORTRAN TEST GENERATOR (FTG) =========================


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

! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

CALL ftg_testsub_capture_output(ra, oreal, oa)

! ================= END FORTRAN TEST GENERATOR (FTG) =========================

END SUBROUTINE testsub

! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

SUBROUTINE ftg_testsub_init_for_capture(stage, dir)
  
  USE mpi
  USE m_ser_ftg,         ONLY: ftg_set_serializer, ftg_add_serializer_metainfo, ignore_bullshit_max_dim_size, &
  &  ignore_bullshit_allow_negative_indices
  
  CHARACTER(*), INTENT(IN) :: stage, dir
  
  INTEGER(kind=4) my_mpi_id, error
  CHARACTER(len=1024) :: basename, mkdirerr
  
  CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_mpi_id, error)
  
  WRITE (basename,'(a,a,a,i0)') 'ftg_testsub_', TRIM(stage), '_', my_mpi_id
  
  WRITE (0,*) 'FTG INIT testsub '//TRIM(basename), my_mpi_id
  
  mkdirerr = ''
  CALL EXECUTE_COMMAND_LINE('mkdir -p '//TRIM(dir), cmdmsg=mkdirerr)
  IF (TRIM(mkdirerr) /= '') THEN
    WRITE (0,*) 'FTG INIT *** ERROR: ', TRIM(mkdirerr)
  END IF
  
  ignore_bullshit_max_dim_size = 100000
  ignore_bullshit_allow_negative_indices = .TRUE.
  
  CALL ftg_set_serializer(TRIM(dir), TRIM(basename), 'w')
  CALL ftg_add_serializer_metainfo('subroutine', 'testsub')
  CALL ftg_add_serializer_metainfo('stage', stage)
  CALL ftg_add_serializer_metainfo('mpi_id', my_mpi_id)
  
END SUBROUTINE ftg_testsub_init_for_capture

LOGICAL FUNCTION ftg_testsub_capture_input_active()

ftg_testsub_capture_input_active =       &
ftg_testsub_capture_input_enabled .AND.  &
ftg_testsub_round .EQ. ftg_testsub_capture_round

END FUNCTION ftg_testsub_capture_input_active

SUBROUTINE ftg_testsub_capture_input(ra, rlog, oreal, oa)
  
  USE mpi
  USE m_ser_ftg, ONLY: ftg_set_savepoint, ftg_write, ftg_register_only, ftg_destroy_savepoint, ftg_destroy_serializer
  
  USE globals, ONLY: globals__number => number
  USE types, ONLY: types__tj => tj, testj
  
  TYPE(testa), INTENT(in) :: ra
  LOGICAL, DIMENSION(:), INTENT(in) :: rlog
  REAL, INTENT(in), OPTIONAL :: oreal
  TYPE(testa), INTENT(in), OPTIONAL :: oa
  
  INTEGER(kind=4) my_mpi_id, error
  INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
  CHARACTER(len=256) :: ftg_c
  
  IF (ftg_testsub_capture_input_active()) THEN
    
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_mpi_id, error)
    CALL ftg_testsub_init_for_capture('input', ftg_testsub_input_dir)
    CALL ftg_set_savepoint('input')
    
    WRITE (0,*) 'FTG ROUND testsub ', ftg_testsub_round, my_mpi_id
    WRITE (0,*) 'FTG WRITE INPUT DATA testsub', my_mpi_id
    
    ! REQUIRED ARGUMENTS
    CALL ftg_register_only("ra", "TYPE(testa)")
    IF (ASSOCIATED(ra%b)) THEN
      CALL ftg_register_only("ra%b", "TYPE(testb)", LBOUND(ra%b), UBOUND(ra%b))
    END IF
    CALL ftg_write("ra%b%i0", ra%b%i0, LBOUND(ra%b%i0), UBOUND(ra%b%i0))
    
    IF (ASSOCIATED(ra%b)) THEN
      DO ftg_d1 = LBOUND(ra%b, 1), UBOUND(ra%b, 1)
        WRITE (ftg_c,'(A,I0,A)') 'ra%b(', ftg_d1, ')%i2'
        CALL ftg_write(ftg_c, ra%b(ftg_d1)%i2, LBOUND(ra%b(ftg_d1)%i2), UBOUND(ra%b(ftg_d1)%i2))
      END DO
    END IF
    
    CALL ftg_register_only("ra%c", "TYPE(testc)")
    CALL ftg_write("ra%c%r2", ra%c%r2, LBOUND(ra%c%r2), UBOUND(ra%c%r2))
    
    CALL ftg_write("rlog", rlog, LBOUND(rlog), UBOUND(rlog))
    
    
    
    ! OPTIONAL ARGUMENTS
    IF (PRESENT(oreal)) THEN
      CALL ftg_write("oreal", oreal)
      
      
    END IF
    IF (PRESENT(oa)) THEN
      CALL ftg_register_only("oa", "TYPE(testa)")
      IF (ASSOCIATED(oa%b)) THEN
        CALL ftg_register_only("oa%b", "TYPE(testb)", LBOUND(oa%b), UBOUND(oa%b))
      END IF
      CALL ftg_write("oa%b%i0", oa%b%i0, LBOUND(oa%b%i0), UBOUND(oa%b%i0))
      
      IF (ASSOCIATED(oa%b)) THEN
        DO ftg_d1 = LBOUND(oa%b, 1), UBOUND(oa%b, 1)
          WRITE (ftg_c,'(A,I0,A)') 'oa%b(', ftg_d1, ')%i2'
          CALL ftg_write(ftg_c, oa%b(ftg_d1)%i2, LBOUND(oa%b(ftg_d1)%i2), UBOUND(oa%b(ftg_d1)%i2))
        END DO
      END IF
      
      CALL ftg_register_only("oa%c", "TYPE(testc)")
      CALL ftg_write("oa%c%r2", oa%c%r2, LBOUND(oa%c%r2), UBOUND(oa%c%r2))
      
      
    END IF
    
    ! GLOBALS
    CALL ftg_write("globals__number", globals__number)
    
    ftg_c = "types__tj"
    CALL ftg_register_only(ftg_c, "TYPE(testj)")
    ftg_c = "types__tj%models"
    CALL ftg_register_only(ftg_c, "TYPE(jmodel_m)", LBOUND(types__tj%models), UBOUND(types__tj%models))
    IF (ASSOCIATED(types__tj%models)) THEN
      DO ftg_d1 = LBOUND(types__tj%models, 1), UBOUND(types__tj%models, 1)
        IF (ASSOCIATED(types__tj%models(ftg_d1)%m)) THEN
          WRITE (ftg_c,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m'
          CALL ftg_register_only(ftg_c, "TYPE(jmodel)")
          WRITE (ftg_c,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m%id'
          CALL ftg_write(ftg_c, types__tj%models(ftg_d1)%m%id)
        END IF
      END DO
    END IF
    
    ! *** WARNING: Type not supported by serialbox ***
    !     types__tj%models%m%name
    !     CHARACTER(len=10), DIMENSION(0)
    
    
    
    CALL ftg_write("ftg_testsub_round", ftg_testsub_round)
    
    CALL ftg_destroy_savepoint()
    WRITE (0,*) 'FTG FINALIZE INPUT DATA testsub', my_mpi_id
    CALL ftg_destroy_serializer()
    
  END IF
  
END SUBROUTINE ftg_testsub_capture_input

LOGICAL FUNCTION ftg_testsub_capture_output_active()

ftg_testsub_capture_output_active =       &
ftg_testsub_capture_output_enabled .AND.  &
ftg_testsub_round .EQ. ftg_testsub_capture_round

END FUNCTION ftg_testsub_capture_output_active

SUBROUTINE ftg_testsub_capture_output(ra, oreal, oa)
  
  USE mpi
  USE m_ser_ftg, ONLY: ftg_set_savepoint, ftg_write, ftg_register_only, ftg_destroy_savepoint, ftg_destroy_serializer
  
  USE globals, ONLY: globals__number => number
  USE types, ONLY: types__tj => tj, testj
  
  TYPE(testa), INTENT(in) :: ra
  REAL, INTENT(in), OPTIONAL :: oreal
  TYPE(testa), INTENT(in), OPTIONAL :: oa
  
  INTEGER(kind=4) my_mpi_id, error
  INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
  CHARACTER(len=256) :: ftg_c
  
  IF (ftg_testsub_capture_output_active()) THEN
    
    CALL MPI_COMM_RANK(MPI_COMM_WORLD, my_mpi_id, error)
    CALL ftg_testsub_init_for_capture('output', ftg_testsub_output_dir)
    CALL ftg_set_savepoint('output')
    
    WRITE (0,*) 'FTG ROUND testsub ', ftg_testsub_round, my_mpi_id
    WRITE (0,*) 'FTG WRITE output DATA testsub', my_mpi_id
    
    
    ! REQUIRED ARGUMENTS
    CALL ftg_register_only("ra", "TYPE(testa)")
    IF (ASSOCIATED(ra%b)) THEN
      CALL ftg_register_only("ra%b", "TYPE(testb)", LBOUND(ra%b), UBOUND(ra%b))
    END IF
    CALL ftg_write("ra%b%i0", ra%b%i0, LBOUND(ra%b%i0), UBOUND(ra%b%i0))
    
    IF (ASSOCIATED(ra%b)) THEN
      DO ftg_d1 = LBOUND(ra%b, 1), UBOUND(ra%b, 1)
        WRITE (ftg_c,'(A,I0,A)') 'ra%b(', ftg_d1, ')%i2'
        CALL ftg_write(ftg_c, ra%b(ftg_d1)%i2, LBOUND(ra%b(ftg_d1)%i2), UBOUND(ra%b(ftg_d1)%i2))
      END DO
    END IF
    
    CALL ftg_register_only("ra%c", "TYPE(testc)")
    CALL ftg_write("ra%c%r2", ra%c%r2, LBOUND(ra%c%r2), UBOUND(ra%c%r2))
    
    
    
    ! OPTIONAL ARGUMENTS
    IF (PRESENT(oreal)) THEN
      CALL ftg_write("oreal", oreal)
      
      
    END IF
    IF (PRESENT(oa)) THEN
      CALL ftg_register_only("oa", "TYPE(testa)")
      IF (ASSOCIATED(oa%b)) THEN
        CALL ftg_register_only("oa%b", "TYPE(testb)", LBOUND(oa%b), UBOUND(oa%b))
      END IF
      CALL ftg_write("oa%b%i0", oa%b%i0, LBOUND(oa%b%i0), UBOUND(oa%b%i0))
      
      IF (ASSOCIATED(oa%b)) THEN
        DO ftg_d1 = LBOUND(oa%b, 1), UBOUND(oa%b, 1)
          WRITE (ftg_c,'(A,I0,A)') 'oa%b(', ftg_d1, ')%i2'
          CALL ftg_write(ftg_c, oa%b(ftg_d1)%i2, LBOUND(oa%b(ftg_d1)%i2), UBOUND(oa%b(ftg_d1)%i2))
        END DO
      END IF
      
      CALL ftg_register_only("oa%c", "TYPE(testc)")
      CALL ftg_write("oa%c%r2", oa%c%r2, LBOUND(oa%c%r2), UBOUND(oa%c%r2))
      
      
    END IF
    
    ! GLOBALS
    CALL ftg_write("globals__number", globals__number)
    
    ftg_c = "types__tj"
    CALL ftg_register_only(ftg_c, "TYPE(testj)")
    ftg_c = "types__tj%models"
    CALL ftg_register_only(ftg_c, "TYPE(jmodel_m)", LBOUND(types__tj%models), UBOUND(types__tj%models))
    IF (ASSOCIATED(types__tj%models)) THEN
      DO ftg_d1 = LBOUND(types__tj%models, 1), UBOUND(types__tj%models, 1)
        IF (ASSOCIATED(types__tj%models(ftg_d1)%m)) THEN
          WRITE (ftg_c,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m'
          CALL ftg_register_only(ftg_c, "TYPE(jmodel)")
          WRITE (ftg_c,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m%id'
          CALL ftg_write(ftg_c, types__tj%models(ftg_d1)%m%id)
        END IF
      END DO
    END IF
    
    ! *** WARNING: Type not supported by serialbox ***
    !     types__tj%models%m%name
    !     CHARACTER(len=10), DIMENSION(0)
    
    
    
    CALL ftg_write("ftg_testsub_round", ftg_testsub_round)
    
    CALL ftg_destroy_savepoint()
    WRITE (0,*) 'FTG FINALIZE output DATA testsub', my_mpi_id
    CALL ftg_destroy_serializer()
    
    CALL MPI_BARRIER (MPI_COMM_WORLD, error)
    STOP 'FTG STOP'
    
  END IF
  
END SUBROUTINE ftg_testsub_capture_output

! ================= END FORTRAN TEST GENERATOR (FTG) =========================


END MODULE sub
