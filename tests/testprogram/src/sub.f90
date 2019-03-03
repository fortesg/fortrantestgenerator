MODULE sub

USE types
USE globals, ONLY : set, get, comm_variable

IMPLICIT NONE

PRIVATE

INTEGER :: self

PUBLIC :: init, wrapper


! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

PUBLIC :: self
PUBLIC :: testsub

! ================= END FORTRAN TEST GENERATOR (FTG) =========================


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

SUBROUTINE init(start)
  INTEGER, INTENT(in) :: start
  self = start
END SUBROUTINE init

FUNCTION wrapper(ra, rlog, oreal, oa)
  TYPE(testa), INTENT(inout) :: ra
  LOGICAL, INTENT(in) :: rlog(4)
  REAL, INTENT(out), OPTIONAL :: oreal
  TYPE(testa), INTENT(inout), OPTIONAL :: oa
  REAL :: wrapper

  wrapper = testsub(ra, rlog, oreal, oa)
END FUNCTION wrapper

FUNCTION testsub(ra, rlog, oreal, oa)

  TYPE(testa), INTENT(inout) :: ra
  LOGICAL, INTENT(in) :: rlog(4)
  REAL, INTENT(out), OPTIONAL :: oreal
  TYPE(testa), INTENT(inout), OPTIONAL :: oa
  TYPE(jmodel) :: jm
#ifdef _OPENACC
  LOGICAL :: openacc
#endif
  REAL, POINTER :: testsub

! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

ftg_testsub_round = ftg_testsub_round + 1
IF (ftg_testsub_capture_input_active()) THEN
  CALL ftg_testsub_capture_input(ra, rlog, oreal, oa)
END IF

! ================= END FORTRAN TEST GENERATOR (FTG) =========================


  IF (rlog(1)) THEN
    ra%c%r2(:,:) = ra%b(1)%i2(1:2, 1:2) * ra%b(1)%i0 + ra%c%r2(:,:) + self
  END IF

  IF (PRESENT(oa)) THEN
    oa%c%r2(:,:) = oa%b(1)%i2(1:2, 1:2) * oa%b(1)%i0 + oa%c%r2(:,:)
    oa%c%r2(1,:) = oa%c%r2(1,:) + comm_variable(1)%grid_comm_pattern%send(1)%index_no(:)
  END IF

  CALL set(ra%c%r2(1,1))
  ALLOCATE(testsub)
  testsub = get()
  IF (PRESENT(oreal)) THEN
    oreal = testsub
  END IF

  jm = get_jmodel(1)
  WRITE (*,*) jm%name, ' : ', jm%id

#ifdef _OPENACC
  WRITE (*,*) 'OPENACC enabled'
#endif

! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

IF (ftg_testsub_capture_output_active()) THEN
  CALL ftg_testsub_capture_output(ra, oreal, oa, testsub)
END IF

! ================= END FORTRAN TEST GENERATOR (FTG) =========================

END FUNCTION testsub

! ================= BEGIN FORTRAN TEST GENERATOR (FTG) =======================

SUBROUTINE ftg_testsub_init_for_capture(stage, dir)
  
  USE mo_impl_constants, ONLY: MAX_CHAR_LENGTH
  USE mo_mpi,          ONLY: get_my_mpi_all_id
  USE m_ser_ftg,       ONLY: ftg_set_serializer, ftg_add_serializer_metainfo, ignore_bullshit_max_dim_size, &
  &  ignore_bullshit_allow_negative_indices
  
  CHARACTER(*), INTENT(IN) :: stage, dir
  
  CHARACTER(len=MAX_CHAR_LENGTH) :: basename, mkdirerr
  
  WRITE (basename,'(A,A,A,I0.2)') 'ftg_testsub_', TRIM(stage), '_', get_my_mpi_all_id()
  
  WRITE (0,*) 'FTG INIT testsub '//TRIM(basename)
  
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
  CALL ftg_add_serializer_metainfo('mpi_all_id', get_my_mpi_all_id())
  
END SUBROUTINE ftg_testsub_init_for_capture

LOGICAL FUNCTION ftg_testsub_capture_input_active()

ftg_testsub_capture_input_active =     &
ftg_testsub_capture_input_enabled .AND.&
ftg_testsub_round .EQ. ftg_testsub_capture_round

END FUNCTION ftg_testsub_capture_input_active

SUBROUTINE ftg_testsub_capture_input(ra, rlog, oreal, oa)
  
  USE mtime,   ONLY: calendarType
  USE mo_mpi,  ONLY: get_my_mpi_all_id
  USE m_ser_ftg, ONLY: ftg_set_savepoint, ftg_write, ftg_register_only, ftg_destroy_savepoint, ftg_destroy_serializer
  
  USE globals, ONLY: globals__number => number, globals__comm_variable => comm_variable
  USE types, ONLY: types__tj => tj, t_comm_variable, testj
  USE types, ONLY: testa, t_comm_variable, testj
  
  TYPE(testa), INTENT(in) :: ra
  LOGICAL, DIMENSION(:), INTENT(in) :: rlog
  REAL, INTENT(in), OPTIONAL :: oreal
  TYPE(testa), INTENT(in), OPTIONAL :: oa
  
  INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
  CHARACTER(len=256) :: ftg_name
  
  IF (ftg_testsub_capture_input_active()) THEN
    
    CALL ftg_testsub_init_for_capture('input', ftg_testsub_input_dir)
    CALL ftg_set_savepoint('input')
    
    WRITE (0,*) 'FTG ROUND testsub ', ftg_testsub_round, get_my_mpi_all_id()
    WRITE (0,*) 'FTG WRITE INPUT DATA testsub', get_my_mpi_all_id()
    
    ! MTIME CALENDAR TYPE --> Remove this line if mtime is not used
    CALL ftg_write("ftg_mtime_calendar", calendarType())
    
    ! REQUIRED ARGUMENTS
    CALL ftg_register_only("ra", "TYPE(testa)")
    IF (ASSOCIATED(ra%b)) THEN
      CALL ftg_register_only("ra%b", "TYPE(testb)", LBOUND(ra%b), UBOUND(ra%b))
      CALL ftg_write("ra%b%i0", ra%b%i0, LBOUND(ra%b%i0), UBOUND(ra%b%i0))
      
      
      DO ftg_d1 = LBOUND(ra%b, 1), UBOUND(ra%b, 1)
        IF (ALLOCATED(ra%b(ftg_d1)%i2)) THEN
          WRITE (ftg_name,'(A,I0,A)') 'ra%b(', ftg_d1, ')%i2'
          CALL ftg_write(ftg_name, ra%b(ftg_d1)%i2, LBOUND(ra%b(ftg_d1)%i2), UBOUND(ra%b(ftg_d1)%i2))
          
          
        END IF
      END DO
    END IF
    
    CALL ftg_register_only("ra%c", "TYPE(testc)")
    IF (ALLOCATED(ra%c%r2)) THEN
      CALL ftg_write("ra%c%r2", ra%c%r2, LBOUND(ra%c%r2), UBOUND(ra%c%r2))
      
      
    END IF
    CALL ftg_write("rlog", rlog, LBOUND(rlog), UBOUND(rlog))
    
    
    
    ! OPTIONAL ARGUMENTS
    IF (PRESENT(oreal)) THEN
      CALL ftg_write("oreal", oreal)
      
      
    END IF
    IF (PRESENT(oa)) THEN
      CALL ftg_register_only("oa", "TYPE(testa)")
      IF (ASSOCIATED(oa%b)) THEN
        CALL ftg_register_only("oa%b", "TYPE(testb)", LBOUND(oa%b), UBOUND(oa%b))
        CALL ftg_write("oa%b%i0", oa%b%i0, LBOUND(oa%b%i0), UBOUND(oa%b%i0))
        
        
        DO ftg_d1 = LBOUND(oa%b, 1), UBOUND(oa%b, 1)
          IF (ALLOCATED(oa%b(ftg_d1)%i2)) THEN
            WRITE (ftg_name,'(A,I0,A)') 'oa%b(', ftg_d1, ')%i2'
            CALL ftg_write(ftg_name, oa%b(ftg_d1)%i2, LBOUND(oa%b(ftg_d1)%i2), UBOUND(oa%b(ftg_d1)%i2))
            
            
          END IF
        END DO
      END IF
      
      CALL ftg_register_only("oa%c", "TYPE(testc)")
      IF (ALLOCATED(oa%c%r2)) THEN
        CALL ftg_write("oa%c%r2", oa%c%r2, LBOUND(oa%c%r2), UBOUND(oa%c%r2))
        
        
      END IF
    END IF
    
    ! GLOBALS
    CALL ftg_write("globals__number", globals__number)
    
    
    CALL ftg_write("self", self)
    
    
    IF (ALLOCATED(globals__comm_variable)) THEN
      CALL ftg_register_only("globals__comm_variable", "TYPE(t_comm_variable)", LBOUND(globals__comm_variable), UBOUND( &
      &  globals__comm_variable))
      DO ftg_d1 = LBOUND(globals__comm_variable, 1), UBOUND(globals__comm_variable, 1)
        IF (ASSOCIATED(globals__comm_variable(ftg_d1)%grid_comm_pattern)) THEN
          WRITE (ftg_name,'(A,I0,A)') 'globals__comm_variable(', ftg_d1, ')%grid_comm_pattern'
          CALL ftg_register_only(ftg_name, "TYPE(t_grid_comm_pattern)")
          IF (ASSOCIATED(globals__comm_variable(ftg_d1)%grid_comm_pattern%send)) THEN
            WRITE (ftg_name,'(A,I0,A)') 'globals__comm_variable(', ftg_d1, ')%grid_comm_pattern%send'
            CALL ftg_register_only(ftg_name, "TYPE(t_process_comm_pattern)", LBOUND(globals__comm_variable(ftg_d1)% &
            &  grid_comm_pattern%send), UBOUND(globals__comm_variable(ftg_d1)%grid_comm_pattern%send))
            DO ftg_d2 = LBOUND(globals__comm_variable(ftg_d1)%grid_comm_pattern%send, 1), UBOUND(globals__comm_variable(ftg_d1)% &
            &  grid_comm_pattern%send, 1)
              IF (ALLOCATED(globals__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)%index_no)) THEN
                WRITE (ftg_name,'(A,I0,A,I0,A)') 'globals__comm_variable(', ftg_d1, ')%grid_comm_pattern%send(', ftg_d2, &
                &  ')%index_no'
                CALL ftg_write(ftg_name, globals__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)%index_no, LBOUND( &
                &  globals__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)%index_no), UBOUND(globals__comm_variable(ftg_d1)% &
                &  grid_comm_pattern%send(ftg_d2)%index_no))
                
                
              END IF
            END DO
          END IF
        END IF
      END DO
    END IF
    
    CALL ftg_register_only("types__tj", "TYPE(testj)")
    IF (ASSOCIATED(types__tj%models)) THEN
      CALL ftg_register_only("types__tj%models", "TYPE(jmodel_m)", LBOUND(types__tj%models), UBOUND(types__tj%models))
      DO ftg_d1 = LBOUND(types__tj%models, 1), UBOUND(types__tj%models, 1)
        IF (ASSOCIATED(types__tj%models(ftg_d1)%m)) THEN
          WRITE (ftg_name,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m'
          CALL ftg_register_only(ftg_name, "TYPE(jmodel)")
          WRITE (ftg_name,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m%id'
          CALL ftg_write(ftg_name, types__tj%models(ftg_d1)%m%id)
          
          
        END IF
      END DO
    END IF
    
    ! *** WARNING: Type not supported by serialbox ***
    !     types__tj%models%m%name
    !     CHARACTER(len=10), DIMENSION(0)
    
    CALL ftg_write("ftg_testsub_round", ftg_testsub_round)
    
    CALL ftg_destroy_savepoint()
    WRITE (0,*) 'FTG FINALIZE INPUT DATA testsub', get_my_mpi_all_id()
    CALL ftg_destroy_serializer()
    
  END IF
  
END SUBROUTINE ftg_testsub_capture_input

LOGICAL FUNCTION ftg_testsub_capture_output_active()

ftg_testsub_capture_output_active =     &
ftg_testsub_capture_output_enabled .AND.&
ftg_testsub_round .EQ. ftg_testsub_capture_round

END FUNCTION ftg_testsub_capture_output_active

SUBROUTINE ftg_testsub_capture_output(ra, oreal, oa, testsub)
  
  USE mo_mpi, ONLY: work_mpi_barrier, get_my_mpi_all_id
  USE mo_exception, ONLY: finish
  USE m_ser_ftg, ONLY: ftg_set_savepoint, ftg_write, ftg_register_only, ftg_destroy_savepoint, ftg_destroy_serializer
  
  USE globals, ONLY: globals__number => number, globals__comm_variable => comm_variable
  USE types, ONLY: types__tj => tj, t_comm_variable, testj
  USE types, ONLY: testa, t_comm_variable, testj
  
  TYPE(testa), INTENT(in) :: ra
  REAL, INTENT(in), OPTIONAL :: oreal
  TYPE(testa), INTENT(in), OPTIONAL :: oa
  REAL, POINTER, INTENT(in) :: testsub
  
  INTEGER :: ftg_d1, ftg_d2, ftg_d3, ftg_d4
  CHARACTER(len=256) :: ftg_name
  
  IF (ftg_testsub_capture_output_active()) THEN
    
    CALL ftg_testsub_init_for_capture('output', ftg_testsub_output_dir)
    CALL ftg_set_savepoint('output')
    
    WRITE (0,*) 'FTG ROUND testsub ', ftg_testsub_round, get_my_mpi_all_id()
    WRITE (0,*) 'FTG WRITE output DATA testsub', get_my_mpi_all_id()
    
    
    ! REQUIRED ARGUMENTS
    CALL ftg_register_only("ra", "TYPE(testa)")
    IF (ASSOCIATED(ra%b)) THEN
      CALL ftg_register_only("ra%b", "TYPE(testb)", LBOUND(ra%b), UBOUND(ra%b))
      CALL ftg_write("ra%b%i0", ra%b%i0, LBOUND(ra%b%i0), UBOUND(ra%b%i0))
      
      
      DO ftg_d1 = LBOUND(ra%b, 1), UBOUND(ra%b, 1)
        IF (ALLOCATED(ra%b(ftg_d1)%i2)) THEN
          WRITE (ftg_name,'(A,I0,A)') 'ra%b(', ftg_d1, ')%i2'
          CALL ftg_write(ftg_name, ra%b(ftg_d1)%i2, LBOUND(ra%b(ftg_d1)%i2), UBOUND(ra%b(ftg_d1)%i2))
          
          
        END IF
      END DO
    END IF
    
    CALL ftg_register_only("ra%c", "TYPE(testc)")
    IF (ALLOCATED(ra%c%r2)) THEN
      CALL ftg_write("ra%c%r2", ra%c%r2, LBOUND(ra%c%r2), UBOUND(ra%c%r2))
      
      
    END IF
    
    ! OPTIONAL ARGUMENTS
    IF (PRESENT(oreal)) THEN
      CALL ftg_write("oreal", oreal)
      
      
    END IF
    IF (PRESENT(oa)) THEN
      CALL ftg_register_only("oa", "TYPE(testa)")
      IF (ASSOCIATED(oa%b)) THEN
        CALL ftg_register_only("oa%b", "TYPE(testb)", LBOUND(oa%b), UBOUND(oa%b))
        CALL ftg_write("oa%b%i0", oa%b%i0, LBOUND(oa%b%i0), UBOUND(oa%b%i0))
        
        
        DO ftg_d1 = LBOUND(oa%b, 1), UBOUND(oa%b, 1)
          IF (ALLOCATED(oa%b(ftg_d1)%i2)) THEN
            WRITE (ftg_name,'(A,I0,A)') 'oa%b(', ftg_d1, ')%i2'
            CALL ftg_write(ftg_name, oa%b(ftg_d1)%i2, LBOUND(oa%b(ftg_d1)%i2), UBOUND(oa%b(ftg_d1)%i2))
            
            
          END IF
        END DO
      END IF
      
      CALL ftg_register_only("oa%c", "TYPE(testc)")
      IF (ALLOCATED(oa%c%r2)) THEN
        CALL ftg_write("oa%c%r2", oa%c%r2, LBOUND(oa%c%r2), UBOUND(oa%c%r2))
        
        
      END IF
    END IF
    
    ! RESULT
    IF (ASSOCIATED(testsub)) THEN
      CALL ftg_write("testsub", testsub)
      
      
    END IF
    
    ! GLOBALS
    CALL ftg_write("globals__number", globals__number)
    
    
    CALL ftg_write("self", self)
    
    
    IF (ALLOCATED(globals__comm_variable)) THEN
      CALL ftg_register_only("globals__comm_variable", "TYPE(t_comm_variable)", LBOUND(globals__comm_variable), UBOUND( &
      &  globals__comm_variable))
      DO ftg_d1 = LBOUND(globals__comm_variable, 1), UBOUND(globals__comm_variable, 1)
        IF (ASSOCIATED(globals__comm_variable(ftg_d1)%grid_comm_pattern)) THEN
          WRITE (ftg_name,'(A,I0,A)') 'globals__comm_variable(', ftg_d1, ')%grid_comm_pattern'
          CALL ftg_register_only(ftg_name, "TYPE(t_grid_comm_pattern)")
          IF (ASSOCIATED(globals__comm_variable(ftg_d1)%grid_comm_pattern%send)) THEN
            WRITE (ftg_name,'(A,I0,A)') 'globals__comm_variable(', ftg_d1, ')%grid_comm_pattern%send'
            CALL ftg_register_only(ftg_name, "TYPE(t_process_comm_pattern)", LBOUND(globals__comm_variable(ftg_d1)% &
            &  grid_comm_pattern%send), UBOUND(globals__comm_variable(ftg_d1)%grid_comm_pattern%send))
            DO ftg_d2 = LBOUND(globals__comm_variable(ftg_d1)%grid_comm_pattern%send, 1), UBOUND(globals__comm_variable(ftg_d1)% &
            &  grid_comm_pattern%send, 1)
              IF (ALLOCATED(globals__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)%index_no)) THEN
                WRITE (ftg_name,'(A,I0,A,I0,A)') 'globals__comm_variable(', ftg_d1, ')%grid_comm_pattern%send(', ftg_d2, &
                &  ')%index_no'
                CALL ftg_write(ftg_name, globals__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)%index_no, LBOUND( &
                &  globals__comm_variable(ftg_d1)%grid_comm_pattern%send(ftg_d2)%index_no), UBOUND(globals__comm_variable(ftg_d1)% &
                &  grid_comm_pattern%send(ftg_d2)%index_no))
                
                
              END IF
            END DO
          END IF
        END IF
      END DO
    END IF
    
    CALL ftg_register_only("types__tj", "TYPE(testj)")
    IF (ASSOCIATED(types__tj%models)) THEN
      CALL ftg_register_only("types__tj%models", "TYPE(jmodel_m)", LBOUND(types__tj%models), UBOUND(types__tj%models))
      DO ftg_d1 = LBOUND(types__tj%models, 1), UBOUND(types__tj%models, 1)
        IF (ASSOCIATED(types__tj%models(ftg_d1)%m)) THEN
          WRITE (ftg_name,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m'
          CALL ftg_register_only(ftg_name, "TYPE(jmodel)")
          WRITE (ftg_name,'(A,I0,A)') 'types__tj%models(', ftg_d1, ')%m%id'
          CALL ftg_write(ftg_name, types__tj%models(ftg_d1)%m%id)
          
          
        END IF
      END DO
    END IF
    
    ! *** WARNING: Type not supported by serialbox ***
    !     types__tj%models%m%name
    !     CHARACTER(len=10), DIMENSION(0)
    
    CALL ftg_write("ftg_testsub_round", ftg_testsub_round)
    
    CALL ftg_destroy_savepoint()
    WRITE (0,*) 'FTG FINALIZE output DATA testsub', get_my_mpi_all_id()
    CALL ftg_destroy_serializer()
    
    CALL work_mpi_barrier()
    CALL finish('FTG EXIT', 'Output captured: testsub', 0)
    
  END IF
  
END SUBROUTINE ftg_testsub_capture_output

! ================= END FORTRAN TEST GENERATOR (FTG) =========================


END MODULE sub
