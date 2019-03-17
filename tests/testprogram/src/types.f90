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

  TYPE :: jmodel
      INTEGER                            :: id
      CHARACTER(len=10)                  :: name
  END TYPE jmodel

  TYPE jmodel_m
    TYPE(jmodel), POINTER :: m => NULL()
  END TYPE jmodel_m

  TYPE testj
     INTEGER :: no_of_models
     TYPE(jmodel_m), POINTER :: models(:) => NULL()
  END TYPE testj

  INTERFACE get_jmodel
     MODULE PROCEDURE get_model_by_id
  END INTERFACE get_jmodel

  TYPE t_comm_variable
    TYPE(t_grid_comm_pattern), POINTER :: grid_comm_pattern
  END TYPE t_comm_variable

  TYPE t_grid_comm_pattern
    TYPE(t_process_comm_pattern), POINTER :: send(:)
  END TYPE t_grid_comm_pattern

  TYPE t_process_comm_pattern
    INTEGER, ALLOCATABLE :: index_no(:)  ! the local index of the variable
  END TYPE t_process_comm_pattern

  TYPE, ABSTRACT :: abstr
    CONTAINS
    PROCEDURE (int_deff), DEFERRED :: deff
  END TYPE abstr

  ABSTRACT INTERFACE
    INTEGER FUNCTION int_deff(this)
      IMPORT abstr
      CLASS(abstr), INTENT(inout) :: this
    END FUNCTION int_deff
  END INTERFACE

  TYPE, EXTENDS(abstr) :: concr
    INTEGER :: internal = 0
    INTEGER, ALLOCATABLE :: multi(:)
    CONTAINS
    PROCEDURE :: deff => impl
  END TYPE concr

  TYPE :: abstrcont
    CLASS(abstr), POINTER :: p
  END TYPE abstrcont

  TYPE :: bbstr
    TYPE(abstrcont) :: bb(3)
  END TYPE bbstr

  TYPE(testj) :: tj

CONTAINS

  FUNCTION get_model_by_id(id) RESULT(model)

    INTEGER, INTENT(in)        :: id
    TYPE(jmodel), POINTER :: model

    model => tj%models(id)%m

  END FUNCTION get_model_by_id

  SUBROUTINE init_jmodels(no_of_models)
    INTEGER, INTENT(in) :: no_of_models
    INTEGER :: i
    IF (no_of_models > 0) THEN
      tj%no_of_models = no_of_models
      ALLOCATE(tj%models(no_of_models))
      DO i = 1, no_of_models
        ALLOCATE(tj%models(i)%m)
        tj%models(i)%m%id = i
        WRITE (tj%models(i)%m%name, '(A,I0)') 'Model ', i
      END DO
    END IF
  END SUBROUTINE init_jmodels

  INTEGER FUNCTION impl(this)
      CLASS(concr), INTENT(inout) :: this
      this%internal = this%internal + 1
      impl = this%internal + this%multi(1)
  END FUNCTION impl
END MODULE types
