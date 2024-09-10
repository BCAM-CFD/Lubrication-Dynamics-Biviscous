!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------
SUBROUTINE input_destructor(this)
  !----------------------------------------
  ! Destructor of the input object
  !----------------------------------------
  IMPLICIT NONE
  TYPE(input_type), INTENT(inout) :: this

  IF (ALLOCATED(this%L)) THEN
     DEALLOCATE(this%L)
  ENDIF

END SUBROUTINE input_destructor
