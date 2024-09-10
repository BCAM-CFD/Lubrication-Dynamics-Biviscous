!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE write_comp_times(this, step, unit)
  !---------------------------------------------
  ! The computational times are outputted in a file.
  !---------------------------------------------
  IMPLICIT NONE
  TYPE(comp_time_type), INTENT(in) :: this
  INTEGER, INTENT(in)              :: step
  INTEGER, INTENT(in)              :: unit

  WRITE(unit, '(I10, 4E20.10)') &
       step,               &  !1
       this%total,         &  !2
       this%neigh,         &  !3
       this%semi_impl,     &  !4
       this%VV                !5
  
END SUBROUTINE write_comp_times
