!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE write_neighbours(this, part)
!---------------------------------------------
  ! Subroutine to write in the shell the 
  ! of particle part neighbours
  !-------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(in) :: this
  INTEGER, INTENT(in)           :: part
  INTEGER :: I, J
  
  WRITE(*,*) '---- List of particle',part,'neighbours ----'
  DO I = 1, this%part(part)%N_neigh
     J = this%part(part)%neigh_list(I)
     WRITE(*,*) J
  ENDDO

END SUBROUTINE write_neighbours
