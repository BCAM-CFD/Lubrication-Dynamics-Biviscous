!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!----------------------------------
SUBROUTINE particle_destructor(this)
!----------------------------------
  ! Destructor of the class particle
  !--------------------------------
  IMPLICIT NONE
  TYPE(particle_type), INTENT(inout) :: this
  
  IF (ALLOCATED(this%pos)) THEN
     DEALLOCATE(this%pos)
  ENDIF
  
  IF (ALLOCATED(this%vel)) THEN
     DEALLOCATE(this%vel)
  ENDIF

  IF (ALLOCATED(this%force)) THEN
     DEALLOCATE(this%force)
  ENDIF

  IF (ALLOCATED(this%acc)) THEN
     DEALLOCATE(this%acc)
  ENDIF

  IF (ALLOCATED(this%neigh_list)) THEN
     DEALLOCATE(this%neigh_list)
  ENDIF

  IF (ALLOCATED(this%pos0)) THEN
     DEALLOCATE(this%pos0)
  ENDIF

  IF (ALLOCATED(this%Spp)) THEN
     DEALLOCATE(this%Spp)
  ENDIF

  IF (ALLOCATED(this%Spp_rep)) THEN
     DEALLOCATE(this%Spp_rep)
  ENDIF

  IF (ALLOCATED(this%Spp_lub_norm)) THEN
     DEALLOCATE(this%Spp_lub_norm)
  ENDIF

  IF (ALLOCATED(this%Spp_lub_tang)) THEN
     DEALLOCATE(this%Spp_lub_tang)
  ENDIF    

END SUBROUTINE particle_destructor
