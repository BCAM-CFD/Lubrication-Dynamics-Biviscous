!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE forces(this, error_out)
  !-----------------------------------
  ! Calculations of forces on particles (only repulsion forces).
  ! See Vazquez-Quesada et al, Journal of Non-Newtonian Fluid Mechanics, 2016.
  !-----------------------------------
  IMPLICIT NONE
  TYPE(system_type), intent(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: I, J, K, L
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box
  INTEGER :: dim
  REAL(Pr) :: Rij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: eij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Fij
  REAL(Pr) :: F0
  REAL(Pr) :: exp_taus
  REAL(Pr) :: s
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_forces.f90'  

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(Fij(dim))

  Box(:) = this%L(:)

  !-- Forces are put to zero --
  DO I = 1, this%N
     this%part(I)%force(:)         = 0.0_Pr
     this%wall%rep_force_top(:)    = 0.0_Pr
     this%wall%rep_force_bottom(:) = 0.0_Pr
  ENDDO

  !-- The forces are computed --
  DO I = 1, this%N
     DO K = 1, this%part(I)%N_neigh
        J = this%part(I)%neigh_list(K)
        
        include 'inc_calculate_Rij_sq.f90'

        IF (Rij_sq .LE. this%rcut_sq) THEN
           Rij = SQRT(Rij_sq) 
           eij(:) = pos_ij(:) / Rij
           IF (Rij < this%part(I)%R + this%part(J)%R) THEN
              CALL error_header(file_name)              
              WRITE(*,*) '*** forces error: particles',I,'and', J,&
                   'are penetrating one to each other. ***'
              error_out = 1
              GOTO 1000 !-- End of subroutine --
           ENDIF
           IF (Rij < this%rcut_on) THEN
              Rij = this%rcut_on
           ENDIF

           s = Rij - this%part(I)%R - this%part(J)%R
           !************ IMPORTANT: Note that the definition of tau and s 
           ! is different in here than in Bertevas et al 2011 *******
           exp_taus = EXP(-this%tau_rep * s)
           F0 = this%F0_rep * this%tau_rep * exp_taus / &
                (1.0_Pr - exp_taus)
           Fij(:) = F0 * eij(:)
           this%part(I)%force(:) = this%part(I)%force(:) + Fij(:)
           this%part(J)%force(:) = this%part(J)%force(:) - Fij(:)
        ENDIF
     ENDDO

     !-- Top wall --
     IF (this%part(I)%pos(dim) > this%L(dim) - 0.5_Pr * this%rcut) THEN
        Rij    = this%L(dim) - this%part(I)%pos(dim)
        eij(:) = 0.0_Pr
        eij(dim) = -1.0_Pr
        IF (Rij < this%part(I)%R) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** forces error: top wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
        
        s = Rij - this%part(I)%R
        
        IF (s < this%rcut_on - 2.0_Pr * this%part(I)%R) THEN
           s = this%rcut_on - 2.0_Pr * this%part(I)%R
        ENDIF
        exp_taus = EXP(-this%tau_rep * s)
        F0 = this%F0_rep * this%tau_rep * exp_taus / &
             (1.0_Pr - exp_taus)
        Fij(:) = F0 * eij(:)
        
        this%part(I)%force(:)  = this%part(I)%force(:)  + Fij(:)
        this%wall%rep_force_top(:) = this%wall%rep_force_top(:) - Fij(:)
     ENDIF

     !-- Bottom wall --
     IF (this%part(I)%pos(dim) < 0.5_Pr * this%rcut) THEN
        Rij    = this%part(I)%pos(dim)
        eij(:) = 0.0_Pr
        eij(dim) = 1.0_Pr
        IF (Rij < this%part(I)%R) THEN
           CALL error_header(file_name)           
           WRITE(*,*) '*** forces error: bottom wall is being penetrated. ***'
           error_out = 1
           GOTO 1000 !-- End of subroutine --
        ENDIF
        
        s = Rij - this%part(I)%R
        
        IF (s < this%rcut_on - 2.0_Pr * this%part(I)%R) THEN
           s = this%rcut_on - 2.0_Pr * this%part(I)%R
        ENDIF
        exp_taus = EXP(-this%tau_rep * s)
        F0 = this%F0_rep * this%tau_rep * exp_taus / &
             (1.0_Pr - exp_taus)
        Fij(:) = F0 * eij(:)
        this%part(I)%force(:)     = this%part(I)%force(:)     + Fij(:)
        this%wall%rep_force_bottom(:) = this%wall%rep_force_bottom(:) - Fij(:)
     ENDIF

  ENDDO

  ! --- Accelerations of the particles are calculated ---
  DO I = 1, this%N
     this%part(I)%acc(:) = this%part(I)%force(:) / this%part(I)%mass
  ENDDO

  !-- The total force on the walls is calculated --
  this%wall%force_top(:) = this%wall%rep_force_top(:) + &
       this%wall%lub_force_top(:)
  this%wall%force_bottom(:) = this%wall%rep_force_bottom(:) + &
       this%wall%lub_force_bottom(:)


1000 CONTINUE

  IF (ALLOCATED(Box)) THEN
     DEALLOCATE(Box)
  ENDIF
  IF (ALLOCATED(pos_ij)) THEN
     DEALLOCATE(pos_ij)
  ENDIF
  IF (ALLOCATED(eij)) THEN
     DEALLOCATE(eij)
  ENDIF
  IF (ALLOCATED(Fij)) THEN
     DEALLOCATE(Fij)
  ENDIF
END SUBROUTINE forces
