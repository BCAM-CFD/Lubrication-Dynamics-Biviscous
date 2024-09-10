!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE stress(this, error_out)
  !-----------------------------------
  ! The average stress tensor on the particles in the bulk is 
  ! computed in order to calculate the normal stress diferences.
  ! The stress is calculated with the Irving-Kirkwood method.
  ! Check Bertevas et al, Rheological acta, 2010 and
  ! Phan-Thien et al, Journal of rheology, 2014.
  ! **** Previously to compute the stress, the particles
  ! from the bulk should be calculated ***
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
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vij
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: vi
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub_norm
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Flub_tang
  REAL(Pr), DIMENSION(:,:), ALLOCATABLE :: Spp
  REAL(Pr) :: eij_vij
  REAL(Pr) :: F0
  REAL(Pr) :: exp_taus
  REAL(Pr) :: s
  REAL(Pr) :: R
  REAL(Pr) :: fij1
  REAL(Pr) :: fij2
  REAL(Pr) :: gij
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_stress.f90'  

  !-- Eqs (5) from JNNFM (for equal radius) --
  R = this%part(1)%R
  fij1 = -6.0_Pr * this%pi * this%eta0 * R**2.0_Pr / 4.0_Pr
  fij2 = -6.0_Pr * this%pi * this%eta0 * R * 9.0_Pr / 40.0_Pr
  gij   = -6.0_Pr * this%pi * this%eta0 * R / 6.0_Pr

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))
  ALLOCATE(eij(dim))
  ALLOCATE(Fij(dim))
  ALLOCATE(vij(dim))
  ALLOCATE(vi(dim))
  ALLOCATE(Flub_norm(dim))
  ALLOCATE(Flub_tang(dim))
  ALLOCATE(Spp(dim,dim))

  Box(:) = this%L(:)

  !-- Forces are initialized ---
  DO I = 1, this%N
     this%part(I)%Spp(:,:) = 0.0_Pr
  ENDDO

  !-- Forces and stresses are calculated --
  DO I = 1, this%N
     DO K = 1, this%part(I)%N_neigh
        J = this%part(I)%neigh_list(K)
        IF ((this%part(I)%bulk) .OR. (this%part(J)%bulk)) THEN

           include 'inc_calculate_Rij_sq.f90'

           IF (Rij_sq .LE. this%rcut_sq) THEN
              Rij = SQRT(Rij_sq) 
              eij(:) = pos_ij(:) / Rij
              IF (Rij < this%rcut_on) THEN
                 Rij = this%rcut_on
              ENDIF
              vij(:) = this%part(I)%vel(:) - this%part(J)%vel(:)
              eij_vij = DOT_PRODUCT(eij, vij)

              s = Rij - this%part(I)%R - this%part(J)%R
              IF (s .LE. 0) THEN
                 CALL error_header(file_name)                 
                 WRITE(*,*) '*** stress error: particles',I,'and', J,&
                      'are penetrating one to each other. ***'
                 error_out = 1
                 GOTO 1000 !-- End of subroutine --
              ENDIF
              exp_taus = EXP(-this%tau_rep * s)
              F0 = this%F0_rep * this%tau_rep * exp_taus / &
                   (1.0_Pr - exp_taus)
              Fij(:) = F0 * eij(:)
              !-- Lubrication force --
              Flub_norm(:) = (fij1/s + fij2 * &
                   LOG(this%part(I)%R / s)) * eij_vij * eij(:) 
              Flub_tang(:) = gij * LOG(this%part(I)%R / s) * &
                   (vij(:) - eij_vij * eij(:))
              Fij(:) = Fij(:) + Flub_norm(:) + Flub_tang(:)
              DO L = 1, dim
                 Spp(L,:) = pos_ij(L) * Fij(:)
              ENDDO
              this%part(I)%Spp = this%part(I)%Spp + Spp
              this%part(J)%Spp = this%part(J)%Spp + Spp
           ENDIF
        ENDIF

     ENDDO
  ENDDO

  !-- The stress tensor is calculated ---
  this%Spp(:,:) = 0.0_Pr
  DO I = 1, this%N
     IF (this%part(I)%bulk) THEN
        vi(:) = this%part(I)%vel(:)
        !--- Corrected by Sagaya ---
!!$        vi(1) = vi(1) - (this%wall%vel_bottom(1) - &
!!$             this%gamma_dot * this%part(I)%pos(dim))
        vi(1) = vi(1) - (this%wall%vel_bottom(1) + &
             this%gamma_dot * this%part(I)%pos(dim))
        !-------------------------
        DO J = 1, dim
           Spp(J,:) = vi(J) * vi(:) * this%part(I)%mass 
        ENDDO
        this%Spp(:,:) = this%Spp(:,:) + &
             0.5_Pr * this%part(I)%Spp(:,:) + Spp(:,:)
     ENDIF
  ENDDO
  this%Spp = -this%Spp / this%V_bulk
  
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
  IF (ALLOCATED(vij)) THEN
     DEALLOCATE(vij)
  ENDIF
  IF (ALLOCATED(vi)) THEN
     DEALLOCATE(vi)
  ENDIF
  IF (ALLOCATED(Flub_norm)) THEN
     DEALLOCATE(Flub_norm)
  ENDIF
  IF (ALLOCATED(Flub_tang)) THEN
     DEALLOCATE(Flub_tang)
  ENDIF


END SUBROUTINE stress
