!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!---------------------------------------------
SUBROUTINE Calculate_neigh_list(this, error_out)
  !-----------------------------------------------
  ! This subroutine calculates the list of neighbours
  ! of every particle.
  !-----------------------------------------------
  IMPLICIT NONE
  TYPE(system_type), INTENT(inout) :: this
  INTEGER, INTENT(out)             :: error_out
  INTEGER :: M, N, P, Q, R, S
  INTEGER :: I, J
  INTEGER, DIMENSION(3) :: neigh_coord
  REAL(Pr) :: Rij_sq
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: pos_ij(:)
  REAL(Pr), DIMENSION(:), ALLOCATABLE :: Box(:)
  INTEGER :: dim
  CHARACTER(LEN=MAX_CHAR) :: file_name !-- Source file name --  

  error_out = 0

  file_name = 'inc_calculate_neigh_list.f90'  

  dim = this%dim
  ALLOCATE(pos_ij(dim))
  ALLOCATE(Box(dim))

  Box(:) = this%L(:)

  !--- The list of neighbours is inicialized ---
  this%part(:)%N_neigh = 0

  !--- The neighbours of every fluid particle are found ---
  IF (dim == 2) THEN
     DO M = 1, this%Ncells(1)
        DO N = 1, this%Ncells(2)
           DO P = 1, this%cell(M,N,1)%Npart
              I = this%cell(M,N,1)%list_part(P)
              DO R = 1, this%cell(M,N,1)%N_neigh
                 neigh_coord(1:dim) = this%cell(M,N,1)%neigh_coord(R,1:dim)
                 DO S = 1, this%cell(neigh_coord(1), neigh_coord(2), 1)%Npart
                    J  = this%cell(neigh_coord(1), neigh_coord(2), 1)%list_part(S)
                    IF (I < J) THEN
                       include 'inc_calculate_Rij_sq.f90'
                       IF (Rij_sq .LE. this%Rmax_sq) THEN
                          this%part(I)%N_neigh = this%part(I)%N_neigh + 1
                          this%part(I)%neigh_list(this%part(I)%N_neigh) = J
                       ENDIF
                    ENDIF
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ELSE !-- dim == 3 --
     DO M = 1, this%Ncells(1)
        DO N = 1, this%Ncells(2)
           DO Q = 1, this%Ncells(3)
              DO P = 1, this%cell(M,N,Q)%Npart
                 I = this%cell(M,N,Q)%list_part(P)
                 DO R = 1, this%cell(M,N,Q)%N_neigh
                    neigh_coord(1:dim) = this%cell(M,N,Q)%neigh_coord(R, 1:dim)
                    DO S = 1, this%cell(neigh_coord(1), neigh_coord(2), &
                         neigh_coord(3))%Npart
                       J  = this%cell(neigh_coord(1), neigh_coord(2), &
                            neigh_coord(3))%list_part(S)
                       IF (I < J) THEN
                          include 'inc_calculate_Rij_sq.f90'
                          IF (Rij_sq .LE. this%Rmax_sq) THEN
                             this%part(I)%N_neigh = this%part(I)%N_neigh + 1
                             IF (this%part(I)%N_neigh > this%Nmax_list) THEN
                                CALL error_header(file_name)                                
                                WRITE(*,*) '*** Calculate neigh list error: too much neighbours. ***'
                                error_out = 1
                                GOTO 1000 !-- End of subroutine --
                             ENDIF
                             this%part(I)%neigh_list(this%part(I)%N_neigh) = J
                          ENDIF
                       ENDIF
                    ENDDO
                 ENDDO
              ENDDO
           ENDDO
        ENDDO
     ENDDO
  ENDIF

1000 CONTINUE
  
  IF (ALLOCATED(pos_ij)) THEN
     DEALLOCATE(pos_ij)
  ENDIF
  IF (ALLOCATED(Box)) THEN
     DEALLOCATE(Box)
  ENDIF
  
END SUBROUTINE Calculate_neigh_list
