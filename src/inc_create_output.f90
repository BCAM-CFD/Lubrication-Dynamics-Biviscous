!---------------------------------------------------------------------
! This code has been developed  in collaboration between
!  - Marco Ellero, leader of  the CFD  Modelling and Simulation  group at  BCAM (Basque
!    Center  for  Applied  Mathematics)  in  Bilbao,  Spain.
!  - Adolfo Vazquez-Quesada from the Department  of Fundamental Physics at UNED,
!    in Madrid, Spain.
! Developer: Adolfo Vazquez-Quesada.
!---------------------------------------------------------------------
!-------------------------------
  SUBROUTINE create_output(this, dir, freq_write, freq_write_part, error_out)
!-------------------------------
    ! Creator of the class object 
    !---------------------------
    use class_files_utilities
    IMPLICIT NONE
    TYPE(output_type), INTENT(inout)    :: this
    CHARACTER(LEN=MAX_CHAR), INTENT(in) :: dir
    INTEGER, INTENT(in)                 :: freq_write
    INTEGER, INTENT(in)                 :: freq_write_part
    INTEGER, INTENT(out)                :: error_out
    
    error_out = 0
    
    !-- Output directory is created --
    this%dir = dir
    CALL SYSTEM('mkdir '//dir)

    !-- info file is created --
    this%info%name = trim(this%dir)//'/info.dat'

    !-- walls file is created --
    this%walls%name = trim(this%dir)//'/walls.dat'
    CALL search_unit(this%walls%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    OPEN(this%walls%unit, FILE=trim(this%walls%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    !-- shear_rate file is created --
    this%shear_rate%name = trim(this%dir)//'/shear_rate.dat'
    CALL search_unit(this%shear_rate%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    OPEN(this%shear_rate%unit, FILE=trim(this%shear_rate%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    !-- Nsweeps file is created --
    this%Nsweeps%name = trim(this%dir)//'/Nsweeps.dat'
    CALL search_unit(this%Nsweeps%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    OPEN(this%Nsweeps%unit, FILE=trim(this%Nsweeps%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    !-- stress file is created --
    this%stress%name = trim(this%dir)//'/stress.dat'
    CALL search_unit(this%stress%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    OPEN(this%stress%unit, FILE=trim(this%stress%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    !-- Computational times file is created --
    this%comp_time%name = trim(this%dir)//'/comp_time.dat'
    CALL search_unit(this%comp_time%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    OPEN(this%comp_time%unit, FILE=trim(this%comp_time%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')
    
    !-- particles file is created --
    WRITE(*,*) 'mkdir '//trim(dir)//'/particles'
    CALL SYSTEM('mkdir '//trim(dir)//'/particles')
    this%particles%base_name = trim(this%dir)//'/particles/particles'
    this%particles%name = &
         this%particles%base_name !-- Only for info purposes --

    !-- plane file is created --
    this%plane%name = trim(this%dir)//'/plane.dat'
    CALL search_unit(this%plane%unit, error_out)
    IF (error_out .NE. 0) THEN
       GOTO 1000 !-- End of subroutine --
    ENDIF
    OPEN(this%plane%unit, FILE=trim(this%plane%name), &
         FORM='FORMATTED', STATUS='UNKNOWN')

    this%freq_write      = freq_write
    this%freq_write_part = freq_write_part

1000 CONTINUE
    
  END SUBROUTINE create_output
