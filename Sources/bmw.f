!*******************************************************************************
!>  @file bmw.f
!>  @brief Contains the main routines for Biot-Savart Magnetic Vmec
!>  Vector-Potential (BMW) code.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  BMW is a code for extending fields belond the VMEC domain in a manner that
!>  ensures divergence free fields. BMW does a Biot-Savart volume integration of
!>  of the equilibrium current density to obtain a continous vector potential
!>  every where on the mgrid grid.
!>
!>  @author Mark Cianciosa
!>
!>  Below is a brief discription of the major top level objects of the code. For
!>  discriptions of lower level objects consult the referenced top level
!>  objects.
!>
!>  @ref m_grid        Object containing the vaccum field information.
!>  @ref primed_grid   Object containing the plasma currents and primed grid
!>                     positions.
!>  @ref unprimed_grid Object containing the plasma vector potential response.
!>  @ref bmw_context   Defines the main pointers to all allocated memory and
!>                     objects.
!*******************************************************************************
!*******************************************************************************
!  MAIN PROGRAM
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief BMW main program.
!>
!>  Highest level BMW routine. This computes the vector potential on the mgrid
!>  grid.
!-------------------------------------------------------------------------------
      PROGRAM bmw
      USE bmw_context
      USE bmw_commandline_parser
      USE, INTRINSIC :: iso_fortran_env, Only : output_unit
      USE bmw_state_flags
      USE safe_open_mod

      IMPLICIT NONE

!  local variables
      CLASS (bmw_parallel_context_class), POINTER :: parallel => null()
      CLASS (bmw_context_class), POINTER            :: context => null()
      CLASS (bmw_commandline_parser_class), POINTER ::                         &
     &   cl_parser => null()
      INTEGER                                       :: flags
      INTEGER                                       :: num_p
      INTEGER                                       :: io_unit
      INTEGER                                       :: status
      REAL (rprec)                                  :: start_time

!  Start of executable code
#if defined (MPI_OPT)
      CALL MPI_INIT(status)
#endif
      CALL profiler_construct

      start_time = profiler_get_start_time()

      parallel => bmw_parallel_context_class(                                  &
#if defined (MPI_OPT)
     &                                       MPI_COMM_WORLD                    &
#endif
     &                                       )

      cl_parser => bmw_commandline_parser_class(parallel)

!  Check if the required flags are set.
!      IF (.not.cl_parser%is_flag_set('-mgridf')) THEN
!         WRITE (*,1001) '-mgridf'
!         CALL bmw_commandline_parser_print_help
!      END IF
      IF (.not.cl_parser%is_flag_set('-woutf')) THEN
         WRITE (*,1001) '-woutf'
         CALL bmw_commandline_parser_print_help
      END IF
      IF (.not.cl_parser%is_flag_set('-outf')) THEN
         WRITE (*,1001) '-outf'
         CALL bmw_commandline_parser_print_help
      END IF

      CALL parallel%set_threads(cl_parser%get('-para', 1))

      flags = bmw_state_flags_off
      IF (cl_parser%is_flag_set('-force')) THEN
         flags = IBSET(flags, bmw_state_flags_force)
      END IF
      IF (cl_parser%is_flag_set('-ju')) THEN
         flags = IBSET(flags, bmw_state_flags_ju)
      END IF
      IF (cl_parser%is_flag_set('-jv')) THEN
         flags = IBSET(flags, bmw_state_flags_jv)
         flags = IBCLR(flags, bmw_state_flags_ju)
      END IF
      IF (cl_parser%is_flag_set('-siestaf')) THEN
         flags = IBSET(flags, bmw_state_flags_siesta)
         flags = IBCLR(flags, bmw_state_flags_ju)
         flags = IBCLR(flags, bmw_state_flags_jv)
      END IF
      IF (cl_parser%is_flag_set('-wvacf')) THEN
         flags = IBSET(flags, bmw_state_flags_correct)
         flags = IBCLR(flags, bmw_state_flags_siesta)
      END IF

      flags = IBSET(flags, bmw_state_flags_mgrid)
      num_p = 0

      io_unit = output_unit
      IF (cl_parser%is_flag_set('-logf')) THEN
         CALL safe_open(io_unit, status, cl_parser%get_string('-logf'),        &
     &                  'replace', 'formatted', delim_in='none')
      END IF

      IF (parallel%offset .eq. 0) THEN
         WRITE (io_unit,*)
         WRITE (io_unit,1000) series
         WRITE (io_unit,*)
      END IF
      CALL parallel%report(io_unit)

      IF (cl_parser%is_flag_set('-mgridf')) THEN
         context => bmw_context_class(cl_parser%get('-mgridf'),                &
     &                                cl_parser%get('-woutf'),                 &
     &                                cl_parser%get('-siestaf'),               &
     &                                flags, num_p, parallel, io_unit)
      ELSE
         num_p = cl_parser%get('-num_p', 1)
         context => bmw_context_class(cl_parser%get('-woutf'),                 &
     &                                cl_parser%get('-siestaf'), flags,        &
     &                                cl_parser%get('-num_r', 1), num_p,       &
     &                                cl_parser%get('-num_z', 1),              &
     &                                cl_parser%get('-rmax', 0.0_dp),          &
     &                                cl_parser%get('-rmin', 0.0_dp),          &
     &                                cl_parser%get('-zmax', 0.0_dp),          &
     &                                cl_parser%get('-zmin', 0.0_dp),          &
     &                                parallel, io_unit)
      END IF
      IF (cl_parser%is_flag_set('-wvacf')) THEN
         CALL context%set_up_grid(cl_parser%get('-wvacf'),                     &
     &                            num_p*context%m_grid%nfp, flags,             &
     &                            cl_parser%get('-p_start', -1),               &
     &                            cl_parser%get('-p_end', -1),                 &
     &                            parallel, io_unit)
      ELSE
         CALL context%set_up_grid(cl_parser%get('-p_start', -1),               &
     &                            cl_parser%get('-p_end', -1),                 &
     &                            parallel, io_unit)
      END IF
      CALL context%write(cl_parser%get('-outf'), parallel)

      DEALLOCATE(context)
      DEALLOCATE(cl_parser)

      CALL profiler_set_stop_time('bmw_main', start_time)
      IF (parallel%offset .eq. 0) THEN
         CALL profiler_write(io_unit)
      END IF
      CALL profiler_destruct

      CLOSE(io_unit)

      DEALLOCATE(parallel)

1000  FORMAT('BMW ',i4,' Series.')
1001  FORMAT('Required flag ',a,' not set.')

      END PROGRAM
