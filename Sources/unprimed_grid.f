!*******************************************************************************
!>  @file unprimed_grid.f
!>  @brief Contains module @ref unprimed_grid.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref unprimed_grid_class. This contains
!>  the state variables to define the currents and positions of the volumn
!>  integral.
!*******************************************************************************
      MODULE unprimed_grid
      USE stel_kinds, ONLY: rprec
      USE profiler
      USE bmw_parallel_context
      USE m_grid
      USE primed_grid

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) unprimed grid base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a unprimed grid. This is grid the volume integral
!>  will be summed over.
!-------------------------------------------------------------------------------
      TYPE :: unprimed_grid_class
!>  Vector potential in the R direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: a_r => null()
!>  Vector potential in the Phi direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: a_p => null()
!>  Vector potential in the Z direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: a_z => null()

!>  B field in the R direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: b_r => null()
!>  B field in the Phi direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: b_p => null()
!>  B field in the Z direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: b_z => null()
      CONTAINS
         FINAL :: unprimed_grid_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Interface to constructors.
!-------------------------------------------------------------------------------
      INTERFACE unprimed_grid_class
         MODULE PROCEDURE unprimed_grid_construct_m,                           &
     &                    unprimed_grid_construct_a,                           &
     &                    unprimed_grid_construct_c
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref unprimed_grid_class object.
!>
!>  Allocates memory and initializes a @ref unprimed_grid_class object. This
!>  computes the vector potential, magnetic field and positions on the unprimed
!>  grid. Magnetic field components are provided on a truncated grid. Due to the
!>  central differencing, field componetents cannot be provided for first and
!>  last points on the r and z grids.
!>
!>  @param[in] mgrid    A @ref m_grid::m_grid_class object.
!>  @param[in] pgrid    A @ref primed_grid::primed_grid_class object.
!>  @param[in] p_start  Starting phi index to compute fields to.
!>  @param[in] p_end    Ending phi index to compute fields to.
!>  @param[in] parallel @ref bmw_parallel_context::bmw_parallel_context_class
!>                      object instance.
!>  @param[in] io_unit  Unit number to write messages to.
!>  @returns A pointer to a constructed @ref unprimed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION unprimed_grid_construct_m(mgrid, pgrid, p_start, p_end,         &
     &                                   parallel, io_unit)
!$    USE omp_lib
      USE bmw_state_flags, ONLY: clear_screen, progress
      USE, INTRINSIC :: iso_fortran_env, Only : output_unit

      IMPLICIT NONE

!  Declare Arguments
      CLASS (unprimed_grid_class), POINTER :: unprimed_grid_construct_m
      CLASS (m_grid_class), INTENT(in)               :: mgrid
      CLASS (primed_grid_class), INTENT(in)          :: pgrid
      INTEGER, INTENT(in)                            :: p_start
      INTEGER, INTENT(in)                            :: p_end
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: i
      INTEGER                                        :: ri
      INTEGER                                        :: zi
      INTEGER                                        :: vi
      INTEGER                                        :: num_r
      INTEGER                                        :: num_p
      INTEGER                                        :: num_z
      REAL (rprec)                                   :: x
      REAL (rprec)                                   :: y
      REAL (rprec)                                   :: ax
      REAL (rprec)                                   :: ay
      INTEGER                                        :: k_p
      INTEGER                                        :: k_m
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: r
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: z
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: r_p
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: r_m
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosv
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinv
      REAL (rprec)                                   :: total
      REAL (rprec)                                   :: done
      REAL (rprec)                                   :: current
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: rp
      REAL (rprec)                                   :: ar_p
      REAL (rprec)                                   :: ar_m
      REAL (rprec)                                   :: ap_p
      REAL (rprec)                                   :: ap_m
      REAL (rprec)                                   :: az_p
      REAL (rprec)                                   :: az_m

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(unprimed_grid_construct_m)

      num_r = SIZE(mgrid%a_r, 1)
      num_p = SIZE(mgrid%a_p, 3)
      num_z = SIZE(mgrid%a_z, 2)

      ALLOCATE(unprimed_grid_construct_m%a_r(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_m%a_p(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_m%a_z(num_r,num_z,num_p))

      ALLOCATE(unprimed_grid_construct_m%b_r(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_m%b_p(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_m%b_z(num_r,num_z,num_p))

      ALLOCATE(cosv(num_p))
      ALLOCATE(sinv(num_p))

      ALLOCATE(r(num_r))
      ALLOCATE(z(num_z))
      ALLOCATE(r_p(num_r))
      ALLOCATE(r_m(num_r))

      total = parallel%end(num_r*num_z*num_p)
      total = CEILING(total/parallel%num_threads)

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, ri, zi, vi, x, y, ax, ay, rp, k_p, k_m,                      &
!$OMP&         ar_p, ar_m, ap_p, ap_m, az_p, az_m, current)

!  Multi process will do an all reduce so these arrays need to be initalized.
      IF (parallel%stride .gt. 1) THEN
!$OMP WORKSHARE
         unprimed_grid_construct_m%a_r = 0.0
         unprimed_grid_construct_m%a_p = 0.0
         unprimed_grid_construct_m%a_z = 0.0
         unprimed_grid_construct_m%b_r = 0.0
         unprimed_grid_construct_m%b_p = 0.0
         unprimed_grid_construct_m%b_z = 0.0
         cosv = 0.0
         sinv = 0.0
         r = 0.0
         z = 0.0
         r_p = 0.0
         r_m = 0.0
!$OMP END WORKSHARE
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_r), parallel%end(num_r)
         r(i) = (i - 1.0)*mgrid%dr + mgrid%rmin
         r_p(i) = r(i) + mgrid%dr
         r_m(i) = r(i) - mgrid%dr
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_z), parallel%end(num_z)
         z(i) = (i - 1.0)*mgrid%dz + mgrid%zmin
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_p), parallel%end(num_p)
         x = (i - 1.0)*pgrid%dv
         cosv(i) = COS(x)
         sinv(i) = SIN(x)
      END DO
!$OMP END DO

!$OMP SINGLE
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(cosv)
         CALL parallel%reduce(sinv)
         CALL parallel%reduce(r)
         CALL parallel%reduce(z)
         CALL parallel%reduce(r_p)
         CALL parallel%reduce(r_m)
      END IF
!$OMP END SINGLE

      current = 0.0

      ALLOCATE(rp(SIZE(pgrid%x, 1),                                            &
     &            SIZE(pgrid%x, 2),                                            &
     &            SIZE(pgrid%x, 3)))

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_r*num_z*num_p),                                &
     &       parallel%end(num_r*num_z*num_p)
         ri = bmw_parallel_context_i(i, num_r)
         zi = bmw_parallel_context_j(i, num_r, num_z)
         vi = bmw_parallel_context_k(i, num_r, num_z)

         IF (parallel%offset .eq. 0) THEN
!$          IF (OMP_GET_THREAD_NUM() .eq. 0) THEN
               current = current + 1.0
               done = 100.0*current/total

               WRITE (io_unit,1000,ADVANCE='NO')                               &
     &            clear_screen, progress(MOD(INT(current),4)), done

               IF (io_unit .ne. output_unit) THEN
                  BACKSPACE (io_unit)
               END IF
!$          END IF
         END IF

         x = r(ri)*cosv(vi)
         y = r(ri)*sinv(vi)

         rp = SQRT((pgrid%x - x)**2.0 + (pgrid%y - y)**2.0 +                   &
     &             (pgrid%z - z(zi))**2.0)

         ax = SUM(pgrid%j_x/rp)*pgrid%dvol
         ay = SUM(pgrid%j_y/rp)*pgrid%dvol

         unprimed_grid_construct_m%a_r(ri,zi,vi) =                             &
     &       x/r(ri)*ax + y/r(ri)*ay + mgrid%a_r(ri,zi,vi)
         unprimed_grid_construct_m%a_p(ri,zi,vi) =                             &
     &      -y/r(ri)*ax + x/r(ri)*ay + mgrid%a_p(ri,zi,vi)
         unprimed_grid_construct_m%a_z(ri,zi,vi) =                             &
     &      SUM(pgrid%j_z/rp)*pgrid%dvol + mgrid%a_z(ri,zi,vi)
      END DO
!$OMP END DO

      DEALLOCATE(rp)

!$OMP SINGLE
!  Multi process did not fill out the entire array. Get the missing pieces from
!  the other processes.
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(unprimed_grid_construct_m%a_r)
         CALL parallel%reduce(unprimed_grid_construct_m%a_p)
         CALL parallel%reduce(unprimed_grid_construct_m%a_z)
      END IF
!$OMP END SINGLE

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_p*num_r*num_z),                                &
     &       parallel%end(num_p*num_r*num_z)
         ri = bmw_parallel_context_i(i, num_r)
         zi = bmw_parallel_context_j(i, num_r, num_z)
         vi = bmw_parallel_context_k(i, num_r, num_z)

         IF (ri .eq. 1 .or. ri .eq. num_r .or.                                 &
     &       zi .eq. 1 .or. zi .eq. num_z) THEN
            unprimed_grid_construct_m%b_r(ri,zi,vi) = 0.0
            unprimed_grid_construct_m%b_p(ri,zi,vi) = 0.0
            unprimed_grid_construct_m%b_z(ri,zi,vi) = 0.0
         ELSE
            IF (num_p .eq. 1) THEN
               k_p = 1
               k_m = 1
            ELSE
               IF (vi .eq. 1) THEN
                  k_p = 2
                  k_m = num_p
               ELSE IF (vi .eq. num_p) THEN
                  k_p = 1
                  k_m = num_p - 1
               ELSE
                  k_p = vi + 1
                  k_m = vi - 1
               END IF
            END IF

! 1/rdazdp - dapdz
            az_p = unprimed_grid_construct_m%a_z(ri,zi,k_p)
            az_m = unprimed_grid_construct_m%a_z(ri,zi,k_m)
            ap_p = unprimed_grid_construct_m%a_p(ri,zi + 1,vi)
            ap_m = unprimed_grid_construct_m%a_p(ri,zi - 1,vi)
            unprimed_grid_construct_m%b_r(ri,zi,vi) =                          &
     &         (az_p - az_m)/(2.0*pgrid%dv*r(ri)) -                            &
     &         (ap_p - ap_m)/(2.0*mgrid%dz)

! dardz - dazdr
            ar_p = unprimed_grid_construct_m%a_r(ri,zi + 1,vi)
            ar_m = unprimed_grid_construct_m%a_r(ri,zi - 1,vi)
            az_p = unprimed_grid_construct_m%a_z(ri + 1,zi,vi)
            az_m = unprimed_grid_construct_m%a_z(ri - 1,zi,vi)
            unprimed_grid_construct_m%b_p(ri,zi,vi) =                          &
     &         (ar_p - ar_m)/(2.0*mgrid%dz) -                                  &
     &         (az_p - az_m)/(2.0*mgrid%dr)

! 1/r(drapdr - dardp)
            ar_p = unprimed_grid_construct_m%a_r(ri,zi,k_p)
            ar_m = unprimed_grid_construct_m%a_r(ri,zi,k_m)
            ap_p = unprimed_grid_construct_m%a_p(ri + 1,zi,vi)
            ap_m = unprimed_grid_construct_m%a_p(ri - 1,zi,vi)
            unprimed_grid_construct_m%b_z(ri,zi,vi) =                          &
     &         (r_p(ri)*ap_p - r_m(ri)*ap_m)/(2.0*mgrid%dr*r(ri)) -            &
     &         (ar_p - ar_m)/(2.0*pgrid%dv*r(ri))

         END IF
      END DO
!$OMP END DO
!$OMP END PARALLEL

      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(unprimed_grid_construct_m%b_r)
         CALL parallel%reduce(unprimed_grid_construct_m%b_p)
         CALL parallel%reduce(unprimed_grid_construct_m%b_z)
      END IF

      DEALLOCATE(cosv)
      DEALLOCATE(sinv)
      DEALLOCATE(r)
      DEALLOCATE(z)
      DEALLOCATE(r_p)
      DEALLOCATE(r_m)

      IF (parallel%offset .eq. 0) THEN
         WRITE (io_unit,1001) clear_screen
      END IF

      CALL profiler_set_stop_time('unprimed_grid_construct_m',                 &
     &                            start_time)

1000  FORMAT(a,a,f6.2,' % Finished')
1001  FORMAT(a,'Unprimed Grid Finished')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref unprimed_grid_class object at specfied points.
!>
!>  Allocates memory and initializes a @ref unprimed_grid_class object. This
!>  computes the vector potential, magnetic field and positions on the unprimed
!>  grid. The primed grid is supplied as arbitrary r and z positions with a
!>  uniformly spaced phi grid. Since the primed grid is arbitrary, this function
!>  does not compute the magnetic field components. The middle index represents
!>  phi positions.
!>
!>  @param[in] mgrid    A @ref m_grid::m_grid_class object.
!>  @param[in] pgrid    A @ref primed_grid::primed_grid_class object.
!>  @param[in] r_grid   3D Array of r positions.
!>  @param[in] z_grid   3D Array of z positions.
!>  @param[in] dphi     Phi grid spacing.
!>  @param[in] parallel @ref bmw_parallel_context::bmw_parallel_context_class
!>                      object instance.
!>  @param[in] io_unit  Unit number to write messages to.
!>  @returns A pointer to a constructed @ref unprimed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION unprimed_grid_construct_a(mgrid, pgrid, r_grid, z_grid,         &
     &                                   dphi, parallel, io_unit)
!$    USE omp_lib
      USE bmw_state_flags, ONLY: clear_screen, progress
      USE, INTRINSIC :: iso_fortran_env, Only : output_unit

      IMPLICIT NONE

!  Declare Arguments
      CLASS (unprimed_grid_class), POINTER :: unprimed_grid_construct_a
      CLASS (m_grid_class), INTENT(in)               :: mgrid
      CLASS (primed_grid_class), INTENT(in)          :: pgrid
      REAL (rprec), DIMENSION(:,:,:), INTENT(in)     :: r_grid
      REAL (rprec), DIMENSION(:,:,:), INTENT(in)     :: z_grid
      REAL (rprec), INTENT(in)                       :: dphi
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: num_1
      INTEGER                                        :: num_2
      INTEGER                                        :: num_3
      REAL (rprec)                                   :: current
      REAL (rprec)                                   :: done
      REAL (rprec)                                   :: total
      INTEGER                                        :: i
      INTEGER                                        :: ai
      INTEGER                                        :: aj
      INTEGER                                        :: ak
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: rp
      REAL (rprec)                                   :: ax
      REAL (rprec)                                   :: ay
      REAL (rprec)                                   :: x
      REAL (rprec)                                   :: y
      REAL (rprec)                                   :: z
      REAL (rprec)                                   :: r
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: phi
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosv
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinv

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (MAXVAL(r_grid) .gt. mgrid%rmax .or.                                  &
     &    MINVAL(r_grid) .lt. mgrid%rmin .or.                                  &
     &    MAXVAL(z_grid) .gt. mgrid%zmax .or.                                  &
     &    MINVAL(z_grid) .lt. mgrid%zmin) THEN
         IF (parallel%offset .eq. 0) THEN
            WRITE (io_unit, 2000)
         END IF
         CALL bmw_parallel_context_abort(-1)
      END IF

      ALLOCATE(unprimed_grid_construct_a)

      num_1 = SIZE(r_grid, 1)
      num_2 = SIZE(r_grid, 2)
      num_3 = SIZE(r_grid, 3)

      ALLOCATE(unprimed_grid_construct_a%a_r(num_1,num_2,num_3))
      ALLOCATE(unprimed_grid_construct_a%a_p(num_1,num_2,num_3))
      ALLOCATE(unprimed_grid_construct_a%a_z(num_1,num_2,num_3))

      ALLOCATE(cosv(num_2))
      ALLOCATE(sinv(num_2))
      ALLOCATE(phi(num_2))

      total = parallel%end(num_1*num_2*num_3)
      total = CEILING(total/parallel%num_threads)

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, ai, aj, ak, r, x, y, z, ax, ay, current, rp)

!  Multi process will do an all reduce so these arrays need to be initalized.
      IF (parallel%stride .gt. 1) THEN
!$OMP WORKSHARE
         unprimed_grid_construct_a%a_r = 0.0
         unprimed_grid_construct_a%a_p = 0.0
         unprimed_grid_construct_a%a_z = 0.0
         cosv = 0.0
         sinv = 0.0
         phi = 0.0
!$OMP END WORKSHARE
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_2), parallel%end(num_2)
         phi(i) = (i - 1.0)*dphi
         cosv(i) = COS(phi(i))
         sinv(i) = SIN(phi(i))
      END DO
!$OMP END DO

!$OMP SINGLE
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(cosv)
         CALL parallel%reduce(sinv)
         CALL parallel%reduce(phi)
      END IF
!$OMP END SINGLE

      current = 0.0

      ALLOCATE(rp(SIZE(pgrid%x, 1),                                            &
     &            SIZE(pgrid%x, 2),                                            &
     &            SIZE(pgrid%x, 3)))

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_1*num_2*num_3),                                &
     &       parallel%end(num_1*num_2*num_3)
         ai = bmw_parallel_context_i(i, num_1)
         aj = bmw_parallel_context_j(i, num_1, num_2)
         ak = bmw_parallel_context_k(i, num_1, num_2)

         IF (parallel%offset .eq. 0) THEN
!$          IF (OMP_GET_THREAD_NUM() .eq. 0) THEN
               current = current + 1.0
               done = 100.0*current/total

               WRITE (io_unit,1000,ADVANCE='NO')                               &
     &            clear_screen, progress(MOD(INT(current),4)), done

               IF (io_unit .ne. output_unit) THEN
                  BACKSPACE (io_unit)
               END IF
!$          END IF
         END IF

         r = r_grid(ai,aj,ak)
         z = z_grid(ai,aj,ak)
         x = r*cosv(aj)
         y = r*sinv(aj)

         rp = SQRT((pgrid%x - x)**2.0 +                                        &
     &             (pgrid%y - y)**2.0 +                                        &
     &             (pgrid%z - z)**2.0)

         ax = SUM(pgrid%j_x/rp)*pgrid%dvol
         ay = SUM(pgrid%j_y/rp)*pgrid%dvol

!  Interpolate from the mgrid grid to the unprimed R Z grid to add the vacuum
!  vector potential.
         CALL mgrid%interpolate(r, phi(aj), z,                                 &
     &           unprimed_grid_construct_a%a_r(ai,aj,ak),                      &
     &           unprimed_grid_construct_a%a_p(ai,aj,ak),                      &
     &           unprimed_grid_construct_a%a_z(ai,aj,ak))

         unprimed_grid_construct_a%a_r(ai,aj,ak) =                             &
     &      unprimed_grid_construct_a%a_r(ai,aj,ak) + x/r*ax + y/r*ay
         unprimed_grid_construct_a%a_p(ai,aj,ak) =                             &
     &      unprimed_grid_construct_a%a_p(ai,aj,ak) - y/r*ax + x/r*ay
         unprimed_grid_construct_a%a_z(ai,aj,ak) =                             &
     &      unprimed_grid_construct_a%a_z(ai,aj,ak) +                          &
     &      SUM(pgrid%j_z/rp)*pgrid%dvol
      END DO
!$OMP END DO

      DEALLOCATE(rp)

!$OMP END PARALLEL

!  Multi process did not fill out the entire array. Get the missing pieces from
!  the other processes.
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(unprimed_grid_construct_a%a_r)
         CALL parallel%reduce(unprimed_grid_construct_a%a_p)
         CALL parallel%reduce(unprimed_grid_construct_a%a_z)
      END IF

      IF (parallel%offset .eq. 0) THEN
         WRITE (io_unit,1001) clear_screen
      END IF

      CALL profiler_set_stop_time('unprimed_grid_construct_a',                 &
     &                            start_time)

1000  FORMAT(a,a,f6.2,' % Finished')
1001  FORMAT(a,'Unprimed Grid Finished')

2000  FORMAT('Error: Unprimed grid extends beyond the mgrid grid.')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref unprimed_grid_class object.
!>
!>  Allocates memory and initializes a @ref unprimed_grid_class object. This
!>  computes the vector potential, magnetic field and positions on the unprimed
!>  grid. Magnetic field components are provided on a truncated grid. Due to the
!>  central differencing, field componetents cannot be provided for first and
!>  last points on the r and z grids.
!>
!>  This version removes vacuum current contribuitons.
!>
!>  @param[in] mgrid     A @ref m_grid::m_grid_class object.
!>  @param[in] pgrid     A @ref primed_grid::primed_grid_class object.
!>  @param[in] pgrid_vac A @ref primed_grid::primed_grid_class object for the
!>                       vacuum correction.
!>  @param[in] p_start   Starting phi index to compute fields to.
!>  @param[in] p_end     Ending phi index to compute fields to.
!>  @param[in] parallel  @ref bmw_parallel_context::bmw_parallel_context_class
!>                       object instance.
!>  @param[in] io_unit   Unit number to write messages to.
!>  @returns A pointer to a constructed @ref unprimed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION unprimed_grid_construct_c(mgrid, pgrid, pgrid_vac,              &
     &                                   p_start, p_end, parallel,             &
     &                                   io_unit)
!$    USE omp_lib
      USE bmw_state_flags, ONLY: clear_screen, progress
      USE, INTRINSIC :: iso_fortran_env, Only : output_unit

      IMPLICIT NONE

!  Declare Arguments
      CLASS (unprimed_grid_class), POINTER :: unprimed_grid_construct_c
      CLASS (m_grid_class), INTENT(in)               :: mgrid
      CLASS (primed_grid_class), INTENT(in)          :: pgrid
      CLASS (primed_grid_class), INTENT(in)          :: pgrid_vac
      INTEGER, INTENT(in)                            :: p_start
      INTEGER, INTENT(in)                            :: p_end
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: i
      INTEGER                                        :: ri
      INTEGER                                        :: zi
      INTEGER                                        :: vi
      INTEGER                                        :: num_r
      INTEGER                                        :: num_p
      INTEGER                                        :: num_z
      REAL (rprec)                                   :: x
      REAL (rprec)                                   :: y
      REAL (rprec)                                   :: ax
      REAL (rprec)                                   :: ay
      INTEGER                                        :: k_p
      INTEGER                                        :: k_m
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: r
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: z
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: r_p
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: r_m
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosv
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinv
      REAL (rprec)                                   :: total
      REAL (rprec)                                   :: done
      REAL (rprec)                                   :: current
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: rp
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: rp_vac
      REAL (rprec)                                   :: ar_p
      REAL (rprec)                                   :: ar_m
      REAL (rprec)                                   :: ap_p
      REAL (rprec)                                   :: ap_m
      REAL (rprec)                                   :: az_p
      REAL (rprec)                                   :: az_m

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(unprimed_grid_construct_c)

      num_r = SIZE(mgrid%a_r, 1)
      num_p = SIZE(mgrid%a_p, 3)
      num_z = SIZE(mgrid%a_z, 2)

      ALLOCATE(unprimed_grid_construct_c%a_r(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_c%a_p(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_c%a_z(num_r,num_z,num_p))

      ALLOCATE(unprimed_grid_construct_c%b_r(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_c%b_p(num_r,num_z,num_p))
      ALLOCATE(unprimed_grid_construct_c%b_z(num_r,num_z,num_p))

      ALLOCATE(cosv(num_p))
      ALLOCATE(sinv(num_p))

      ALLOCATE(r(num_r))
      ALLOCATE(z(num_z))
      ALLOCATE(r_p(num_r))
      ALLOCATE(r_m(num_r))

      total = parallel%end(num_r*num_z*num_p)
      total = CEILING(total/parallel%num_threads)

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, ri, zi, vi, x, y, ax, ay, rp, rp_vac, k_p, k_m,              &
!$OMP&         ar_p, ar_m, ap_p, ap_m, az_p, az_m, current)

!  Multi process will do an all reduce so these arrays need to be initalized.
      IF (parallel%stride .gt. 1) THEN
!$OMP WORKSHARE
         unprimed_grid_construct_c%a_r = 0.0
         unprimed_grid_construct_c%a_p = 0.0
         unprimed_grid_construct_c%a_z = 0.0
         unprimed_grid_construct_c%b_r = 0.0
         unprimed_grid_construct_c%b_p = 0.0
         unprimed_grid_construct_c%b_z = 0.0
         cosv = 0.0
         sinv = 0.0
         r = 0.0
         z = 0.0
         r_p = 0.0
         r_m = 0.0
!$OMP END WORKSHARE
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_r), parallel%end(num_r)
         r(i) = (i - 1.0)*mgrid%dr + mgrid%rmin
         r_p(i) = r(i) + mgrid%dr
         r_m(i) = r(i) - mgrid%dr
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_z), parallel%end(num_z)
         z(i) = (i - 1.0)*mgrid%dz + mgrid%zmin
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_p), parallel%end(num_p)
         x = (i - 1.0)*pgrid%dv
         cosv(i) = COS(x)
         sinv(i) = SIN(x)
      END DO
!$OMP END DO

!$OMP SINGLE
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(cosv)
         CALL parallel%reduce(sinv)
         CALL parallel%reduce(r)
         CALL parallel%reduce(z)
         CALL parallel%reduce(r_p)
         CALL parallel%reduce(r_m)
      END IF
!$OMP END SINGLE

      current = 0.0

      ALLOCATE(rp(SIZE(pgrid%x, 1),                                            &
     &            SIZE(pgrid%x, 2),                                            &
     &            SIZE(pgrid%x, 3)))
      ALLOCATE(rp_vac(SIZE(pgrid_vac%x, 1),                                    &
     &                SIZE(pgrid_vac%x, 2),                                    &
     &                SIZE(pgrid_vac%x, 3)))

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_r*num_z*num_p),                                &
     &       parallel%end(num_r*num_z*num_p)
         ri = bmw_parallel_context_i(i, num_r)
         zi = bmw_parallel_context_j(i, num_r, num_z)
         vi = bmw_parallel_context_k(i, num_r, num_z)

         IF (parallel%offset .eq. 0) THEN
!$          IF (OMP_GET_THREAD_NUM() .eq. 0) THEN
               current = current + 1.0
               done = 100.0*current/total

               WRITE (io_unit,1000,ADVANCE='NO')                               &
     &            clear_screen, progress(MOD(INT(current),4)), done

               IF (io_unit .ne. output_unit) THEN
                  BACKSPACE (io_unit)
               END IF
!$          END IF
         END IF

         x = r(ri)*cosv(vi)
         y = r(ri)*sinv(vi)

         rp = SQRT((pgrid%x - x)**2.0 + (pgrid%y - y)**2.0 +                   &
     &             (pgrid%z - z(zi))**2.0)
         rp_vac = SQRT((pgrid_vac%x - x)**2.0 +                                &
     &                 (pgrid_vac%y - y)**2.0 +                                &
     &                 (pgrid_vac%z - z(zi))**2.0)

         ax = SUM(pgrid%j_x/rp)*pgrid%dvol                                     &
     &      - SUM(pgrid_vac%j_x/rp_vac)*pgrid_vac%dvol
         ay = SUM(pgrid%j_y/rp)*pgrid%dvol                                     &
     &      - SUM(pgrid_vac%j_y/rp_vac)*pgrid_vac%dvol

         unprimed_grid_construct_c%a_r(ri,zi,vi) =                             &
     &       x/r(ri)*ax + y/r(ri)*ay + mgrid%a_r(ri,zi,vi)
         unprimed_grid_construct_c%a_p(ri,zi,vi) =                             &
     &      -y/r(ri)*ax + x/r(ri)*ay + mgrid%a_p(ri,zi,vi)
         unprimed_grid_construct_c%a_z(ri,zi,vi) =                             &
     &      SUM(pgrid%j_z/rp)*pgrid%dvol -                                     &
     &      SUM(pgrid_vac%j_z/rp_vac)*pgrid_vac%dvol +                         &
     &      mgrid%a_z(ri,zi,vi)
      END DO
!$OMP END DO

      DEALLOCATE(rp)
      DEALLOCATE(rp_vac)

!$OMP SINGLE
!  Multi process did not fill out the entire array. Get the missing pieces from
!  the other processes.
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(unprimed_grid_construct_c%a_r)
         CALL parallel%reduce(unprimed_grid_construct_c%a_p)
         CALL parallel%reduce(unprimed_grid_construct_c%a_z)
      END IF
!$OMP END SINGLE

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_p*num_r*num_z),                                &
     &       parallel%end(num_p*num_r*num_z)
         ri = bmw_parallel_context_i(i, num_r)
         zi = bmw_parallel_context_j(i, num_r, num_z)
         vi = bmw_parallel_context_k(i, num_r, num_z)

         IF (ri .eq. 1 .or. ri .eq. num_r .or.                                 &
     &       zi .eq. 1 .or. zi .eq. num_z) THEN
            unprimed_grid_construct_c%b_r(ri,zi,vi) = 0.0
            unprimed_grid_construct_c%b_p(ri,zi,vi) = 0.0
            unprimed_grid_construct_c%b_z(ri,zi,vi) = 0.0
         ELSE
            IF (num_p .eq. 1) THEN
               k_p = 1
               k_m = 1
            ELSE
               IF (vi .eq. 1) THEN
                  k_p = 2
                  k_m = num_p
               ELSE IF (vi .eq. num_p) THEN
                  k_p = 1
                  k_m = num_p - 1
               ELSE
                  k_p = vi + 1
                  k_m = vi - 1
               END IF
            END IF

! 1/rdazdp - dapdz
            az_p = unprimed_grid_construct_c%a_z(ri,zi,k_p)
            az_m = unprimed_grid_construct_c%a_z(ri,zi,k_m)
            ap_p = unprimed_grid_construct_c%a_p(ri,zi + 1,vi)
            ap_m = unprimed_grid_construct_c%a_p(ri,zi - 1,vi)
            unprimed_grid_construct_c%b_r(ri,zi,vi) =                          &
     &         (az_p - az_m)/(2.0*pgrid%dv*r(ri)) -                            &
     &         (ap_p - ap_m)/(2.0*mgrid%dz)

! dardz - dazdr
            ar_p = unprimed_grid_construct_c%a_r(ri,zi + 1,vi)
            ar_m = unprimed_grid_construct_c%a_r(ri,zi - 1,vi)
            az_p = unprimed_grid_construct_c%a_z(ri + 1,zi,vi)
            az_m = unprimed_grid_construct_c%a_z(ri - 1,zi,vi)
            unprimed_grid_construct_c%b_p(ri,zi,vi) =                          &
     &         (ar_p - ar_m)/(2.0*mgrid%dz) -                                  &
     &         (az_p - az_m)/(2.0*mgrid%dr)

! 1/r(drapdr - dardp)
            ar_p = unprimed_grid_construct_c%a_r(ri,zi,k_p)
            ar_m = unprimed_grid_construct_c%a_r(ri,zi,k_m)
            ap_p = unprimed_grid_construct_c%a_p(ri + 1,zi,vi)
            ap_m = unprimed_grid_construct_c%a_p(ri - 1,zi,vi)
            unprimed_grid_construct_c%b_z(ri,zi,vi) =                          &
     &         (r_p(ri)*ap_p - r_m(ri)*ap_m)/(2.0*mgrid%dr*r(ri)) -            &
     &         (ar_p - ar_m)/(2.0*pgrid%dv*r(ri))

         END IF
      END DO
!$OMP END DO
!$OMP END PARALLEL

      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(unprimed_grid_construct_c%b_r)
         CALL parallel%reduce(unprimed_grid_construct_c%b_p)
         CALL parallel%reduce(unprimed_grid_construct_c%b_z)
      END IF

      DEALLOCATE(cosv)
      DEALLOCATE(sinv)
      DEALLOCATE(r)
      DEALLOCATE(z)
      DEALLOCATE(r_p)
      DEALLOCATE(r_m)

      IF (parallel%offset .eq. 0) THEN
         WRITE (io_unit,1001) clear_screen
      END IF

      CALL profiler_set_stop_time('unprimed_grid_construct_m',                 &
     &                            start_time)

1000  FORMAT(a,a,f6.2,' % Finished')
1001  FORMAT(a,'Unprimed Grid Finished')

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref unprimed_grid_class object.
!>
!>  Deallocates memory and uninitializes a @ref unprimed_grid_class object.
!>
!>  @param[inout] this A @ref unprimed_grid_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE unprimed_grid_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (unprimed_grid_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%a_r)) THEN
         DEALLOCATE(this%a_r)
         this%a_r => null()
      END IF

      IF (ASSOCIATED(this%a_p)) THEN
         DEALLOCATE(this%a_p)
         this%a_p => null()
      END IF

      IF (ASSOCIATED(this%a_z)) THEN
         DEALLOCATE(this%a_z)
         this%a_z => null()
      END IF

      IF (ASSOCIATED(this%b_r)) THEN
         DEALLOCATE(this%b_r)
         this%b_r => null()
      END IF

      IF (ASSOCIATED(this%b_p)) THEN
         DEALLOCATE(this%b_p)
         this%b_p => null()
      END IF

      IF (ASSOCIATED(this%b_z)) THEN
         DEALLOCATE(this%b_z)
         this%b_z => null()
      END IF

      END SUBROUTINE

      END MODULE
