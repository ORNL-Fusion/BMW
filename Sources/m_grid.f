!*******************************************************************************
!>  @file m_grid.f
!>  @brief Contains module @ref m_grid.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref m_grid_class. This contains the
!>  state variables to define the vacuum vector potential.
!*******************************************************************************
      MODULE m_grid
      USE stel_kinds, ONLY: rprec
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) m grid base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a m grid. This is grid contains information about
!>  the vacuum fields.
!-------------------------------------------------------------------------------
      TYPE :: m_grid_class
!>  Minimum R position.
         REAL (rprec)                            :: rmin
!>  Maximum R position.
         REAL (rprec)                            :: rmax
!>  Minimum Z position.
         REAL (rprec)                            :: zmin
!>  Maximum Z position.
         REAL (rprec)                            :: zmax

!>  Radial grid size.
         REAL (rprec)                            :: dr
!>  Vertical grid size.
         REAL (rprec)                            :: dz

!>  Number of field periods.
         INTEGER                                 :: nfp

!>  Vector potential in the R direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: a_r => null()
!>  Vector potential in the Phi direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: a_p => null()
!>  Vector potential in the Z direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: a_z => null()
      CONTAINS
         PROCEDURE, PASS :: interpolate => m_grid_interpolate
         FINAL           :: m_grid_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the bmw_commandline_parser constructor.
!-------------------------------------------------------------------------------
      INTERFACE m_grid_class
         MODULE PROCEDURE m_grid_construct,                                    &
     &                    m_grid_construct_plasma
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref m_grid_class object.
!>
!>  Allocates memory and initializes a @ref m_grid_class object with an mgrid
!>  file.
!>
!>  @param[in] mgrid_file_name File name for vacuum fields.
!>  @param[in] parallel        @ref bmw_parallel_context_class object instance.
!>  @param[in] io_unit         Unit number to write messages to.
!>  @param[in] vmec            The vmec file object.
!>  @returns A pointer to a constructed @ref m_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION m_grid_construct(mgrid_file_name, parallel, io_unit,            &
     &                          vmec)
      USE ezcdf
      USE bmw_parallel_context
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      CLASS (m_grid_class), POINTER                  :: m_grid_construct
      CHARACTER (len=*), INTENT(in)                  :: mgrid_file_name
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit
      CLASS (vmec_file_class), POINTER, INTENT(in)   :: vmec

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: mgrid_ncid
      INTEGER                                        :: i
      INTEGER                                        :: status
      INTEGER                                        :: varid
      INTEGER                                        :: num_r
      INTEGER                                        :: num_p
      INTEGER                                        :: num_z
      REAL (rprec), DIMENSION(:,:,:), ALLOCATABLE    :: temp_buffer
      CHARACTER (len=6)                              :: temp_string
      INTEGER, DIMENSION(3)                          :: temp_dims

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(m_grid_construct)

      CALL cdf_open(mgrid_ncid, TRIM(mgrid_file_name), 'r', status)

      CALL cdf_inquire(mgrid_ncid, 'ar_001', temp_dims, ier=status)
      IF (status .ne. 0) THEN
         IF (parallel%offset .eq. 0) THEN
            WRITE (io_unit,3000) TRIM(mgrid_file_name)
         END IF
         CALL bmw_parallel_context_abort(status)
      END IF

      CALL cdf_read(mgrid_ncid, 'ir', num_r)
      CALL cdf_read(mgrid_ncid, 'kp', num_p)
      CALL cdf_read(mgrid_ncid, 'jz', num_z)

      CALL cdf_read(mgrid_ncid, 'nfp', m_grid_construct%nfp)

      CALL cdf_read(mgrid_ncid, 'rmax', m_grid_construct%rmax)
      CALL cdf_read(mgrid_ncid, 'zmax', m_grid_construct%zmax)
      CALL cdf_read(mgrid_ncid, 'rmin', m_grid_construct%rmin)
      CALL cdf_read(mgrid_ncid, 'zmin', m_grid_construct%zmin)

      m_grid_construct%dr = (m_grid_construct%rmax -                           &
     &                       m_grid_construct%rmin)/(num_r - 1.0)
      m_grid_construct%dz = (m_grid_construct%zmax -                           &
     &                       m_grid_construct%zmin)/(num_z - 1.0)

      ALLOCATE(m_grid_construct%a_r(num_r, num_z, num_p))
      ALLOCATE(m_grid_construct%a_p(num_r, num_z, num_p))
      ALLOCATE(m_grid_construct%a_z(num_r, num_z, num_p))

!$OMP PARALLEL
!$OMP WORKSHARE
      m_grid_construct%a_r = 0.0
      m_grid_construct%a_p = 0.0
      m_grid_construct%a_z = 0.0
!$OMP END WORKSHARE
!$OMP END PARALLEL

      ALLOCATE(temp_buffer(num_r, num_z, num_p))

      DO i = 1 + parallel%offset, SIZE(vmec%extcur), parallel%stride
         WRITE (temp_string, 1000) i
         CALL cdf_read(mgrid_ncid, temp_string, temp_buffer)
         m_grid_construct%a_r = m_grid_construct%a_r                           &
     &                        + temp_buffer*vmec%extcur(i)

         WRITE (temp_string, 1001) i
         CALL cdf_read(mgrid_ncid, temp_string, temp_buffer)
         m_grid_construct%a_p = m_grid_construct%a_p                           &
     &                        + temp_buffer*vmec%extcur(i)

         WRITE (temp_string, 1002) i
         CALL cdf_read(mgrid_ncid, temp_string, temp_buffer)
         m_grid_construct%a_z = m_grid_construct%a_z                           &
     &                        + temp_buffer*vmec%extcur(i)
      END DO

      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(m_grid_construct%a_r)
         CALL parallel%reduce(m_grid_construct%a_p)
         CALL parallel%reduce(m_grid_construct%a_z)
      END IF

      DEALLOCATE(temp_buffer)

      CALL cdf_close(mgrid_ncid)

      IF (parallel%offset .eq. 0) THEN
         WRITE (io_unit,2000)
      END IF

      CALL profiler_set_stop_time('m_grid_construct', start_time)

1000  FORMAT('ar_',i3.3)
1001  FORMAT('ap_',i3.3)
1002  FORMAT('az_',i3.3)

2000  FORMAT('M Grid Ready')

3000  FORMAT(a,' does not contain the vacuum vector potential.')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref m_grid_class object.
!>
!>  Allocates memory and initializes a @ref m_grid_class for plasma only
!>  responce on a manual grid.
!>
!>  @param[in] num_r    Number of radial points.
!>  @param[in] num_p    Number of toroidal points.
!>  @param[in] num_z    Number of vertical points.
!>  @param[in] rmax     Maximum radial position.
!>  @param[in] rmin     Minimum radial position.
!>  @param[in] zmax     Maximum vertical position.
!>  @param[in] zmin     Minimum vertical position.
!>  @param[in] parallel @ref bmw_parallel_context_class object instance.
!>  @param[in] io_unit  Unit number to write messages to.
!>  @param[in] vmec     The vmec file object.
!>  @returns A pointer to a constructed @ref m_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION m_grid_construct_plasma(num_r, num_p, num_z,                    &
     &                                 rmax, rmin, zmax, zmin,                 &
     &                                 parallel, io_unit, vmec)
      USE bmw_parallel_context
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      CLASS (m_grid_class), POINTER :: m_grid_construct_plasma
      INTEGER, INTENT(in)                            :: num_r
      INTEGER, INTENT(in)                            :: num_p
      INTEGER, INTENT(in)                            :: num_z
      REAL (rprec), INTENT(in)                       :: rmax
      REAL (rprec), INTENT(in)                       :: rmin
      REAL (rprec), INTENT(in)                       :: zmax
      REAL (rprec), INTENT(in)                       :: zmin
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit
      CLASS (vmec_file_class), POINTER, INTENT(in)   :: vmec

!  local variables
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(m_grid_construct_plasma)

      m_grid_construct_plasma%nfp = vmec%nfp

      m_grid_construct_plasma%rmax = rmax
      m_grid_construct_plasma%zmax = zmax
      m_grid_construct_plasma%rmin = rmin
      m_grid_construct_plasma%zmin = zmin

      m_grid_construct_plasma%dr = (m_grid_construct_plasma%rmax -             &
     &                              m_grid_construct_plasma%rmin)              &
     &                           / (num_r - 1.0)
      m_grid_construct_plasma%dz = (m_grid_construct_plasma%zmax -             &
     &                              m_grid_construct_plasma%zmin)              &
     &                           / (num_z - 1.0)

      ALLOCATE(m_grid_construct_plasma%a_r(num_r, num_z, num_p))
      ALLOCATE(m_grid_construct_plasma%a_p(num_r, num_z, num_p))
      ALLOCATE(m_grid_construct_plasma%a_z(num_r, num_z, num_p))

!$OMP PARALLEL
!$OMP WORKSHARE
      m_grid_construct_plasma%a_r = 0.0
      m_grid_construct_plasma%a_p = 0.0
      m_grid_construct_plasma%a_z = 0.0
!$OMP END WORKSHARE
!$OMP END PARALLEL

      CALL profiler_set_stop_time('m_grid_construct_plasma', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref m_grid_class object.
!>
!>  Deallocates memory and uninitializes a @ref m_grid_class object.
!>
!>  @param[inout] this A @ref m_grid_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE m_grid_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (m_grid_class), INTENT(inout) :: this

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

      END SUBROUTINE

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Interpolate the vector potential at a point.
!>
!>  This interpolates the vector potential to from the mgrid grid to an
!>  arbitrary point. This performs a trilinear interpolation.
!>
!>  @param[in]  this A @ref m_grid_class instance.
!>  @param[in]  r    Radial position to interpolate to.
!>  @param[in]  phi  Phi position to interpolate to.
!>  @param[in]  z    Vertical position to interpolate to.
!>  @param[out] ar   Interpolated radial component of the vector potential.
!>  @param[out] ap   Interpolated phi component of the vector potential.
!>  @param[out] az   Interpolated vertial component of the vector potential.
!-------------------------------------------------------------------------------
      PURE SUBROUTINE m_grid_interpolate(this, r, phi, z, ar, ap, az)
      USE stel_constants, ONLY: twopi

      IMPLICIT NONE

!  Declare Arguments
      CLASS (m_grid_class), INTENT(in) :: this
      REAL (rprec), INTENT(in)         :: r
      REAL (rprec), INTENT(in)         :: phi
      REAL (rprec), INTENT(in)         :: z
      REAL (rprec), INTENT(out)        :: ar
      REAL (rprec), INTENT(out)        :: ap
      REAL (rprec), INTENT(out)        :: az

!  local variables
      REAL (rprec)                     :: norm_phi
      REAL (rprec)                     :: dphi
      REAL (rprec)                     :: i
      REAL (rprec)                     :: j
      REAL (rprec)                     :: k
      INTEGER                          :: ilow
      INTEGER                          :: ihigh
      INTEGER                          :: jlow
      INTEGER                          :: jhigh
      INTEGER                          :: klow
      INTEGER                          :: khigh

!  Start of executable code
      dphi = twopi/(this%nfp*SIZE(this%a_r, 3))

      norm_phi = phi
      DO WHILE (norm_phi > twopi/this%nfp)
         norm_phi = norm_phi - twopi/this%nfp
      END DO

!  Find the nearest index positions.
      i = (r - this%rmin)/this%dr + 1.0
      j = (z - this%zmin)/this%dz + 1.0
      k = norm_phi/dphi + 1.0

      ilow = FLOOR(i)
      ihigh = CEILING(i)
      jlow = FLOOR(j)
      jhigh = CEILING(j)
      klow = FLOOR(k)
      IF (k .gt. SIZE(this%a_r, 3)) THEN
         khigh = 1
      ELSE
         khigh = CEILING(k)
      END IF

!  Scale index from 0 - 1.
      i = i - ilow
      j = j - jlow
      k = k - klow

      ar = m_grid_intf(                                                        &
     &        m_grid_intf(                                                     &
     &           m_grid_intf(                                                  &
     &              this%a_r(ilow,jlow,klow),                                  &
     &              this%a_r(ihigh,jlow,klow),                                 &
     &              i),                                                        &
     &           m_grid_intf(                                                  &
     &              this%a_r(ilow,jhigh,klow),                                 &
     &              this%a_r(ihigh,jhigh,klow),                                &
     &              i),                                                        &
     &           j),                                                           &
     &        m_grid_intf(                                                     &
     &           m_grid_intf(                                                  &
     &              this%a_r(ilow,jlow,khigh),                                 &
     &              this%a_r(ihigh,jlow,khigh),                                &
     &              i),                                                        &
     &           m_grid_intf(                                                  &
     &              this%a_r(ilow,jhigh,khigh),                                &
     &              this%a_r(ihigh,jhigh,khigh),                               &
     &              i),                                                        &
     &           j),                                                           &
     &        k)

      ap = m_grid_intf(                                                        &
     &        m_grid_intf(                                                     &
     &           m_grid_intf(                                                  &
     &              this%a_p(ilow,jlow,klow),                                  &
     &              this%a_p(ihigh,jlow,klow),                                 &
     &              i),                                                        &
     &           m_grid_intf(                                                  &
     &              this%a_p(ilow,jhigh,klow),                                 &
     &              this%a_p(ihigh,jhigh,klow),                                &
     &              i),                                                        &
     &           j),                                                           &
     &        m_grid_intf(                                                     &
     &           m_grid_intf(                                                  &
     &              this%a_p(ilow,jlow,khigh),                                 &
     &              this%a_p(ihigh,jlow,khigh),                                &
     &              i),                                                        &
     &           m_grid_intf(                                                  &
     &              this%a_p(ilow,jhigh,khigh),                                &
     &              this%a_p(ihigh,jhigh,khigh),                               &
     &              i),                                                        &
     &           j),                                                           &
     &        k)

      az = m_grid_intf(                                                        &
     &        m_grid_intf(                                                     &
     &           m_grid_intf(                                                  &
     &              this%a_z(ilow,jlow,klow),                                  &
     &              this%a_z(ihigh,jlow,klow),                                 &
     &              i),                                                        &
     &           m_grid_intf(                                                  &
     &              this%a_z(ilow,jhigh,klow),                                 &
     &              this%a_z(ihigh,jhigh,klow),                                &
     &              i),                                                        &
     &           j),                                                           &
     &        m_grid_intf(                                                     &
     &           m_grid_intf(                                                  &
     &              this%a_z(ilow,jlow,khigh),                                 &
     &              this%a_z(ihigh,jlow,khigh),                                &
     &              i),                                                        &
     &           m_grid_intf(                                                  &
     &              this%a_z(ilow,jhigh,khigh),                                &
     &              this%a_z(ihigh,jhigh,khigh),                               &
     &              i),                                                        &
     &           j),                                                           &
     &        k)

      END SUBROUTINE

!*******************************************************************************
!  UTILITY SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Interpolate the vector potential at a point.
!>
!>  This interpolates the vector potential to from the mgrid grid to an
!>  arbitrary point. This performs a trilinear interpolation.
!>
!>  @param[in]  this A @ref m_grid_class instance.
!>  @param[in]  r    Radial position to interpolate to.
!>  @param[in]  phi  Phi position to interpolate to.
!>  @param[in]  z    Vertical position to interpolate to.
!>  @param[out] ar   Interpolated radial component of the vector potential.
!>  @param[out] ap   Interpolated phi component of the vector potential.
!>  @param[out] az   Interpolated vertial component of the vector potential.
!-------------------------------------------------------------------------------
      PURE FUNCTION m_grid_intf(w1, w2, x)

      IMPLICIT NONE

!  Declare Arguments
      REAL (rprec)                    :: m_grid_intf
      REAL (rprec), INTENT(in)        :: w1
      REAL (rprec), INTENT(in)        :: w2
      REAL (rprec), INTENT(in)        :: x

!  Start of executable code
      m_grid_intf = w1*(1.0 - x) + w2*x

      END FUNCTION

      END MODULE
