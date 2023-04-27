!*******************************************************************************
!>  @file bmw_context.f
!>  @brief Contains module @ref bmw_context.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref bmw_context_class. This contains the
!>  state variables needed by BMW.
!*******************************************************************************
      MODULE bmw_context
      USE m_grid
      USE primed_grid
      USE unprimed_grid
      USE bmw_parallel_context

      IMPLICIT NONE

!*******************************************************************************
!  bmw context module parameters
!*******************************************************************************
!>  Version number.
      INTEGER, PARAMETER :: series = 3

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) bmw context base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a bmw context. This contains all memory needed to
!>  operate bmw.
!-------------------------------------------------------------------------------
      TYPE :: bmw_context_class
!>  Vacuum grid object.
         CLASS (m_grid_class), POINTER        :: m_grid => null()
!>  Primed grid object.
         CLASS (primed_grid_class), POINTER   :: p_grid => null()
!>  Unprimed grid object.
         CLASS (unprimed_grid_class), POINTER :: up_grid => null()
      CONTAINS
         PROCEDURE, PASS :: set_up_grid_m => bmw_context_set_up_grid_m
         PROCEDURE, PASS :: set_up_grid_a => bmw_context_set_up_grid_a
         GENERIC         :: set_up_grid => set_up_grid_m, set_up_grid_a
         PROCEDURE, PASS :: write => bmw_context_write
         FINAL :: bmw_context_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the bmw_context constructor.
!-------------------------------------------------------------------------------
      INTERFACE bmw_context_class
         MODULE PROCEDURE bmw_context_construct,                               &
     &                    bmw_context_construct_plasma
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref bmw_context_class object.
!>
!>  Allocates memory and initializes a @ref bmw_context_class object.
!>
!>  @param[in] mgrid_file_name  Path and name of the mgrid file.
!>  @param[in] wout_file_name   Path and name of the wout file.
!>  @param[in] siesta_file_name Path and name of the siesta file.
!>  @param[in] flags            Option flags.
!>  @param[in] num_p            Number of phi planes per field period.
!>  @param[in] parallel         @ref bmw_parallel_context_class object instance.
!>  @param[in] io_unit          Unit number to write messages to.
!>  @returns A pointer to a constructed @ref bmw_context_class object.
!-------------------------------------------------------------------------------
      FUNCTION bmw_context_construct(mgrid_file_name, wout_file_name,          &
     &                               siesta_file_name, flags, num_p,           &
     &                               parallel, io_unit)
      USE read_wout_mod, Only: read_wout_file, extcur
      USE bmw_state_flags, Only: bmw_state_flags_mgrid

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_context_class), POINTER :: bmw_context_construct
      CHARACTER (len=*), INTENT(in)                  :: mgrid_file_name
      CHARACTER (len=*), INTENT(in)                  :: wout_file_name
      CHARACTER (len=*), INTENT(in)                  :: siesta_file_name
      INTEGER, INTENT(in)                            :: flags
      INTEGER, INTENT(inout)                         :: num_p
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: status

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(bmw_context_construct)

      CALL read_wout_file(wout_file_name, status)
      IF (status .ne. 0) THEN
         IF (parallel%offset .eq. 0) THEN
            WRITE (io_unit,1000) TRIM(wout_file_name)
         END IF
         CALL bmw_parallel_context_abort(status)
      END IF
      IF (.not.ALLOCATED(extcur)) THEN
         IF (parallel%offset .eq. 0) THEN
            WRITE (io_unit,1001) TRIM(wout_file_name)
         END IF
         CALL bmw_parallel_context_abort(status)
      END IF

      bmw_context_construct%m_grid =>                                          &
     &   m_grid_class(mgrid_file_name, parallel, io_unit)

      IF (BTEST(flags, bmw_state_flags_mgrid)) THEN
         num_p = SIZE(bmw_context_construct%m_grid%a_p, 3)
      END IF

      bmw_context_construct%p_grid =>                                          &
     &   primed_grid_class(num_p*bmw_context_construct%m_grid%nfp,             &
     &                     flags, siesta_file_name, parallel, io_unit)

      CALL profiler_set_stop_time('bmw_context_construct', start_time)

1000  FORMAT(a,' is an invalid wout file.')
1001  FORMAT(a,' is not a free boundary wout file.')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref bmw_context_class object.
!>
!>  Allocates memory and initializes a @ref bmw_context_class object.
!>
!>  @param[in] mgrid_file_name  Path and name of the mgrid file.
!>  @param[in] wout_file_name   Path and name of the wout file.
!>  @param[in] siesta_file_name Path and name of the siesta file.
!>  @param[in] flags            Option flags.
!>  @param[in] num_r            Number of radial points.
!>  @param[in] num_p            Number of toroidal points.
!>  @param[in] num_z            Number of vertical points.
!>  @param[in] rmax             Maximum radial position.
!>  @param[in] rmin             Minimum radial position.
!>  @param[in] zmax             Maximum vertical position.
!>  @param[in] zmin             Minimum vertical position.
!>  @param[in] parallel         @ref bmw_parallel_context_class object instance.
!>  @param[in] io_unit          Unit number to write messages to.
!>  @returns A pointer to a constructed @ref bmw_context_class object.
!-------------------------------------------------------------------------------
      FUNCTION bmw_context_construct_plasma(wout_file_name,                    &
     &                                      siesta_file_name, flags,           &
     &                                      num_r, num_p, num_z,               &
     &                                      rmax, rmin, zmax, zmin,            &
     &                                      parallel, io_unit)
      USE read_wout_mod, Only: read_wout_file, extcur
      USE bmw_state_flags, Only: bmw_state_flags_mgrid

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_context_class), POINTER :: bmw_context_construct_plasma
      CHARACTER (len=*), INTENT(in)                  :: wout_file_name
      CHARACTER (len=*), INTENT(in)                  :: siesta_file_name
      INTEGER, INTENT(in)                            :: flags
      INTEGER, INTENT(in)                            :: num_r
      INTEGER, INTENT(in)                            :: num_p
      INTEGER, INTENT(in)                            :: num_z
      REAL (rprec), INTENT(in)                       :: rmax
      REAL (rprec), INTENT(in)                       :: rmin
      REAL (rprec), INTENT(in)                       :: zmax
      REAL (rprec), INTENT(in)                       :: zmin
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: status

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(bmw_context_construct_plasma)

      CALL read_wout_file(wout_file_name, status)
      IF (status .ne. 0) THEN
         IF (parallel%offset .eq. 0) THEN
            WRITE (io_unit,1000) TRIM(wout_file_name)
         END IF
         CALL bmw_parallel_context_abort(status)
      END IF
      IF (.not.ALLOCATED(extcur)) THEN
         IF (parallel%offset .eq. 0) THEN
            WRITE (io_unit,1001) TRIM(wout_file_name)
         END IF
         CALL bmw_parallel_context_abort(status)
      END IF

      bmw_context_construct_plasma%m_grid =>                                   &
     &   m_grid_class(num_r, num_p, num_z, rmax, rmin, zmax, zmin,             &
     &                parallel, io_unit)

      bmw_context_construct_plasma%p_grid =>                                   &
     &   primed_grid_class(                                                    &
     &      num_p*bmw_context_construct_plasma%m_grid%nfp,                     &
     &      flags, siesta_file_name, parallel, io_unit)

      CALL profiler_set_stop_time('bmw_context_construct_plasma',              &
     &                            start_time)

1000  FORMAT(a,' is an invalid wout file.')
1001  FORMAT(a,' is not a free boundary wout file.')

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref bmw_context_class object.
!>
!>  Deallocates memory and uninitializes a @ref bmw_context_class object.
!>
!>  @param[inout] this A @ref bmw_context_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_context_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (bmw_context_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%m_grid)) THEN
         DEALLOCATE(this%m_grid)
         this%m_grid => null()
      END IF

      IF (ASSOCIATED(this%p_grid)) THEN
         DEALLOCATE(this%p_grid)
         this%p_grid => null()
      END IF

      IF (ASSOCIATED(this%up_grid)) THEN
         DEALLOCATE(this%up_grid)
         this%up_grid => null()
      END IF

      END SUBROUTINE

!*******************************************************************************
!  SETTER SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Set the unprimed grid.
!>
!>  This initializes the unprimed file using the mgrid grid as the unprimed
!>  grid.
!>
!>  @param[inout] this     A @ref bmw_context_class instance.
!>  @param[in]    p_start  Starting phi index to compute fields to.
!>  @param[in]    p_end    Ending phi index to compute fields to.
!>  @param[in]    parallel @ref bmw_parallel_context_class object instance.
!>  @param[in]    io_unit  Unit number to write messages to.
!>  @returns A pointer to a constructed @ref bmw_context_class object.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_context_set_up_grid_m(this, p_start, p_end,               &
     &                                     parallel, io_unit)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_context_class), INTENT(inout)       :: this
      INTEGER, INTENT(in)                            :: p_start
      INTEGER, INTENT(in)                            :: p_end
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit

!  local variables
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      this%up_grid => unprimed_grid_class(this%m_grid, this%p_grid,            &
     &                                    p_start, p_end, parallel,            &
     &                                    io_unit)

      CALL profiler_set_stop_time('bmw_context_set_up_grid_m',                 &
     &                            start_time)

      END SUBROUTINE

!-------------------------------------------------------------------------------
!>  @brief Set the unprimed grid.
!>
!>  This initializes the unprimed file using supplied grid as the unprimed
!>  grid.
!>
!>  @param[inout] this     A @ref bmw_context_class instance.
!>  @param[in]    r_grid   3D Array of r positions.
!>  @param[in]    z_grid   3D Array of z positions.
!>  @param[in]    dphi     Phi grid spacing.
!>  @param[in]    parallel @ref bmw_parallel_context_class object instance.
!>  @param[in]    io_unit  Unit number to write messages to.
!>  @returns A pointer to a constructed @ref bmw_context_class object.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_context_set_up_grid_a(this, r_grid, z_grid, dphi,         &
     &                                     parallel, io_unit)

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_context_class), INTENT(inout)       :: this
      REAL (rprec), DIMENSION(:,:,:), INTENT(in)     :: r_grid
      REAL (rprec), DIMENSION(:,:,:), INTENT(in)     :: z_grid
      REAL (rprec), INTENT(in)                       :: dphi
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                            :: io_unit

!  local variables
      REAL (rprec)                                   :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      this%up_grid => unprimed_grid_class(this%m_grid, this%p_grid,            &
     &                                    r_grid, z_grid, dphi,                &
     &                                    parallel, io_unit)

      CALL profiler_set_stop_time('bmw_context_set_up_grid_a',                 &
     &                            start_time)

      END SUBROUTINE

!*******************************************************************************
!  NETCDF SUBROUTINES
!*******************************************************************************
!>  @page result_file_bmw BMW Result File
!>
!>  @tableofcontents
!>  @section result_file_bmw_intro_sec Introduction
!>  This page documents the contents of a result NetCDF.
!>
!>  @section result_file_bmw_var_sec Variables
!>  @header{Variable(Dimensions), Description, Code Reference}
!>  @begin_table
!>     @item{series,                          Version number.,                    bmw_context::series}
!>     @item{nfp,                             Number of field periods,            m_grid::m_grid_class::nfp}
!>     @item{rmin,                            Minimum R point.,                   m_grid::m_grid_class::rmin}
!>     @item{rmax,                            Maximum R point.,                   m_grid::m_grid_class::rmax}
!>     @item{zmin,                            Minimum Z point.,                   m_grid::m_grid_class::rmin}
!>     @item{zmax,                            Maximum Z point.,                   m_grid::m_grid_class::zmax}
!>     @item{ar_grid (num_p\, num_z\, num_r), Vector potential in R direction.,   unprimed_grid::unprimed_grid_class::a_r}
!>     @item{ap_grid (num_p\, num_z\, num_r), Vector potential in Phi direction., unprimed_grid::unprimed_grid_class::a_p}
!>     @item{az_grid (num_p\, num_z\, num_r), Vector potential in Z direction.,   unprimed_grid::unprimed_grid_class::a_z}
!>     @item{br_grid (num_p\, num_z\, num_r), Magnetic field in R direction.,     unprimed_grid::unprimed_grid_class::b_r}
!>     @item{bp_grid (num_p\, num_z\, num_r), Magnetic field in Phi direction.,   unprimed_grid::unprimed_grid_class::b_p}
!>     @item{bz_grid (num_p\, num_z\, num_r), Magnetic field in Z direction.,     unprimed_grid::unprimed_grid_class::b_z}
!>     @item{px_grid (num_s\, num_u\, num_v), X position of the primed grid.,     primed_grid::primed_grid_class::x}
!>     @item{py_grid (num_s\, num_u\, num_v), Y position of the primed grid.,     primed_grid::primed_grid_class::y}
!>     @item{pz_grid (num_s\, num_u\, num_v), Z position of the primed grid.,     primed_grid::primed_grid_class::z}
!>     @item{jx_grid (num_s\, num_u\, num_v), X position of the primed grid.,     primed_grid::primed_grid_class::j_x}
!>     @item{jy_grid (num_s\, num_u\, num_v), Y position of the primed grid.,     primed_grid::primed_grid_class::j_y}
!>     @item{jz_grid (num_s\, num_u\, num_v), Z position of the primed grid.,     primed_grid::primed_grid_class::j_z}
!>  @end_table
!-------------------------------------------------------------------------------
!>  @brief Write NetCDF based result file.
!>
!>  Defines dimensions and variables for the BMW result file. Flat internal
!>  arrays need to be reshaped to the dimension.
!>
!>  @param[in] this             A @ref bmw_context_class instance.
!>  @param[in] result_file_name NetCDF file id of the result file.
!>  @param[in] parallel         @ref bmw_parallel_context_class object instance.
!-------------------------------------------------------------------------------
      SUBROUTINE bmw_context_write(this, result_file_name, parallel)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (bmw_context_class), INTENT(in)          :: this
      CHARACTER (len=*), INTENT(in)                  :: result_file_name
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: status
      INTEGER                                        :: result_iou

!  local parameters
      CHARACTER (len=*), DIMENSION(3), PARAMETER  ::                           &
     &   up_dims = (/ 'r  ','z  ','phi' /)
      CHARACTER (len=*), DIMENSION(3), PARAMETER  ::                           &
     &   p_dims = (/ 's','u','v' /)

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (parallel%offset .eq. 0) THEN
         CALL cdf_open(result_iou, TRIM(result_file_name), 'w', status)

         CALL cdf_define(result_iou, 'series', series)

         CALL cdf_define(result_iou, 'nfp', this%m_grid%nfp)

         CALL cdf_define(result_iou, 'rmin', this%m_grid%rmin)
         CALL cdf_define(result_iou, 'rmax', this%m_grid%rmax)
         CALL cdf_define(result_iou, 'zmin', this%m_grid%zmin)
         CALL cdf_define(result_iou, 'zmax', this%m_grid%zmax)

         CALL cdf_define(result_iou, 'ar_grid', this%up_grid%a_r,              &
     &                   dimname=up_dims)
         CALL cdf_define(result_iou, 'ap_grid', this%up_grid%a_p,              &
     &                   dimname=up_dims)
         CALL cdf_define(result_iou, 'az_grid', this%up_grid%a_z,              &
     &                   dimname=up_dims)

         CALL cdf_define(result_iou, 'br_grid', this%up_grid%b_r,              &
     &                   dimname=up_dims)
         CALL cdf_define(result_iou, 'bp_grid', this%up_grid%b_p,              &
     &                   dimname=up_dims)
         CALL cdf_define(result_iou, 'bz_grid', this%up_grid%b_z,              &
     &                   dimname=up_dims)

         CALL cdf_define(result_iou, 'px_grid', this%p_grid%x,                 &
     &                   dimname=p_dims)
         CALL cdf_define(result_iou, 'py_grid', this%p_grid%y,                 &
     &                   dimname=p_dims)
         CALL cdf_define(result_iou, 'pz_grid', this%p_grid%z,                 &
     &                   dimname=p_dims)

         CALL cdf_define(result_iou, 'jx_grid', this%p_grid%j_x,               &
     &                   dimname=p_dims)
         CALL cdf_define(result_iou, 'jy_grid', this%p_grid%j_y,               &
     &                   dimname=p_dims)
         CALL cdf_define(result_iou, 'jz_grid', this%p_grid%j_z,               &
     &                   dimname=p_dims)

         CALL cdf_write(result_iou, 'series', series)

         CALL cdf_write(result_iou, 'nfp', this%m_grid%nfp)

         CALL cdf_write(result_iou, 'rmin', this%m_grid%rmin)
         CALL cdf_write(result_iou, 'rmax', this%m_grid%rmax)
         CALL cdf_write(result_iou, 'zmin', this%m_grid%zmin)
         CALL cdf_write(result_iou, 'zmax', this%m_grid%zmax)

         CALL cdf_write(result_iou, 'ar_grid', this%up_grid%a_r)
         CALL cdf_write(result_iou, 'ap_grid', this%up_grid%a_p)
         CALL cdf_write(result_iou, 'az_grid', this%up_grid%a_z)

         CALL cdf_write(result_iou, 'br_grid', this%up_grid%b_r)
         CALL cdf_write(result_iou, 'bp_grid', this%up_grid%b_p)
         CALL cdf_write(result_iou, 'bz_grid', this%up_grid%b_z)

         CALL cdf_write(result_iou, 'px_grid', this%p_grid%x)
         CALL cdf_write(result_iou, 'py_grid', this%p_grid%y)
         CALL cdf_write(result_iou, 'pz_grid', this%p_grid%z)

         CALL cdf_write(result_iou, 'jx_grid', this%p_grid%j_x)
         CALL cdf_write(result_iou, 'jy_grid', this%p_grid%j_y)
         CALL cdf_write(result_iou, 'jz_grid', this%p_grid%j_z)

         CALL cdf_close(result_iou)
      END IF

      CALL profiler_set_stop_time('bmw_context_write', start_time)

      END SUBROUTINE

      END MODULE
