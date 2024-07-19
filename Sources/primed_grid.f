!*******************************************************************************
!>  @file primed_grid.f
!>  @brief Contains module @ref primed_grid.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref primed_grid_class. This contains the
!>  state variables to define the currents and positions of the volumn integral.
!*******************************************************************************
      MODULE primed_grid
      USE stel_kinds, ONLY: rprec
      USE profiler
      USE bmw_parallel_context

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) primed grid base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a primed grid. This is grid the volume integral will
!>  be summed over.
!-------------------------------------------------------------------------------
      TYPE :: primed_grid_class
!>  Volume integration element.
         REAL (rprec)                            :: dvol
!>  Toroidal grid spacing.
         REAL (rprec)                            :: dv

!>  X position.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: x => null()
!>  Y position.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: y => null()
!>  Z position.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: z => null()

!>  Current density in the X direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: j_x => null()
!>  Current density in the Y direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: j_y => null()
!>  Current density in the Z direction.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: j_z => null()
      CONTAINS
         FINAL :: primed_grid_destruct
      END TYPE

!*******************************************************************************
!  INTERFACE BLOCKS
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Interface for the bmw_commandline_parser constructor.
!-------------------------------------------------------------------------------
      INTERFACE primed_grid_class
         MODULE PROCEDURE primed_grid_construct,                               &
     &                    primed_grid_construct_no_vac
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
! FIXME: Temp constructor to remove the vaccum vmec argument.
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref primed_grid_class object.
!>
!>  Allocates memory and initializes a @ref primed_grid_class object depending
!>  on the option flags.
!>
!>  @param[in] num_v            Number of toroidal grid points.
!>  @param[in] flags            Number of toroidal grid points.
!>  @param[in] vmec             VMEC file object.
!>  @param[in] siesta_file_name Name of the siesta restart file.
!>  @param[in] parallel         @ref bmw_parallel_context_class object instance.
!>  @param[in] io_unit          Unit number to write messages to.
!>  @returns A pointer to a constructed @ref primed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION primed_grid_construct_no_vac(num_v, flags, vmec,                &
     &                                      siesta_file_name, parallel,        &
     &                                      io_unit)
      USE bmw_state_flags
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      TYPE (primed_grid_class), POINTER :: primed_grid_construct_no_vac
      INTEGER, INTENT(in)                           :: num_v
      INTEGER, INTENT(in)                           :: flags
      CLASS (vmec_file_class), POINTER, INTENT(in)  :: vmec
      CHARACTER (len=*), INTENT(in)                 :: siesta_file_name
      TYPE (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                           :: io_unit

!  local variables
      REAL (rprec)                                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      primed_grid_construct_no_vac =>                                          &
     &   primed_grid_construct(num_v, flags, vmec, '', siesta_file_name,       &
     &                         parallel, io_unit)

      CALL profiler_set_stop_time('primed_grid_construct_no_vac',              &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref primed_grid_class object.
!>
!>  Allocates memory and initializes a @ref primed_grid_class object depending
!>  on the option flags.
!>
!>  @param[in] num_v            Number of toroidal grid points.
!>  @param[in] flags            Number of toroidal grid points.
!>  @param[in] vmec             VMEC file object.
!>  @param[in] vmec_vac_file    Name of the zero beta vmec file.
!>  @param[in] siesta_file_name Name of the siesta restart file.
!>  @param[in] parallel         @ref bmw_parallel_context_class object instance.
!>  @param[in] io_unit          Unit number to write messages to.
!>  @returns A pointer to a constructed @ref primed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION primed_grid_construct(num_v, flags, vmec,                       &
     &                               vmec_vac_file, siesta_file_name,          &
     &                               parallel, io_unit)
      USE bmw_state_flags
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      TYPE (primed_grid_class), POINTER :: primed_grid_construct
      INTEGER, INTENT(in)                           :: num_v
      INTEGER, INTENT(in)                           :: flags
      CLASS (vmec_file_class), POINTER, INTENT(in)  :: vmec
      CHARACTER (len=*), INTENT(in)                 :: vmec_vac_file
      CHARACTER (len=*), INTENT(in)                 :: siesta_file_name
      TYPE (bmw_parallel_context_class), INTENT(in) :: parallel
      INTEGER, INTENT(in)                           :: io_unit

!  local variables
      REAL (rprec)                                  :: start_time

!  Start of executable code
      start_time = profiler_get_start_time()

      IF (BTEST(flags, bmw_state_flags_ju)) THEN
         primed_grid_construct =>                                              &
     &      primed_grid_construct_ju(num_v, vmec, parallel)
      ELSE IF (BTEST(flags, bmw_state_flags_jv)) THEN
         primed_grid_construct =>                                              &
     &      primed_grid_construct_jv(num_v, vmec, parallel)
      ELSE IF (BTEST(flags, bmw_state_flags_siesta)) THEN
         primed_grid_construct =>                                              &
     &      primed_grid_construct_siesta(num_v, vmec,                          &
     &                                   siesta_file_name, parallel)
      ELSE
         primed_grid_construct =>                                              &
     &      primed_grid_construct_both(num_v, vmec, parallel)
      END IF

      IF (parallel%offset .eq. 0) THEN
         WRITE(io_unit,1000)
      END IF

      CALL profiler_set_stop_time('primed_grid_construct', start_time)

1000  FORMAT('Prime Grid Ready')

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref primed_grid_class object.
!>
!>  Allocates memory and initializes a @ref primed_grid_class object. This
!>  computes the currents and positions on the primed grid. Plasma currents are
!>  obtained from Curl(B).
!>
!>  @param[in] num_v    Number of toroidal grid points.
!>  @param[in] vmec     VMEC file object.
!>  @param[in] parallel @ref bmw_parallel_context_class object instance.
!>  @returns A pointer to a constructed @ref primed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION primed_grid_construct_both(num_v, vmec, parallel)
      USE stel_constants, ONLY: twopi, mu0
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      CLASS (primed_grid_class), POINTER :: primed_grid_construct_both
      INTEGER, INTENT(in)                            :: num_v
      CLASS (vmec_file_class), POINTER, INTENT(in)   :: vmec
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: i
      REAL (rprec)                                   :: x
      REAL (rprec)                                   :: ds
      INTEGER                                        :: si
      INTEGER                                        :: ui
      INTEGER                                        :: vi
      REAL (rprec)                                   :: r
      REAL (rprec)                                   :: z
      REAL (rprec)                                   :: ru
      REAL (rprec)                                   :: zu
      REAL (rprec)                                   :: rv
      REAL (rprec)                                   :: zv
      REAL (rprec)                                   :: ju
      REAL (rprec)                                   :: jv
      REAL (rprec)                                   :: jr
      REAL (rprec)                                   :: jp
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosv
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmu_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmu_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosnv_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinnv_nyq
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosmn
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinmn
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosmn_nyq
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinmn_nyq
      INTEGER                                        :: num_v_use

!  local parameters
      INTEGER, PARAMETER                             :: num_u = 101
      REAL (rprec), PARAMETER                        :: du = twopi/num_u

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(primed_grid_construct_both)

      IF (num_v .eq. 1) THEN
         num_v_use = 360
      ELSE
         num_v_use = num_v
      END IF

      ALLOCATE(primed_grid_construct_both%x(vmec%ns,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_both%y(vmec%ns,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_both%z(vmec%ns,num_u,num_v_use))

      ALLOCATE(primed_grid_construct_both%j_x(vmec%ns,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_both%j_y(vmec%ns,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_both%j_z(vmec%ns,num_u,num_v_use))

      ALLOCATE(cosv(num_v_use))
      ALLOCATE(sinv(num_v_use))
      ALLOCATE(cosmu(vmec%mnmax,num_u))
      ALLOCATE(sinmu(vmec%mnmax,num_u))
      ALLOCATE(cosnv(vmec%mnmax,num_v_use))
      ALLOCATE(sinnv(vmec%mnmax,num_v_use))
      ALLOCATE(cosmu_nyq(vmec%mnmax_nyq,num_u))
      ALLOCATE(sinmu_nyq(vmec%mnmax_nyq,num_u))
      ALLOCATE(cosnv_nyq(vmec%mnmax_nyq,num_v_use))
      ALLOCATE(sinnv_nyq(vmec%mnmax_nyq,num_v_use))

      ds = 1.0/(vmec%ns - 1.0)
      primed_grid_construct_both%dv = twopi/num_v_use

      primed_grid_construct_both%dvol = vmec%isigng*ds*du                      &
     &   * primed_grid_construct_both%dv/(2.0*twopi)

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, x, si, ui, vi, r, z, ru, rv, zu, zv, ju, jv, jr, jp,         &
!$OMP&         cosmn, sinmn, cosmn_nyq, sinmn_nyq)

!  Multi process will do an all reduce so these arrays need to be initalized.
      IF (parallel%stride .gt. 1) THEN
!$OMP WORKSHARE
         primed_grid_construct_both%x = 0.0
         primed_grid_construct_both%y = 0.0
         primed_grid_construct_both%z = 0.0
         primed_grid_construct_both%j_x = 0.0
         primed_grid_construct_both%j_y = 0.0
         primed_grid_construct_both%j_z = 0.0
         cosv = 0.0
         sinv = 0.0
         cosmu = 0.0
         sinmu = 0.0
         cosnv = 0.0
         sinnv = 0.0
         cosmu_nyq = 0.0
         sinmu_nyq = 0.0
         cosnv_nyq = 0.0
         sinnv_nyq = 0.0
!$OMP END WORKSHARE
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_u), parallel%end(num_u)
         x = (i - 0.5)*du
         cosmu(:,i) = COS(vmec%xm*x)
         sinmu(:,i) = SIN(vmec%xm*x)
         cosmu_nyq(:,i) = COS(vmec%xm_nyq*x)
         sinmu_nyq(:,i) = SIN(vmec%xm_nyq*x)
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_v_use), parallel%end(num_v_use)
         x = (i - 0.5)*primed_grid_construct_both%dv
         cosv(i) = COS(x)
         sinv(i) = SIN(x)
         cosnv(:,i) = COS(vmec%xn*x)
         sinnv(:,i) = SIN(vmec%xn*x)
         cosnv_nyq(:,i) = COS(vmec%xn_nyq*x)
         sinnv_nyq(:,i) = SIN(vmec%xn_nyq*x)
      END DO
!$OMP END DO

      IF (parallel%stride .gt. 1) THEN
!$OMP SINGLE
         CALL parallel%reduce(cosv)
         CALL parallel%reduce(sinv)
         CALL parallel%reduce(cosmu)
         CALL parallel%reduce(sinmu)
         CALL parallel%reduce(cosmu_nyq)
         CALL parallel%reduce(sinmu_nyq)
         CALL parallel%reduce(cosnv)
         CALL parallel%reduce(sinnv)
         CALL parallel%reduce(cosnv_nyq)
         CALL parallel%reduce(sinnv_nyq)
!$OMP END SINGLE
      END IF

      ALLOCATE(cosmn(vmec%mnmax))
      ALLOCATE(sinmn(vmec%mnmax))
      ALLOCATE(cosmn_nyq(vmec%mnmax_nyq))
      IF (vmec%lasym) THEN
         ALLOCATE(sinmn_nyq(vmec%mnmax_nyq))
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(vmec%ns*num_u*num_v_use),                          &
     &       parallel%end(vmec%ns*num_u*num_v_use)
         si = bmw_parallel_context_i(i, vmec%ns)
         ui = bmw_parallel_context_j(i, vmec%ns, num_u)
         vi = bmw_parallel_context_k(i, vmec%ns, num_u)

         cosmn = cosmu(:,ui)*cosnv(:,vi) + sinmu(:,ui)*sinnv(:,vi)
         sinmn = sinmu(:,ui)*cosnv(:,vi) - cosmu(:,ui)*sinnv(:,vi)
         cosmn_nyq = cosmu_nyq(:,ui)*cosnv_nyq(:,vi)                           &
     &             + sinmu_nyq(:,ui)*sinnv_nyq(:,vi)

         r = SUM(vmec%rmncf(:,si)*cosmn(:))
         z = SUM(vmec%zmnsf(:,si)*sinmn(:))

         ru = -SUM(vmec%xm*vmec%rmncf(:,si)*sinmn(:))
         rv =  SUM(vmec%xn*vmec%rmncf(:,si)*sinmn(:))
         zu =  SUM(vmec%xm*vmec%zmnsf(:,si)*cosmn(:))
         zv = -SUM(vmec%xn*vmec%zmnsf(:,si)*cosmn(:))

         ju = SUM(vmec%jksupumncf(:,si)*cosmn_nyq(:))*mu0
         jv = SUM(vmec%jksupvmncf(:,si)*cosmn_nyq(:))*mu0

         IF (vmec%lasym) THEN
            sinmn_nyq = sinmu_nyq(:,ui)*cosnv_nyq(:,vi)                        &
     &                - cosmu_nyq(:,ui)*sinnv_nyq(:,vi)

            r = r + SUM(vmec%rmnsf(:,si)*sinmn(:))
            z = z + SUM(vmec%zmncf(:,si)*cosmn(:))

            ru = ru + SUM(vmec%xm*vmec%rmnsf(:,si)*cosmn(:))
            rv = rv - SUM(vmec%xn*vmec%rmnsf(:,si)*cosmn(:))
            zu = zu - SUM(vmec%xm*vmec%zmncf(:,si)*sinmn(:))
            zv = zv + SUM(vmec%xn*vmec%zmncf(:,si)*sinmn(:))

            ju = ju + SUM(vmec%jksupumnsf(:,si)*sinmn_nyq(:))*mu0
            jv = jv + SUM(vmec%jksupvmnsf(:,si)*sinmn_nyq(:))*mu0
         END IF

         jr = ju*ru + jv*rv
         jp = jv*r
         primed_grid_construct_both%j_z(si,ui,vi) = ju*zu + jv*zv

         IF (si .eq. 1 .or. si .eq. vmec%ns) THEN
            jr = jr/2.0
            jp = jp/2.0
            primed_grid_construct_both%j_z(si,ui,vi) =                         &
     &         primed_grid_construct_both%j_z(si,ui,vi)/2.0
         END IF

         primed_grid_construct_both%j_x(si,ui,vi) = jr*cosv(vi)                &
     &                                            - jp*sinv(vi)
         primed_grid_construct_both%j_y(si,ui,vi) = jr*sinv(vi)                &
     &                                            + jp*cosv(vi)

         primed_grid_construct_both%x(si,ui,vi) = r*cosv(vi)
         primed_grid_construct_both%y(si,ui,vi) = r*sinv(vi)
         primed_grid_construct_both%z(si,ui,vi) = z
      END DO
!$OMP END DO

      DEALLOCATE(cosmn)
      DEALLOCATE(sinmn)
      DEALLOCATE(cosmn_nyq)
      IF (vmec%lasym) THEN
         DEALLOCATE(sinmn_nyq)
      END IF
!$OMP END PARALLEL

      DEALLOCATE(cosv)
      DEALLOCATE(sinv)
      DEALLOCATE(cosmu)
      DEALLOCATE(sinmu)
      DEALLOCATE(cosnv)
      DEALLOCATE(sinnv)
      DEALLOCATE(cosmu_nyq)
      DEALLOCATE(sinmu_nyq)
      DEALLOCATE(cosnv_nyq)
      DEALLOCATE(sinnv_nyq)

!  Multi process did not fill out the entire array. Get the missing pieces from
!  the other processes.
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(primed_grid_construct_both%x)
         CALL parallel%reduce(primed_grid_construct_both%y)
         CALL parallel%reduce(primed_grid_construct_both%z)
         CALL parallel%reduce(primed_grid_construct_both%j_x)
         CALL parallel%reduce(primed_grid_construct_both%j_y)
         CALL parallel%reduce(primed_grid_construct_both%j_z)
      END IF

      CALL profiler_set_stop_time('primed_grid_construct_both',                &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref primed_grid_class object.
!>
!>  Allocates memory and initializes a @ref primed_grid_class object with an
!>  This computes the currents and positions on the primed grid. J^u plasma
!>  current is obtained from force balance.
!>
!>  J^u = (p' + J^v*B^u)/B^v
!>
!>  @param[in] num_v    Number of toroidal grid points.
!>  @param[in] vmec     VMEC file object.
!>  @param[in] parallel @ref bmw_parallel_context_class object instance.
!>  @returns A pointer to a constructed @ref primed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION primed_grid_construct_ju(num_v, vmec, parallel)
      USE stel_constants, ONLY: twopi, mu0
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      CLASS (primed_grid_class), POINTER :: primed_grid_construct_ju
      INTEGER, INTENT(in)                            :: num_v
      CLASS (vmec_file_class), POINTER, INTENT(in)   :: vmec
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: i
      REAL (rprec)                                   :: x
      REAL (rprec)                                   :: ds
      INTEGER                                        :: si
      INTEGER                                        :: ui
      INTEGER                                        :: vi
      REAL (rprec)                                   :: r
      REAL (rprec)                                   :: z
      REAL (rprec)                                   :: ru
      REAL (rprec)                                   :: zu
      REAL (rprec)                                   :: rv
      REAL (rprec)                                   :: zv
      REAL (rprec)                                   :: ju
      REAL (rprec)                                   :: jv
      REAL (rprec)                                   :: bu
      REAL (rprec)                                   :: bv
      REAL (rprec)                                   :: jr
      REAL (rprec)                                   :: jp
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosv
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmu_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmu_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosnv_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinnv_nyq
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosmn
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinmn
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosmn_nyq
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinmn_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: rmnch
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: rmnsh
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: zmnch
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: zmnsh
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: currvmnch
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: currvmnsh
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: p_prime
      INTEGER                                        :: num_v_use

!  local parameters
      INTEGER, PARAMETER                             :: num_u = 101
      REAL (rprec), PARAMETER                        :: du = twopi/num_u

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(primed_grid_construct_ju)

      IF (num_v .eq. 1) THEN
         num_v_use = 360
      ELSE
         num_v_use = num_v
      END IF

      ALLOCATE(primed_grid_construct_ju%x(vmec%ns - 1,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_ju%y(vmec%ns - 1,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_ju%z(vmec%ns - 1,num_u,num_v_use))

      ALLOCATE(primed_grid_construct_ju%j_x(vmec%ns - 1,num_u,                 &
     &                                      num_v_use))
      ALLOCATE(primed_grid_construct_ju%j_y(vmec%ns - 1,num_u,                 &
     &                                      num_v_use))
      ALLOCATE(primed_grid_construct_ju%j_z(vmec%ns - 1,num_u,                 &
     &                                      num_v_use))

      ALLOCATE(cosv(num_v_use))
      ALLOCATE(sinv(num_v_use))
      ALLOCATE(cosmu(vmec%mnmax,num_u))
      ALLOCATE(sinmu(vmec%mnmax,num_u))
      ALLOCATE(cosnv(vmec%mnmax,num_v_use))
      ALLOCATE(sinnv(vmec%mnmax,num_v_use))
      ALLOCATE(cosmu_nyq(vmec%mnmax_nyq,num_u))
      ALLOCATE(sinmu_nyq(vmec%mnmax_nyq,num_u))
      ALLOCATE(cosnv_nyq(vmec%mnmax_nyq,num_v_use))
      ALLOCATE(sinnv_nyq(vmec%mnmax_nyq,num_v_use))

      ALLOCATE(rmnch(vmec%mnmax,vmec%ns - 1))
      ALLOCATE(zmnsh(vmec%mnmax,vmec%ns - 1))
      ALLOCATE(currvmnch(vmec%mnmax_nyq,vmec%ns - 1))

      IF (vmec%lasym) THEN
         ALLOCATE(rmnsh(vmec%mnmax,vmec%ns - 1))
         ALLOCATE(zmnch(vmec%mnmax,vmec%ns - 1))
         ALLOCATE(currvmnsh(vmec%mnmax_nyq,vmec%ns - 1))
      END IF

      ALLOCATE(p_prime(vmec%ns - 1))

      ds = 1.0/(vmec%ns - 1.0)
      primed_grid_construct_ju%dv = twopi/num_v_use

      primed_grid_construct_ju%dvol = vmec%isigng*ds*du                        &
     &   * primed_grid_construct_ju%dv/(2.0*twopi)

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, x, si, ui, vi, r, z, ru, rv, zu, zv, ju, jv, bu, bv,         &
!$OMP&         cosmn, sinmn, cosmn_nyq, sinmn_nyq, jr, jp)

!  Multi process will do an all reduce so these arrays need to be initalized.
      IF (parallel%stride .gt. 1) THEN
!$OMP WORKSHARE
         primed_grid_construct_ju%x = 0.0
         primed_grid_construct_ju%y = 0.0
         primed_grid_construct_ju%z = 0.0
         primed_grid_construct_ju%j_x = 0.0
         primed_grid_construct_ju%j_y = 0.0
         primed_grid_construct_ju%j_z = 0.0
         cosv = 0.0
         sinv = 0.0
         cosmu = 0.0
         sinmu = 0.0
         cosnv = 0.0
         sinnv = 0.0
         cosmu_nyq = 0.0
         sinmu_nyq = 0.0
         cosnv_nyq = 0.0
         sinnv_nyq = 0.0
         rmnch = 0.0
         zmnsh = 0.0
         currvmnch = 0.0
         p_prime = 0.0
!$OMP END WORKSHARE
         IF (vmec%lasym) THEN
!$OMP WORKSHARE
            rmnsh = 0.0
            zmnch = 0.0
            currvmnsh = 0.0
!$OMP END WORKSHARE
         END IF
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(vmec%ns - 1), parallel%end(vmec%ns - 1)
         rmnch(:,i) = (vmec%rmncf(:,i + 1) + vmec%rmncf(:,i))/2.0
         zmnsh(:,i) = (vmec%zmnsf(:,i + 1) + vmec%zmnsf(:,i))/2.0

         currvmnch(:,i) = (vmec%jksupvmncf(:,i + 1) +                          &
     &                     vmec%jksupvmncf(:,i))/2.0

         p_prime(i) = (vmec%presf(i + 1) - vmec%presf(i))/ds

         IF (vmec%lasym) THEN
            rmnsh(:,i) = (vmec%rmnsf(:,i + 1) + vmec%rmnsf(:,i))/2.0
            zmnch(:,i) = (vmec%zmncf(:,i + 1) + vmec%zmncf(:,i))/2.0

            currvmnsh(:,i) = (vmec%jksupvmnsf(:,i + 1) +                       &
     &                        vmec%jksupvmnsf(:,i))/2.0
         END IF
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_u), parallel%end(num_u)
         x = (i - 0.5)*du
         cosmu(:,i) = COS(vmec%xm*x)
         sinmu(:,i) = SIN(vmec%xm*x)
         cosmu_nyq(:,i) = COS(vmec%xm_nyq*x)
         sinmu_nyq(:,i) = SIN(vmec%xm_nyq*x)
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_v_use), parallel%end(num_v_use)
         x = (i - 0.5)*primed_grid_construct_ju%dv
         cosv(i) = COS(x)
         sinv(i) = SIN(x)
         cosnv(:,i) = COS(vmec%xn*x)
         sinnv(:,i) = SIN(vmec%xn*x)
         cosnv_nyq(:,i) = COS(vmec%xn_nyq*x)
         sinnv_nyq(:,i) = SIN(vmec%xn_nyq*x)
      END DO
!$OMP END DO

      IF (parallel%stride .gt. 1) THEN
!$OMP SINGLE
         CALL parallel%reduce(cosv)
         CALL parallel%reduce(sinv)
         CALL parallel%reduce(cosmu)
         CALL parallel%reduce(sinmu)
         CALL parallel%reduce(cosmu_nyq)
         CALL parallel%reduce(sinmu_nyq)
         CALL parallel%reduce(cosnv)
         CALL parallel%reduce(sinnv)
         CALL parallel%reduce(cosnv_nyq)
         CALL parallel%reduce(sinnv_nyq)
         CALL parallel%reduce(rmnch)
         CALL parallel%reduce(zmnsh)
         CALL parallel%reduce(currvmnch)
         CALL parallel%reduce(p_prime)
         IF (vmec%lasym) THEN
            CALL parallel%reduce(rmnsh)
            CALL parallel%reduce(zmnch)
            CALL parallel%reduce(currvmnsh)
         END IF
!$OMP END SINGLE
      END IF

      ALLOCATE(cosmn(vmec%mnmax))
      ALLOCATE(sinmn(vmec%mnmax))
      ALLOCATE(cosmn_nyq(vmec%mnmax_nyq))
      IF (vmec%lasym) THEN
         ALLOCATE(sinmn_nyq(vmec%mnmax_nyq))
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start((vmec%ns - 1)*num_u*num_v_use),                    &
     &       parallel%end((vmec%ns - 1)*num_u*num_v_use)
         si = bmw_parallel_context_i(i, vmec%ns - 1)
         ui = bmw_parallel_context_j(i, vmec%ns - 1, num_u)
         vi = bmw_parallel_context_k(i, vmec%ns - 1, num_u)

         cosmn = cosmu(:,ui)*cosnv(:,vi) + sinmu(:,ui)*sinnv(:,vi)
         sinmn = sinmu(:,ui)*cosnv(:,vi) - cosmu(:,ui)*sinnv(:,vi)
         cosmn_nyq = cosmu_nyq(:,ui)*cosnv_nyq(:,vi)                           &
     &             + sinmu_nyq(:,ui)*sinnv_nyq(:,vi)

         r = SUM(rmnch(:,si)*cosmn(:))
         z = SUM(zmnsh(:,si)*sinmn(:))

         ru = -SUM(vmec%xm*rmnch(:,si)*sinmn(:))
         rv =  SUM(vmec%xn*rmnch(:,si)*sinmn(:))
         zu =  SUM(vmec%xm*zmnsh(:,si)*cosmn(:))
         zv = -SUM(vmec%xn*zmnsh(:,si)*cosmn(:))

         jv = SUM(currvmnch(:,si)*cosmn_nyq(:))

         bu = SUM(vmec%bsupumnch(:,si + 1)*cosmn_nyq(:))
         bv = SUM(vmec%bsupvmnch(:,si + 1)*cosmn_nyq(:))

         IF (vmec%lasym) THEN
            sinmn_nyq = sinmu_nyq(:,ui)*cosnv_nyq(:,vi)                        &
     &                - cosmu_nyq(:,ui)*sinnv_nyq(:,vi)

            r = r + SUM(rmnsh(:,si)*sinmn(:))
            z = z + SUM(zmnch(:,si)*cosmn(:))

            ru = ru + SUM(vmec%xm*rmnsh(:,si)*cosmn(:))
            rv = rv - SUM(vmec%xn*rmnsh(:,si)*cosmn(:))
            zu = zu - SUM(vmec%xm*zmnch(:,si)*sinmn(:))
            zv = zv + SUM(vmec%xn*zmnch(:,si)*sinmn(:))

            jv = jv + SUM(currvmnsh(:,si)*sinmn_nyq(:))

            bu = bu + SUM(vmec%bsupumnsh(:,si + 1)*sinmn_nyq(:))
            bv = bv + SUM(vmec%bsupvmnsh(:,si + 1)*sinmn_nyq(:))
         END IF

         ju = (p_prime(si) + jv*bu)/bv*mu0
         jv = jv*mu0

         jr = ju*ru + jv*rv
         jp = jv*r

         primed_grid_construct_ju%j_z(si,ui,vi) = ju*zu + jv*zv
         primed_grid_construct_ju%j_x(si,ui,vi) = jr*cosv(vi)                  &
     &                                          - jp*sinv(vi)
         primed_grid_construct_ju%j_y(si,ui,vi) = jr*sinv(vi)                  &
     &                                          + jp*cosv(vi)

         primed_grid_construct_ju%x(si,ui,vi) = r*cosv(vi)
         primed_grid_construct_ju%y(si,ui,vi) = r*sinv(vi)
         primed_grid_construct_ju%z(si,ui,vi) = z
      END DO
!$OMP END DO

      DEALLOCATE(cosmn)
      DEALLOCATE(sinmn)
      DEALLOCATE(cosmn_nyq)
      IF (vmec%lasym) THEN
         DEALLOCATE(sinmn_nyq)
      END IF
!$OMP END PARALLEL

      DEALLOCATE(cosv)
      DEALLOCATE(sinv)
      DEALLOCATE(cosmu)
      DEALLOCATE(sinmu)
      DEALLOCATE(cosnv)
      DEALLOCATE(sinnv)
      DEALLOCATE(cosmu_nyq)
      DEALLOCATE(sinmu_nyq)
      DEALLOCATE(cosnv_nyq)
      DEALLOCATE(sinnv_nyq)

      DEALLOCATE(rmnch)
      DEALLOCATE(zmnsh)
      DEALLOCATE(currvmnch)
      DEALLOCATE(p_prime)
      IF (vmec%lasym) THEN
         DEALLOCATE(rmnsh)
         DEALLOCATE(zmnch)
         DEALLOCATE(currvmnsh)
      END IF

!  Multi process did not fill out the entire array. Get the missing pieces from
!  the other processes.
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(primed_grid_construct_ju%x)
         CALL parallel%reduce(primed_grid_construct_ju%y)
         CALL parallel%reduce(primed_grid_construct_ju%z)
         CALL parallel%reduce(primed_grid_construct_ju%j_x)
         CALL parallel%reduce(primed_grid_construct_ju%j_y)
         CALL parallel%reduce(primed_grid_construct_ju%j_z)
      END IF

      CALL profiler_set_stop_time('primed_grid_construct_ju',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref primed_grid_class object.
!>
!>  Allocates memory and initializes a @ref primed_grid_class object with an
!>  This computes the currents and positions on the primed grid. J^v plasma
!>  current is obtained from force balance.
!>
!>  J^v = (J^u*B^v - p')/B^u
!>
!>  @param[in] num_v    Number of toroidal grid points.
!>  @param[in] vmec     VMEC file object.
!>  @param[in] parallel @ref bmw_parallel_context_class object instance.
!>  @returns A pointer to a constructed @ref primed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION primed_grid_construct_jv(num_v, vmec, parallel)
      USE stel_constants, ONLY: twopi, mu0
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      CLASS (primed_grid_class), POINTER :: primed_grid_construct_jv
      INTEGER, INTENT(in)                            :: num_v
      CLASS (vmec_file_class), POINTER, INTENT(in)   :: vmec
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: i
      REAL (rprec)                                   :: x
      REAL (rprec)                                   :: ds
      INTEGER                                        :: si
      INTEGER                                        :: ui
      INTEGER                                        :: vi
      REAL (rprec)                                   :: r
      REAL (rprec)                                   :: z
      REAL (rprec)                                   :: ru
      REAL (rprec)                                   :: zu
      REAL (rprec)                                   :: rv
      REAL (rprec)                                   :: zv
      REAL (rprec)                                   :: ju
      REAL (rprec)                                   :: jv
      REAL (rprec)                                   :: bu
      REAL (rprec)                                   :: bv
      REAL (rprec)                                   :: jr
      REAL (rprec)                                   :: jp
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosv
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmu_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmu_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosnv_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinnv_nyq
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosmn
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinmn
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosmn_nyq
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinmn_nyq
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: rmnch
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: rmnsh
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: zmnch
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: zmnsh
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: currumnch
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: currumnsh
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: p_prime
      INTEGER                                        :: num_v_use

!  local parameters
      INTEGER, PARAMETER                             :: num_u = 101
      REAL (rprec), PARAMETER                        :: du = twopi/num_u

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(primed_grid_construct_jv)

      IF (num_v .eq. 1) THEN
         num_v_use = 360
      ELSE
         num_v_use = num_v
      END IF

      ALLOCATE(primed_grid_construct_jv%x(vmec%ns - 1,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_jv%y(vmec%ns - 1,num_u,num_v_use))
      ALLOCATE(primed_grid_construct_jv%z(vmec%ns - 1,num_u,num_v_use))

      ALLOCATE(primed_grid_construct_jv%j_x(vmec%ns - 1,num_u,                 &
     &                                      num_v_use))
      ALLOCATE(primed_grid_construct_jv%j_y(vmec%ns - 1,num_u,                 &
     &                                      num_v_use))
      ALLOCATE(primed_grid_construct_jv%j_z(vmec%ns - 1,num_u,                 &
     &                                      num_v_use))

      ALLOCATE(cosv(num_v_use))
      ALLOCATE(sinv(num_v_use))
      ALLOCATE(cosmu(vmec%mnmax,num_u))
      ALLOCATE(sinmu(vmec%mnmax,num_u))
      ALLOCATE(cosnv(vmec%mnmax,num_v_use))
      ALLOCATE(sinnv(vmec%mnmax,num_v_use))
      ALLOCATE(cosmu_nyq(vmec%mnmax_nyq,num_u))
      ALLOCATE(sinmu_nyq(vmec%mnmax_nyq,num_u))
      ALLOCATE(cosnv_nyq(vmec%mnmax_nyq,num_v_use))
      ALLOCATE(sinnv_nyq(vmec%mnmax_nyq,num_v_use))

      ALLOCATE(rmnch(vmec%mnmax,vmec%ns - 1))
      ALLOCATE(zmnsh(vmec%mnmax,vmec%ns - 1))
      ALLOCATE(currumnch(vmec%mnmax_nyq,vmec%ns - 1))

      IF (vmec%lasym) THEN
         ALLOCATE(rmnsh(vmec%mnmax,vmec%ns - 1))
         ALLOCATE(zmnch(vmec%mnmax,vmec%ns - 1))
         ALLOCATE(currumnsh(vmec%mnmax_nyq,vmec%ns - 1))
      END IF

      ALLOCATE(p_prime(vmec%ns - 1))

      ds = 1.0/(vmec%ns - 1.0)
      primed_grid_construct_jv%dv = twopi/num_v_use

      primed_grid_construct_jv%dvol = vmec%isigng*ds*du                        &
     &   * primed_grid_construct_jv%dv/(2.0*twopi)

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, x, si, ui, vi, r, z, ru, rv, zu, zv, ju, jv, bu, bv,         &
!$OMP&         cosmn, sinmn, cosmn_nyq, sinmn_nyq, jr, jp)

!  Multi process will do an all reduce so these arrays need to be initalized.
      IF (parallel%stride .gt. 1) THEN
!$OMP WORKSHARE
         primed_grid_construct_jv%x = 0.0
         primed_grid_construct_jv%y = 0.0
         primed_grid_construct_jv%z = 0.0
         primed_grid_construct_jv%j_x = 0.0
         primed_grid_construct_jv%j_y = 0.0
         primed_grid_construct_jv%j_z = 0.0
         cosv = 0.0
         sinv = 0.0
         cosmu = 0.0
         sinmu = 0.0
         cosnv = 0.0
         sinnv = 0.0
         cosmu_nyq = 0.0
         sinmu_nyq = 0.0
         cosnv_nyq = 0.0
         sinnv_nyq = 0.0
         rmnch = 0.0
         zmnsh = 0.0
         currumnch = 0.0
         p_prime = 0.0
!$OMP END WORKSHARE
         IF (vmec%lasym) THEN
            rmnsh = 0.0
            zmnch = 0.0
            currumnsh = 0.0
         END IF
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(vmec%ns - 1), parallel%end(vmec%ns - 1)
         si = bmw_parallel_context_i(i, vmec%ns - 1)

         rmnch(:,si) = (vmec%rmncf(:,si + 1) + vmec%rmncf(:,si))/2.0
         zmnsh(:,si) = (vmec%zmnsf(:,si + 1) + vmec%zmnsf(:,si))/2.0

         currumnch(:,si) = (vmec%jksupvmncf(:,si + 1) +                        &
     &                      vmec%jksupvmncf(:,si))/2.0

         p_prime(si) = (vmec%presf(si + 1) - vmec%presf(si))/ds

         IF (vmec%lasym) THEN
            rmnsh(:,si) = (vmec%rmnsf(:,si + 1) + vmec%rmnsf(:,si))/2.0
            zmnch(:,si) = (vmec%zmncf(:,si + 1) + vmec%zmncf(:,si))/2.0

            currumnsh(:,si) = (vmec%jksupvmnsf(:,si + 1) +                     &
     &                         vmec%jksupvmnsf(:,si))/2.0
         END IF
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_u), parallel%end(num_u)
         x = (i - 0.5)*du
         cosmu(:,i) = COS(vmec%xm*x)
         sinmu(:,i) = SIN(vmec%xm*x)
         cosmu_nyq(:,i) = COS(vmec%xm_nyq*x)
         sinmu_nyq(:,i) = SIN(vmec%xm_nyq*x)
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_v_use), parallel%end(num_v_use)
         x = (i - 0.5)*primed_grid_construct_jv%dv
         cosv(i) = COS(x)
         sinv(i) = SIN(x)
         cosnv(:,i) = COS(vmec%xn*x)
         sinnv(:,i) = SIN(vmec%xn*x)
         cosnv_nyq(:,i) = COS(vmec%xn_nyq*x)
         sinnv_nyq(:,i) = SIN(vmec%xn_nyq*x)
      END DO
!$OMP END DO

      IF (parallel%stride .gt. 1) THEN
!$OMP SINGLE
         CALL parallel%reduce(cosv)
         CALL parallel%reduce(sinv)
         CALL parallel%reduce(cosmu)
         CALL parallel%reduce(sinmu)
         CALL parallel%reduce(cosmu_nyq)
         CALL parallel%reduce(sinmu_nyq)
         CALL parallel%reduce(cosnv)
         CALL parallel%reduce(sinnv)
         CALL parallel%reduce(cosnv_nyq)
         CALL parallel%reduce(sinnv_nyq)
         CALL parallel%reduce(rmnch)
         CALL parallel%reduce(zmnsh)
         CALL parallel%reduce(currumnch)
         CALL parallel%reduce(p_prime)
         IF (vmec%lasym) THEN
            CALL parallel%reduce(rmnsh)
            CALL parallel%reduce(zmnch)
            CALL parallel%reduce(currumnsh)
         END IF
!$OMP END SINGLE
      END IF

      ALLOCATE(cosmn(vmec%mnmax))
      ALLOCATE(sinmn(vmec%mnmax))
      ALLOCATE(cosmn_nyq(vmec%mnmax_nyq))
      IF (vmec%lasym) THEN
         ALLOCATE(sinmn_nyq(vmec%mnmax_nyq))
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start((vmec%ns - 1)*num_u*num_v_use),                    &
     &       parallel%end((vmec%ns - 1)*num_u*num_v_use)
         si = bmw_parallel_context_i(i, vmec%ns - 1)
         ui = bmw_parallel_context_j(i, vmec%ns - 1, num_u)
         vi = bmw_parallel_context_k(i, vmec%ns - 1, num_u)

         cosmn = cosmu(:,ui)*cosnv(:,vi) + sinmu(:,ui)*sinnv(:,vi)
         sinmn = sinmu(:,ui)*cosnv(:,vi) - cosmu(:,ui)*sinnv(:,vi)
         cosmn_nyq = cosmu_nyq(:,ui)*cosnv_nyq(:,vi)                           &
     &             + sinmu_nyq(:,ui)*sinnv_nyq(:,vi)

         r = SUM(rmnch(:,si)*cosmn(:))
         z = SUM(zmnsh(:,si)*sinmn(:))

         ru = -SUM(vmec%xm*rmnch(:,si)*sinmn(:))
         rv =  SUM(vmec%xn*rmnch(:,si)*sinmn(:))
         zu =  SUM(vmec%xm*zmnsh(:,si)*cosmn(:))
         zv = -SUM(vmec%xn*zmnsh(:,si)*cosmn(:))

         ju = SUM(currumnch(:,si)*cosmn_nyq(:))

         bu = SUM(vmec%bsupumnch(:,si + 1)*cosmn_nyq(:))
         bv = SUM(vmec%bsupvmnch(:,si + 1)*cosmn_nyq(:))

         IF (vmec%lasym) THEN
            sinmn_nyq = sinmu_nyq(:,ui)*cosnv_nyq(:,vi)                        &
     &                - cosmu_nyq(:,ui)*sinnv_nyq(:,vi)

            r = r + SUM(rmnsh(:,si)*sinmn(:))
            z = z + SUM(zmnch(:,si)*cosmn(:))

            ru = ru + SUM(vmec%xm*rmnsh(:,si)*cosmn(:))
            rv = rv - SUM(vmec%xn*rmnsh(:,si)*cosmn(:))
            zu = zu - SUM(vmec%xm*zmnch(:,si)*sinmn(:))
            zv = zv + SUM(vmec%xn*zmnch(:,si)*sinmn(:))

            ju = ju + SUM(currumnsh(:,si)*sinmn_nyq(:))

            bu = bu + SUM(vmec%bsupumnsh(:,si + 1)*sinmn_nyq(:))
            bv = bv + SUM(vmec%bsupvmnsh(:,si + 1)*sinmn_nyq(:))
         END IF

         jv = (ju*bv - p_prime(si))/bu*mu0
         ju = ju*mu0

         jr = ju*ru + jv*rv
         jp = jv*r

         primed_grid_construct_jv%j_z(si,ui,vi) = ju*zu + jv*zv
         primed_grid_construct_jv%j_x(si,ui,vi) = jr*cosv(vi)                  &
     &                                          - jp*sinv(vi)
         primed_grid_construct_jv%j_y(si,ui,vi) = jr*sinv(vi)                  &
     &                                          + jp*cosv(vi)

         primed_grid_construct_jv%x(si,ui,vi) = r*cosv(vi)
         primed_grid_construct_jv%y(si,ui,vi) = r*sinv(vi)
         primed_grid_construct_jv%z(si,ui,vi) = z
      END DO
!$OMP END DO

      DEALLOCATE(cosmn)
      DEALLOCATE(sinmn)
      DEALLOCATE(cosmn_nyq)
      IF (vmec%lasym) THEN
         DEALLOCATE(sinmn_nyq)
      END IF
!$OMP END PARALLEL

      DEALLOCATE(cosv)
      DEALLOCATE(sinv)
      DEALLOCATE(cosmu)
      DEALLOCATE(sinmu)
      DEALLOCATE(cosnv)
      DEALLOCATE(sinnv)
      DEALLOCATE(cosmu_nyq)
      DEALLOCATE(sinmu_nyq)
      DEALLOCATE(cosnv_nyq)
      DEALLOCATE(sinnv_nyq)

      DEALLOCATE(rmnch)
      DEALLOCATE(zmnsh)
      DEALLOCATE(currumnch)
      DEALLOCATE(p_prime)
      IF (vmec%lasym) THEN
         DEALLOCATE(rmnsh)
         DEALLOCATE(zmnch)
         DEALLOCATE(currumnsh)
      END IF

!  Multi process did not fill out the entire array. Get the missing pieces from
!  the other processes.
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(primed_grid_construct_jv%x)
         CALL parallel%reduce(primed_grid_construct_jv%y)
         CALL parallel%reduce(primed_grid_construct_jv%z)
         CALL parallel%reduce(primed_grid_construct_jv%j_x)
         CALL parallel%reduce(primed_grid_construct_jv%j_y)
         CALL parallel%reduce(primed_grid_construct_jv%j_z)
      END IF

      CALL profiler_set_stop_time('primed_grid_construct_jv',                  &
     &                            start_time)

      END FUNCTION

!-------------------------------------------------------------------------------
!>  @brief Construct a @ref primed_grid_class object.
!>
!>  Allocates memory and initializes a @ref primed_grid_class object with an
!>  This computes the currents and positions on the primed grid. Plasma currents
!>  are obtained from Curl(B) of the siesta solution.
!>
!>  @param[in] num_v            Number of toroidal grid points.
!>  @param[in] vmec             VMEC file object.
!>  @param[in] siesta_file_name Name of the siesta restart file.
!>  @param[in] parallel         @ref bmw_parallel_context_class object instance.
!>  @returns A pointer to a constructed @ref primed_grid_class object.
!-------------------------------------------------------------------------------
      FUNCTION primed_grid_construct_siesta(num_v, vmec,                       &
     &                                      siesta_file_name, parallel)
      USE stel_constants, ONLY: twopi, mu0
      USE siesta_file
      USE vmec_file

      IMPLICIT NONE

!  Declare Arguments
      CLASS (primed_grid_class), POINTER :: primed_grid_construct_siesta
      INTEGER, INTENT(in)                            :: num_v
      CLASS (vmec_file_class), POINTER, INTENT(in)   :: vmec
      CHARACTER (len=*)                              :: siesta_file_name
      CLASS (bmw_parallel_context_class), INTENT(in) :: parallel

!  local variables
      REAL (rprec)                                   :: start_time
      INTEGER                                        :: i
      REAL (rprec)                                   :: x
      REAL (rprec)                                   :: ds
      INTEGER                                        :: si
      INTEGER                                        :: ui
      INTEGER                                        :: vi
      REAL (rprec)                                   :: r
      REAL (rprec)                                   :: z
      REAL (rprec)                                   :: rs
      REAL (rprec)                                   :: zs
      REAL (rprec)                                   :: ru
      REAL (rprec)                                   :: zu
      REAL (rprec)                                   :: rv
      REAL (rprec)                                   :: zv
      REAL (rprec)                                   :: js
      REAL (rprec)                                   :: ju
      REAL (rprec)                                   :: jv
      REAL (rprec)                                   :: jr
      REAL (rprec)                                   :: jp
      REAL (rprec)                                   :: jz
      INTEGER                                        :: m
      INTEGER                                        :: n
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: cosv
      REAL (rprec), DIMENSION(:), ALLOCATABLE        :: sinv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmu
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinnv
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: cosmn
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: sinmn
      REAL (rprec), DIMENSION(:,:), ALLOCATABLE      :: amnint
      INTEGER                                        :: num_v_use
      CLASS (siesta_file_class), POINTER             :: siesta

!  local parameters
      INTEGER, PARAMETER                             :: num_u = 101
      REAL (rprec), PARAMETER                        :: du = twopi/num_u

!  Start of executable code
      start_time = profiler_get_start_time()

      siesta => siesta_file_class(TRIM(siesta_file_name))

      ALLOCATE(primed_grid_construct_siesta)

      IF (num_v .eq. 1) THEN
         num_v_use = 360
      ELSE
         num_v_use = num_v
      END IF

      ALLOCATE(primed_grid_construct_siesta%x(siesta%nrad,num_u,               &
     &                                        num_v_use))
      ALLOCATE(primed_grid_construct_siesta%y(siesta%nrad,num_u,               &
     &                                        num_v_use))
      ALLOCATE(primed_grid_construct_siesta%z(siesta%nrad,num_u,               &
     &                                        num_v_use))

      ALLOCATE(primed_grid_construct_siesta%j_x(siesta%nrad,num_u,             &
     &                                          num_v_use))
      ALLOCATE(primed_grid_construct_siesta%j_y(siesta%nrad,num_u,             &
     &                                          num_v_use))
      ALLOCATE(primed_grid_construct_siesta%j_z(siesta%nrad,num_u,             &
     &                                          num_v_use))

      ALLOCATE(cosv(num_v_use))
      ALLOCATE(sinv(num_v_use))
      ALLOCATE(cosmu(0:siesta%mpol,num_u))
      ALLOCATE(sinmu(0:siesta%mpol,num_u))
      ALLOCATE(cosnv(-siesta%ntor:siesta%ntor,num_v_use))
      ALLOCATE(sinnv(-siesta%ntor:siesta%ntor,num_v_use))

      ds = 1.0/(siesta%nrad - 1.0)
      primed_grid_construct_siesta%dv = twopi/num_v_use

!  FIXME: Get the sign of the jacobian from the siesta restart file.
      primed_grid_construct_siesta%dvol = vmec%isigng*ds*du                    &
     &   * primed_grid_construct_siesta%dv/(2.0*twopi)

!$OMP PARALLEL
!$OMP& DEFAULT(SHARED)
!$OMP& PRIVATE(i, x, si, ui, vi, n, m, r, ru, rv, z, zu, zv, rs, zs,           &
!$OMP&         js, ju, jv, jr, jp, jz, cosmn, sinmn, amnint)

!  Multi process will do an all reduce so these arrays need to be initalized.
      IF (parallel%stride .gt. 1) THEN
!$OMP WORKSHARE
         primed_grid_construct_siesta%x = 0.0
         primed_grid_construct_siesta%y = 0.0
         primed_grid_construct_siesta%z = 0.0
         primed_grid_construct_siesta%j_x = 0.0
         primed_grid_construct_siesta%j_y = 0.0
         primed_grid_construct_siesta%j_z = 0.0
         cosmu = 0.0
         cosnv = 0.0
         sinmu = 0.0
         sinnv = 0.0
         cosv  = 0.0
         sinv  = 0.0
!$OMP END WORKSHARE
      END IF

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_u), parallel%end(num_u)
         x = (i - 0.5)*du

         DO m = 0, siesta%mpol
            cosmu(m,i) = COS(m*x)
            sinmu(m,i) = SIN(m*x)
         END DO
      END DO
!$OMP END DO

!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(num_v_use), parallel%end(num_v_use)
         x = (i - 0.5)*primed_grid_construct_siesta%dv
         cosv(i) = COS(x)
         sinv(i) = SIN(x)

         DO n = -siesta%ntor, siesta%ntor
            cosnv(n,i) = COS(siesta%tor_modes(n)*x)
            sinnv(n,i) = SIN(siesta%tor_modes(n)*x)
         END DO
      END DO
!$OMP END DO

      IF (parallel%stride .gt. 1) THEN
!$OMP SINGLE
         CALL parallel%reduce(cosv)
         CALL parallel%reduce(sinv)
         CALL parallel%reduce(cosmu)
         CALL parallel%reduce(sinmu)
         CALL parallel%reduce(cosnv)
         CALL parallel%reduce(sinnv)
!$OMP END SINGLE
      END IF

      ALLOCATE(cosmn(0:siesta%mpol,-siesta%ntor:siesta%ntor))
      ALLOCATE(sinmn(0:siesta%mpol,-siesta%ntor:siesta%ntor))

      ALLOCATE(amnint(0:siesta%mpol,-siesta%ntor:siesta%ntor))
!$OMP DO
!$OMP& SCHEDULE(STATIC)
      DO i = parallel%start(siesta%nrad*num_u*num_v_use),                      &
     &       parallel%end(siesta%nrad*num_u*num_v_use)
         si = bmw_parallel_context_i(i, siesta%nrad)
         ui = bmw_parallel_context_j(i, siesta%nrad, num_u)
         vi = bmw_parallel_context_k(i, siesta%nrad, num_v_use)

         ru = 0.0
         rv = 0.0

         zu = 0.0
         zv = 0.0

         DO n = -siesta%ntor, siesta%ntor
            DO m = 0, siesta%mpol
               cosmn(m,n) = cosmu(m, ui)*cosnv(n, vi)                          &
     &                    - sinmu(m, ui)*sinnv(n, vi)
               sinmn(m,n) = sinmu(m, ui)*cosnv(n, vi)                          &
     &                    + cosmu(m, ui)*sinnv(n, vi)

               ru = ru - m*siesta%rmncf(m, n, si)*sinmn(m,n)
               rv = rv                                                         &
     &            + siesta%tor_modes(n)*siesta%rmncf(m,n,si)*sinmn(m,n)

               zu = zu + m*siesta%zmnsf(m, n, si)*cosmn(m,n)
               zv = zv                                                         &
     &            - siesta%tor_modes(n)*siesta%zmnsf(m,n,si)*cosmn(m,n)
            END DO
         END DO

         r = SUM(siesta%rmncf(:,:,si)*cosmn)
         z = SUM(siesta%zmnsf(:,:,si)*sinmn)

         IF (si .eq. 1) THEN
            amnint = 0
            amnint(0,:) = 0
            amnint(1,:) = siesta%rmncf(1,:,1)/ds
         ELSE IF (si .eq. siesta%nrad) THEN
            amnint = (siesta%rmncf(:,:,siesta%nrad) -                          &
     &                siesta%rmncf(:,:,siesta%nrad-1))/ds
         ELSE
            amnint = (siesta%rmncf(:,:,si+1) -                                 &
     &                siesta%rmncf(:,:,si-1))/(2.0*ds)
         ENDIF
         rs = SUM(amnint*cosmn)

         IF (si .eq. 1) THEN
            amnint = 0
            amnint(0,:) = 0
            amnint(1,:) = siesta%zmnsf(1,:,1)/ds
         ELSE IF (si .eq. siesta%nrad) THEN
            amnint = (siesta%zmnsf(:,:,siesta%nrad) -                          &
     &                siesta%zmnsf(:,:,siesta%nrad-1))/ds
         ELSE
            amnint = (siesta%zmnsf(:,:,si+1) -                                 &
     &                siesta%zmnsf(:,:,si-1))/(2.0*ds)
         ENDIF
         zs = SUM(amnint*sinmn)

         js = SUM(siesta%jksupsmnsf(:,:,si)*sinmn)
         ju = SUM(siesta%jksupumncf(:,:,si)*cosmn)
         jv = SUM(siesta%jksupvmncf(:,:,si)*cosmn)
         IF (BTEST(siesta%flags, siesta_lasym_flag)) THEN
            DO n = -siesta%ntor, siesta%ntor
               DO m = 0, siesta%mpol
                  ru = ru + m*siesta%rmnsf(m, n, si)*cosmn(m,n)
                  rv = rv                                                      &
     &               - siesta%tor_modes(n)*siesta%rmnsf(m,n,si) *              &
     &                 cosmn(m,n)

                  zu = zu - m*siesta%zmncf(m, n, si)*sinmn(m,n)
                  zv = zv                                                      &
     &               + siesta%tor_modes(n)*siesta%zmncf(m,n,si) *              &
     &                 sinmn(m,n)
               END DO
            END DO

            r = r + SUM(siesta%rmnsf(:,:,si)*sinmn)
            z = z + SUM(siesta%zmncf(:,:,si)*cosmn)

            IF (si .eq. 1) THEN
               amnint = 0
               amnint(0,:) = 0
               amnint(1,:) = siesta%rmnsf(1,:,1)/ds
            ELSE IF (si .eq. siesta%nrad) THEN
               amnint = (siesta%rmnsf(:,:,siesta%nrad) -                       &
     &                   siesta%rmnsf(:,:,siesta%nrad-1))/ds
            ELSE
               amnint = (siesta%rmnsf(:,:,si+1) -                              &
     &                   siesta%rmnsf(:,:,si-1))/(2.0*ds)
            ENDIF
            rs = rs + SUM(amnint*sinmn)

            IF (si .eq. 1) THEN
               amnint = 0
               amnint(0,:) = 0
               amnint(1,:) = siesta%zmncf(1,:,1)/ds
            ELSE IF (si .eq. siesta%nrad) THEN
               amnint = (siesta%zmncf(:,:,siesta%nrad) -                       &
     &                   siesta%zmncf(:,:,siesta%nrad-1))/ds
            ELSE
               amnint = (siesta%zmncf(:,:,si+1) -                              &
     &                   siesta%zmncf(:,:,si-1))/(2.0*ds)
            ENDIF
            zs = zs + SUM(amnint*cosmn)

            js = js + SUM(siesta%jksupsmncf(:,:,si)*cosmn)
            ju = ju + SUM(siesta%jksupumnsf(:,:,si)*sinmn)
            jv = jv + SUM(siesta%jksupvmnsf(:,:,si)*sinmn)

         END IF

         js = js/(siesta%b_factor*mu0)
         ju = ju/(siesta%b_factor*mu0)
         jv = jv/(siesta%b_factor*mu0)

         jr = js*rs + ju*ru + jv*rv
         jp = jv*r
         jz = js*zs + ju*zu + jv*zv

         IF (si .eq. 1 .or. si .eq. siesta%nrad) THEN
            jr = jr/2.0
            jp = jp/2.0
            jz = jz/2.0
         END IF

         primed_grid_construct_siesta%j_x(si,ui,vi) = jr*cosv(vi)              &
     &                                              - jp*sinv(vi)
         primed_grid_construct_siesta%j_y(si,ui,vi) = jr*sinv(vi)              &
     &                                              + jp*cosv(vi)
         primed_grid_construct_siesta%j_z(si,ui,vi) = jz

         primed_grid_construct_siesta%x(si,ui,vi) = r*cosv(vi)
         primed_grid_construct_siesta%y(si,ui,vi) = r*sinv(vi)
         primed_grid_construct_siesta%z(si,ui,vi) = z
      END DO
!$OMP END DO

      DEALLOCATE(cosmn)
      DEALLOCATE(sinmn)

      DEALLOCATE(amnint)
!$OMP END PARALLEL

      DEALLOCATE(cosv)
      DEALLOCATE(sinv)
      DEALLOCATE(cosmu)
      DEALLOCATE(sinmu)
      DEALLOCATE(cosnv)
      DEALLOCATE(sinnv)

!  Multi process did not fill out the entire array. Get the missing pieces from
!  the other processes.
      IF (parallel%stride .gt. 1) THEN
         CALL parallel%reduce(primed_grid_construct_siesta%x)
         CALL parallel%reduce(primed_grid_construct_siesta%y)
         CALL parallel%reduce(primed_grid_construct_siesta%z)
         CALL parallel%reduce(primed_grid_construct_siesta%j_x)
         CALL parallel%reduce(primed_grid_construct_siesta%j_y)
         CALL parallel%reduce(primed_grid_construct_siesta%j_z)
      END IF

      DEALLOCATE(siesta)

      CALL profiler_set_stop_time('primed_grid_construct_siesta',              &
     &                            start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref primed_grid_class object.
!>
!>  Deallocates memory and uninitializes a @ref primed_grid_class object.
!>
!>  @param[inout] this A @ref primed_grid_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE primed_grid_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (primed_grid_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%x)) THEN
         DEALLOCATE(this%x)
         this%x => null()
      END IF

      IF (ASSOCIATED(this%y)) THEN
         DEALLOCATE(this%y)
         this%y => null()
      END IF

      IF (ASSOCIATED(this%z)) THEN
         DEALLOCATE(this%z)
         this%z => null()
      END IF

      IF (ASSOCIATED(this%j_x)) THEN
         DEALLOCATE(this%j_x)
         this%j_x => null()
      END IF

      IF (ASSOCIATED(this%j_y)) THEN
         DEALLOCATE(this%j_y)
         this%j_y => null()
      END IF

      IF (ASSOCIATED(this%j_z)) THEN
         DEALLOCATE(this%j_z)
         this%j_z => null()
      END IF

      END SUBROUTINE

      END MODULE
