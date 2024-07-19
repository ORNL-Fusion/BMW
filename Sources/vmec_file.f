!*******************************************************************************
!>  @file vmec_file.f
!>  @brief Contains module @ref vmec_file.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref vmec_file_class. This contains the
!>  output of a vmec equilibrium.
!*******************************************************************************
      MODULE vmec_file
      USE stel_kinds, ONLY: rprec
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) vmec file base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a vmec output.
!-------------------------------------------------------------------------------
      TYPE vmec_file_class
!  Number of radial points.
         INTEGER :: ns

!  Number of modes.
         INTEGER :: mnmax
!  Number of modes.
         INTEGER :: isigng
!  Number of field periods.
         INTEGER :: nfp

!  External current array.
         REAL (rprec), DIMENSION(:), POINTER :: extcur => null()

!  Poloidal mode spectrum.
         REAL (rprec), DIMENSION(:), POINTER :: xm => null()
!  Toroidal mode spectrum.
         REAL (rprec), DIMENSION(:), POINTER :: xn => null()

!  Number of nyquist modes.
         INTEGER :: mnmax_nyq

!  Poloidal mode nyquist spectrum.
         REAL (rprec), DIMENSION(:), POINTER :: xm_nyq => null()
!  Toroidal mode nyquist spectrum.
         REAL (rprec), DIMENSION(:), POINTER :: xn_nyq => null()

!  Flag to indicate stellarator symmetry.
         LOGICAL :: lasym

!  Pressure profile.
         REAL (rprec), DIMENSION(:), POINTER :: presf => null()

!  R half parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: rmncf => null()
!  Z half parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: zmnsf => null()
!  R full parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: rmnsf => null()
!  Z full parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: zmncf => null()

!  B^u half parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: bsupumnch => null()
!  B^v half parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: bsupvmnch => null()
!  B^u full parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: bsupumnsh => null()
!  B^v full parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: bsupvmnsh => null()

!  J^u current density half parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: jksupumncf => null()
!  J^v current density half parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: jksupvmncf => null()
!  J^u current density full parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: jksupumnsf => null()
!  J^v current density full parity.
         REAL (rprec), DIMENSION(:,:), POINTER :: jksupvmnsf => null()
      CONTAINS
         FINAL :: vmec_file_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Interface for the vmec_file_class constructor.
!-------------------------------------------------------------------------------
      INTERFACE vmec_file_class
         MODULE PROCEDURE vmec_file_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref vmec_file_class object.
!>
!>  Allocates memory and initializes a @ref vmec_file_class object with an
!>  siesta restart file.
!>
!>  @param[in] vmec_file_name File name for vacuum fields.
!>  @returns A pointer to a constructed @ref vmec_file_class object.
!-------------------------------------------------------------------------------
      FUNCTION vmec_file_construct(vmec_file_name)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (vmec_file_class), POINTER :: vmec_file_construct
      CHARACTER (len=*), INTENT(in)    :: vmec_file_name

!  local variables
      REAL (rprec)                     :: start_time
      INTEGER                          :: vmec_ncid
      INTEGER                          :: status
      INTEGER                          :: nextcur

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(vmec_file_construct)

      CALL cdf_open(vmec_ncid, TRIM(vmec_file_name), 'r', status)

      CALL cdf_read(vmec_ncid, 'ns', vmec_file_construct%ns)

      CALL cdf_read(vmec_ncid, 'signgs', vmec_file_construct%isigng)
      CALL cdf_read(vmec_ncid, 'nfp', vmec_file_construct%nfp)

      CALL cdf_read(vmec_ncid, 'nextcur', nextcur)
      ALLOCATE(vmec_file_construct%extcur(nextcur))
      CALL cdf_read(vmec_ncid, 'extcur', vmec_file_construct%extcur)

      CALL cdf_read(vmec_ncid, 'mnmax', vmec_file_construct%mnmax)
      CALL cdf_read(vmec_ncid, 'mnmax_nyq',                                    &
     &              vmec_file_construct%mnmax_nyq)

      ALLOCATE(vmec_file_construct%xm(vmec_file_construct%mnmax))
      ALLOCATE(vmec_file_construct%xn(vmec_file_construct%mnmax))

      ALLOCATE(vmec_file_construct%xm_nyq(                                     &
     &            vmec_file_construct%mnmax_nyq))
      ALLOCATE(vmec_file_construct%xn_nyq(                                     &
     &            vmec_file_construct%mnmax_nyq))

      ALLOCATE(vmec_file_construct%presf(vmec_file_construct%ns))

      ALLOCATE(vmec_file_construct%rmncf(                                      &
     &   vmec_file_construct%ns, vmec_file_construct%mnmax))
      ALLOCATE(vmec_file_construct%zmnsf(                                      &
     &   vmec_file_construct%ns, vmec_file_construct%mnmax))

      ALLOCATE(vmec_file_construct%bsupumnch(                                  &
     &   vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))
      ALLOCATE(vmec_file_construct%bsupvmnch(                                  &
     &   vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))

      ALLOCATE(vmec_file_construct%jksupumncf(                                 &
     &   vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))
      ALLOCATE(vmec_file_construct%jksupvmncf(                                 &
     &   vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))

      CALL cdf_read(vmec_ncid, 'xm', vmec_file_construct%xm)
      CALL cdf_read(vmec_ncid, 'xn', vmec_file_construct%xn)

      CALL cdf_read(vmec_ncid, 'xm_nyq', vmec_file_construct%xm_nyq)
      CALL cdf_read(vmec_ncid, 'xn_nyq', vmec_file_construct%xn_nyq)

      CALL cdf_read(vmec_ncid, 'presf', vmec_file_construct%presf)

      CALL cdf_read(vmec_ncid, 'rmnc', vmec_file_construct%rmncf)
      CALL cdf_read(vmec_ncid, 'zmns', vmec_file_construct%zmnsf)

      CALL cdf_read(vmec_ncid, 'bsupumnc',                                     &
     &              vmec_file_construct%bsupumnch)
      CALL cdf_read(vmec_ncid, 'bsupvmnc',                                     &
     &              vmec_file_construct%bsupvmnch)

      CALL cdf_read(vmec_ncid, 'currumnc',                                     &
     &              vmec_file_construct%jksupumncf)
      CALL cdf_read(vmec_ncid, 'currvmnc',                                     &
     &              vmec_file_construct%jksupvmncf)

      CALL cdf_read(vmec_ncid, 'lasym', vmec_file_construct%lasym)

      IF (vmec_file_construct%lasym) THEN
         ALLOCATE(vmec_file_construct%rmnsf(                                   &
     &      vmec_file_construct%ns, vmec_file_construct%mnmax))
         ALLOCATE(vmec_file_construct%zmncf(                                   &
     &      vmec_file_construct%ns, vmec_file_construct%mnmax))

         ALLOCATE(vmec_file_construct%bsupumnsh(                               &
     &      vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))
         ALLOCATE(vmec_file_construct%bsupvmnsh(                               &
     &      vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))

         ALLOCATE(vmec_file_construct%jksupumnsf(                              &
     &      vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))
         ALLOCATE(vmec_file_construct%jksupvmnsf(                              &
     &      vmec_file_construct%ns, vmec_file_construct%mnmax_nyq))

         CALL cdf_read(vmec_ncid, 'rmns', vmec_file_construct%rmnsf)
         CALL cdf_read(vmec_ncid, 'zmnc', vmec_file_construct%zmncf)

         CALL cdf_read(vmec_ncid, 'bsupumns',                                  &
     &                 vmec_file_construct%bsupumnsh)
         CALL cdf_read(vmec_ncid, 'bsupvmns',                                  &
     &                 vmec_file_construct%bsupvmnsh)

         CALL cdf_read(vmec_ncid, 'currumns',                                  &
     &                 vmec_file_construct%jksupumnsf)
         CALL cdf_read(vmec_ncid, 'currvmns',                                  &
     &                 vmec_file_construct%jksupvmnsf)
      END IF

      CALL cdf_close(vmec_ncid)

      CALL profiler_set_stop_time('vmec_file_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref vmec_file_class object.
!>
!>  Deallocates memory and uninitializes a @ref vmec_file_class object.
!>
!>  @param[inout] this A @ref vmec_file_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE vmec_file_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (vmec_file_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%xm)) THEN
         DEALLOCATE(this%xm)
         this%xm => null()
      END IF

      IF (ASSOCIATED(this%xn)) THEN
         DEALLOCATE(this%xn)
         this%xn => null()
      END IF

      IF (ASSOCIATED(this%xm_nyq)) THEN
         DEALLOCATE(this%xm_nyq)
         this%xm_nyq => null()
      END IF

      IF (ASSOCIATED(this%xn_nyq)) THEN
         DEALLOCATE(this%xn_nyq)
         this%xn_nyq => null()
      END IF

      IF (ASSOCIATED(this%rmncf)) THEN
         DEALLOCATE(this%rmncf)
         this%rmncf => null()
      END IF

      IF (ASSOCIATED(this%zmnsf)) THEN
         DEALLOCATE(this%zmnsf)
         this%zmnsf => null()
      END IF

      IF (ASSOCIATED(this%rmnsf)) THEN
         DEALLOCATE(this%rmnsf)
         this%rmnsf => null()
      END IF

      IF (ASSOCIATED(this%zmncf)) THEN
         DEALLOCATE(this%zmncf)
         this%zmncf => null()
      END IF

      IF (ASSOCIATED(this%jksupumncf)) THEN
         DEALLOCATE(this%jksupumncf)
         this%jksupumncf => null()
      END IF

      IF (ASSOCIATED(this%jksupvmncf)) THEN
         DEALLOCATE(this%jksupvmncf)
         this%jksupvmncf => null()
      END IF

      IF (ASSOCIATED(this%jksupumnsf)) THEN
         DEALLOCATE(this%jksupumnsf)
         this%jksupumnsf => null()
      END IF

      IF (ASSOCIATED(this%jksupvmnsf)) THEN
         DEALLOCATE(this%jksupvmnsf)
         this%jksupvmnsf => null()
      END IF

      END SUBROUTINE

      END MODULE
