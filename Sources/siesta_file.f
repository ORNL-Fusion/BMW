!*******************************************************************************
!>  @file siesta_file.f
!>  @brief Contains module @ref siesta_file.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Defines the base class of the type @ref siesta_file_class. This contains the
!>  output of a siesta equilibrium.
!*******************************************************************************
      MODULE siesta_file
      USE stel_kinds, ONLY: rprec
      USE profiler

      IMPLICIT NONE

!*******************************************************************************
!  siesta file module parameters
!*******************************************************************************
!>  Version number.
      INTEGER, PARAMETER :: siesta_lasym_flag = 31

!*******************************************************************************
!  DERIVED-TYPE DECLARATIONS
!  1) siesta file base class
!
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  Base class representing a siesta output.
!-------------------------------------------------------------------------------
      TYPE siesta_file_class
!  State flags.
         INTEGER      :: flags

!  Number of radial points.
         INTEGER      :: nrad
!  Number of toroidal modes.
         INTEGER      :: ntor
!  Number of poloidal modes.
         INTEGER      :: mpol

!  Magnetic field scaling factor.
         REAL (rprec) :: b_factor

!  Toroidal modes
         REAL (rprec), DIMENSION(:), POINTER :: tor_modes => null()

!  R half parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: rmncf => null()
!  Z half parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: zmnsf => null()
!  R full parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: rmnsf => null()
!  Z full parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: zmncf => null()

!  J^s current density half parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupsmnsf => null()
!  J^u current density half parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupumncf => null()
!  J^v current density half parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupvmncf => null()
!  J^s current density full parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupsmncf => null()
!  J^u current density full parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupumnsf => null()
!  J^v current density full parity.
         REAL (rprec), DIMENSION(:,:,:), POINTER :: jksupvmnsf => null()
      CONTAINS
         FINAL :: siesta_file_destruct
      END TYPE

!-------------------------------------------------------------------------------
!>  Interface for the siesta_file_class constructor.
!-------------------------------------------------------------------------------
      INTERFACE siesta_file_class
         MODULE PROCEDURE siesta_file_construct
      END INTERFACE

      CONTAINS
!*******************************************************************************
!  CONSTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Construct a @ref siesta_file_class object.
!>
!>  Allocates memory and initializes a @ref siesta_file_class object with an
!>  siesta restart file.
!>
!>  @param[in] siesta_file_name File name for vacuum fields.
!>  @returns A pointer to a constructed @ref siesta_file_class object.
!-------------------------------------------------------------------------------
      FUNCTION siesta_file_construct(siesta_file_name)
      USE ezcdf

      IMPLICIT NONE

!  Declare Arguments
      CLASS (siesta_file_class), POINTER :: siesta_file_construct
      CHARACTER (len=*), INTENT(in)      :: siesta_file_name

!  local variables
      REAL (rprec)                       :: start_time
      INTEGER                            :: siesta_ncid
      INTEGER                            :: status

!  Start of executable code
      start_time = profiler_get_start_time()

      ALLOCATE(siesta_file_construct)

      CALL cdf_open(siesta_ncid, TRIM(siesta_file_name), 'r', status)

      CALL cdf_read(siesta_ncid, 'state_flags',                                &
     &              siesta_file_construct%flags)

      CALL cdf_read(siesta_ncid, 'nrad', siesta_file_construct%nrad)
      CALL cdf_read(siesta_ncid, 'ntor', siesta_file_construct%ntor)
      CALL cdf_read(siesta_ncid, 'mpol', siesta_file_construct%mpol)

      CALL cdf_read(siesta_ncid, 'b_factor',                                   &
     &              siesta_file_construct%b_factor)

      ALLOCATE(siesta_file_construct%tor_modes(                                &
     &   -siesta_file_construct%ntor:siesta_file_construct%ntor))

      ALLOCATE(siesta_file_construct%rmncf(                                    &
     &   0:siesta_file_construct%mpol,                                         &
     &   -siesta_file_construct%ntor:siesta_file_construct%ntor,               &
     &   siesta_file_construct%nrad))
      ALLOCATE(siesta_file_construct%zmnsf(                                    &
     &   0:siesta_file_construct%mpol,                                         &
     &   -siesta_file_construct%ntor:siesta_file_construct%ntor,               &
     &   siesta_file_construct%nrad))

      ALLOCATE(siesta_file_construct%jksupsmnsf(                               &
     &   0:siesta_file_construct%mpol,                                         &
     &   -siesta_file_construct%ntor:siesta_file_construct%ntor,               &
     &   siesta_file_construct%nrad))
      ALLOCATE(siesta_file_construct%jksupumncf(                               &
     &   0:siesta_file_construct%mpol,                                         &
     &   -siesta_file_construct%ntor:siesta_file_construct%ntor,               &
     &   siesta_file_construct%nrad))
      ALLOCATE(siesta_file_construct%jksupvmncf(                               &
     &   0:siesta_file_construct%mpol,                                         &
     &   -siesta_file_construct%ntor:siesta_file_construct%ntor,               &
     &   siesta_file_construct%nrad))

      CALL cdf_read(siesta_ncid, 'tor_modes',                                  &
     &              siesta_file_construct%tor_modes)

      CALL cdf_read(siesta_ncid, 'rmnc(m,n,r)',                                &
     &              siesta_file_construct%rmncf)
      CALL cdf_read(siesta_ncid, 'zmns(m,n,r)',                                &
     &              siesta_file_construct%zmnsf)

      CALL cdf_read(siesta_ncid, 'jksupsmnsf(m,n,r)',                          &
     &              siesta_file_construct%jksupsmnsf)
      CALL cdf_read(siesta_ncid, 'jksupumncf(m,n,r)',                          &
     &              siesta_file_construct%jksupumncf)
      CALL cdf_read(siesta_ncid, 'jksupvmncf(m,n,r)',                          &
     &              siesta_file_construct%jksupvmncf)

      IF (BTEST(siesta_file_construct%flags, siesta_lasym_flag)) THEN
         ALLOCATE(siesta_file_construct%rmnsf(                                 &
     &      0:siesta_file_construct%mpol,                                      &
     &      -siesta_file_construct%ntor:siesta_file_construct%ntor,            &
     &      siesta_file_construct%nrad))
         ALLOCATE(siesta_file_construct%zmncf(                                 &
     &      0:siesta_file_construct%mpol,                                      &
     &      -siesta_file_construct%ntor:siesta_file_construct%ntor,            &
     &      siesta_file_construct%nrad))

         ALLOCATE(siesta_file_construct%jksupsmncf(                            &
     &      0:siesta_file_construct%mpol,                                      &
     &      -siesta_file_construct%ntor:siesta_file_construct%ntor,            &
     &      siesta_file_construct%nrad))
         ALLOCATE(siesta_file_construct%jksupumnsf(                            &
     &      0:siesta_file_construct%mpol,                                      &
     &      -siesta_file_construct%ntor:siesta_file_construct%ntor,            &
     &      siesta_file_construct%nrad))
         ALLOCATE(siesta_file_construct%jksupvmnsf(                            &
     &      0:siesta_file_construct%mpol,                                      &
     &      -siesta_file_construct%ntor:siesta_file_construct%ntor,            &
     &      siesta_file_construct%nrad))

         CALL cdf_read(siesta_ncid, 'rmns(m,n,r)',                             &
     &                 siesta_file_construct%rmnsf)
         CALL cdf_read(siesta_ncid, 'zmnc(m,n,r)',                             &
     &                 siesta_file_construct%zmncf)

         CALL cdf_read(siesta_ncid, 'jksupsmncf(m,n,r)',                       &
     &                 siesta_file_construct%jksupsmncf)
         CALL cdf_read(siesta_ncid, 'jksupumnsf(m,n,r)',                       &
     &                 siesta_file_construct%jksupumnsf)
         CALL cdf_read(siesta_ncid, 'jksupvmnsf(m,n,r)',                       &
     &                 siesta_file_construct%jksupvmnsf)
      END IF

      CALL cdf_close(siesta_ncid)

      CALL profiler_set_stop_time('siesta_file_construct', start_time)

      END FUNCTION

!*******************************************************************************
!  DESTRUCTION SUBROUTINES
!*******************************************************************************
!-------------------------------------------------------------------------------
!>  @brief Deconstruct a @ref siesta_file_class object.
!>
!>  Deallocates memory and uninitializes a @ref siesta_file_class object.
!>
!>  @param[inout] this A @ref siesta_file_class instance.
!-------------------------------------------------------------------------------
      SUBROUTINE siesta_file_destruct(this)

      IMPLICIT NONE

!  Declare Arguments
      TYPE (siesta_file_class), INTENT(inout) :: this

!  Start of executable code
      IF (ASSOCIATED(this%tor_modes)) THEN
         DEALLOCATE(this%tor_modes)
         this%tor_modes => null()
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

      IF (ASSOCIATED(this%jksupsmncf)) THEN
         DEALLOCATE(this%jksupsmncf)
         this%jksupsmncf => null()
      END IF

      IF (ASSOCIATED(this%jksupsmnsf)) THEN
         DEALLOCATE(this%jksupsmnsf)
         this%jksupsmnsf => null()
      END IF

      IF (ASSOCIATED(this%jksupumncf)) THEN
         DEALLOCATE(this%jksupumncf)
         this%jksupumncf => null()
      END IF

      IF (ASSOCIATED(this%jksupumnsf)) THEN
         DEALLOCATE(this%jksupumnsf)
         this%jksupumnsf => null()
      END IF

      IF (ASSOCIATED(this%jksupvmncf)) THEN
         DEALLOCATE(this%jksupvmncf)
         this%jksupvmncf => null()
      END IF

      IF (ASSOCIATED(this%jksupvmnsf)) THEN
         DEALLOCATE(this%jksupvmnsf)
         this%jksupvmnsf => null()
      END IF

      END SUBROUTINE

      END MODULE
