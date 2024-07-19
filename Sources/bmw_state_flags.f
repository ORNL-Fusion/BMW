!*******************************************************************************
!>  @file bmw_state_flags.f
!>  @brief Contains module @ref bmw_state_flags.
!
!  Note separating the Doxygen comment block here so detailed decription is
!  found in the Module not the file.
!
!>  Contains parameters defining the bit positions for flags that mark different
!>  options.
!*******************************************************************************
      MODULE bmw_state_flags

      IMPLICIT NONE

!*******************************************************************************
!  module parameters
!*******************************************************************************
!>  Clear screen output.
      CHARACTER (len=*), PARAMETER ::                                          &
     &   clear_screen = achar(27)//"[2K"//achar(13)
!>  Progress indicator.
      CHARACTER (len=3), DIMENSION(0:3), PARAMETER ::                          &
     &   progress = (/ ' | ',  ' / ', ' - ', ' \ '/)

!>  Clear all flags.
      INTEGER, PARAMETER :: bmw_state_flags_off = 0

!>  Bit position for force override of errors flag.
      INTEGER, PARAMETER :: bmw_state_flags_force = 0
!>  Bit position for the use curl ju response flag.
      INTEGER, PARAMETER :: bmw_state_flags_ju = 1
!>  Bit position for the use curl jv response flag.
      INTEGER, PARAMETER :: bmw_state_flags_jv = 2
!>  Bit position for the use siesta instead of vmec.
      INTEGER, PARAMETER :: bmw_state_flags_siesta = 3
!>  Bit position for mgrid specified number of phi planes.
      INTEGER, PARAMETER :: bmw_state_flags_mgrid = 4
!>  Bit position for vmec vacuum correction.
      INTEGER, PARAMETER :: bmw_state_flags_correct = 5

      END MODULE
