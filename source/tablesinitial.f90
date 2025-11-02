subroutine tablesinitial
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialization
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A1_exfortables_mod, only: & ! All global variables
!              flagremove, &  ! flag for removing existing database
!              flagx4         ! flag to use original EXFOR information
!
! *** Declaration of local data
!
  implicit none
!
! **************** Initialization of constants and arrays **************
!
! arrayinitial: subroutine for initialization of arrays
! reacinitial : subroutine for initialization of nuclear reaction info
! remove      : subroutine to remove previous results
!
  call arrayinitial
  call reacinitial
  if (flagremove) call remove
  return
end subroutine tablesinitial
! Copyright A.J. Koning 2019
