subroutine tablesinput
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Input
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Declaration of local data
!
! readinput   : subroutine to read user input
! input       : subroutine to read input for keywords
! checkkeyword: subroutine to check for errors in keywords
! checkvalue  : subroutine to check for errors in values
!
  call readinput
  call input
  call checkkeyword
  call checkvalue
  return
end subroutine tablesinput
! Copyright A.J. Koning 2019
