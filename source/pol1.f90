subroutine pol1(x1, x2, y1, y2, x, y)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Interpolation of first order
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
  use A0_exfortables_mod
!
! use A0_kinds_mod, only: & ! Definition of single and double precision variables
!              sgl            ! single precision kind
!
! *** Declaration of local data
!
  real(sgl) :: fac            !
  real(sgl) :: x              ! cosine of the angle
  real(sgl) :: x1             !
  real(sgl) :: x2             !
  real(sgl) :: y              !
  real(sgl) :: y1             !
  real(sgl) :: y2             !
!
! ***************************** Interpolation **************************
!
  fac = (x-x1)/(x2-x1)
  y = y1 + fac * (y2 - y1)
  return
end subroutine pol1
! Copyright A.J. Koning 2019
