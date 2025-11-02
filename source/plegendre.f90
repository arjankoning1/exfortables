function plegendre(l, x)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Calculation of Legendre polynomial
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
  use A0_exfortables_mod
!
! use A0_kinds_mod, only: & ! Definition of single and double precision variables
!              sgl   ! single precision kind
!
! *** Declaration of local data
!
  implicit none
  integer   :: i                    ! counter
  integer   :: l                    ! l-value  pl0, pl1 : help variable
  real(sgl) :: pl(0:200)            ! legendre polynomial
  real(sgl) :: pl0                  ! help variable
  real(sgl) :: pl1                  ! help variables
  real(sgl) :: plegendre            ! legendre polynomial
  real(sgl) :: x                    ! cosine of the angle
!
! ************************ Legendre polynomial *************************
!
! pl0, pl1    : help variables
!
  pl0 = 1.
  pl1 = x
  pl(0) = pl0
  pl(1) = pl1
  do i = 2, l
     pl(i) = (x * (2 * i - 1) * pl1 - (i - 1) * pl0) / i
     pl0 = pl1
     pl1 = pl(i)
  enddo
  plegendre = pl(l)
  return
end function plegendre
! Copyright A.J. Koning 2019
