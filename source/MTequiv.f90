subroutine MTequiv
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Look for equivalnce of MT number and residual product
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
!              Ares, &         ! residual A
!              A_xc5, &       ! mass number
!              flagMT, &      ! flag for MT cross section
!              flagMTequiv, & ! flag for equivalence of MT number and residual product
!              flagres, &     ! flag for residual production cross section
!              k0_xc5, &      ! incident particle
!              MT_xc5, &      ! MT number
!              ZAres, &       ! ZA of residual nucleus
!              parA, &    ! mass number of particle
!              parZ, &    ! charge number of particle
!              Zres, &         ! residual Z
!              Z_xc5          ! charge number
!
! *** Declaration of local data
!
  implicit none
  integer          :: k0          !
  integer          :: MT          !
  integer          :: ZCN        !
  integer          :: ACN        !
  integer          :: ZACN        !
!
! ***** Find equivalent residual product for MT number *****************
!
  k0 = k0_xc5
  ZCN = Z_xc5 + parZ(k0)
  ACN = A_xc5  + parA(k0)
  ZACN = 1000 * ZCN + ACN
  if (flagMT) then
    MT = MT_xc5
    ZAres = 0
    if (MT == 4) ZAres = ZACN - 1
    if (MT == 16) ZAres = ZACN - 2
    if (MT == 17) ZAres = ZACN - 3
    if (MT == 37) ZAres = ZACN - 4
    if (MT == 102) ZAres = ZACN
    if (MT == 103) ZAres = ZACN - 1001
    if (MT == 111) ZAres = ZACN - 2002
!
! **** Also add the ones which are approximately equivalent
!
    if (A_xc5 > 16 ) then
      if (MT == 22) ZAres = ZACN - 2005
      if (MT == 23) ZAres = ZACN - 6013
      if (MT == 24) ZAres = ZACN - 2006
      if (MT == 25) ZAres = ZACN - 2007
      if (MT == 28) ZAres = ZACN - 1002
      if (MT == 41) ZAres = ZACN - 1003
      if (MT == 42) ZAres = ZACN - 1004
      if (MT == 44) ZAres = ZACN - 2003
      if (MT == 107) ZAres = ZACN - 2004
      if (MT == 108) ZAres = ZACN - 4008
      if (MT == 109) ZAres = ZACN - 6012
      if (MT == 112) ZAres = ZACN - 3005
    endif
    if (ZAres /= 0) then
      flagMTequiv = .true.
      flagMT = .false.
      flagres = .true.
      Zres = ZAres / 1000
      Ares = ZAres - Zres * 1000
      return
    endif
  endif
!
! ***** Find equivalent MT number for residual product *****************
!
  if (flagres) then
    MT = 0
    if (ZAres == ZACN - 1) MT = 4
    if (ZAres == ZACN - 2) MT = 16
    if (ZAres == ZACN - 3) MT = 17
    if (ZAres == ZACN - 2005) MT = 22
    if (ZAres == ZACN - 1002) MT = 28
    if (ZAres == ZACN - 4) MT = 37
    if (ZAres == ZACN) MT = 102
    if (ZAres == ZACN - 1001) MT = 103
    if (ZAres == ZACN - 2002) MT = 111
    if (MT /= 0) then
      flagMTequiv = .true.
      flagMT = .true.
      flagres = .false.
      MT_xc5 = MT
      Zres = ZAres / 1000
      Ares = ZAres - Zres * 1000
    endif
  endif
  return
end subroutine MTequiv
! Copyright A.J. Koning 2019
