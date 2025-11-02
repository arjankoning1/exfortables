subroutine inelastic(MT)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process inelastic scattering
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
!              A_xc5, &       ! mass number
!              Eexc, &        ! excitation energy
!              Eexc2, &       ! excitation energy
!              Elev, &        ! energy of discrete level
!              flagdisc, &    ! flag for discrete reaction
!              flagdiscang, & ! flag for discrete angular distribution
!              flaginel, &    ! flag for inelastic scattering
!              flaginelang, & ! flag for inelastic angular distribution
!              k0_xc5, &      ! incident particle
!              levpath, &     ! directory containing level files to be read
!              mainis, &      ! main isotope
!              MT_xc5, &      ! MT number
!              valstring, &   ! XC5 string with values
!              Z_xc5          ! charge number
!
! *** Declaration of local data
!
  implicit none
! character(len=4)  :: Zstring      !
  character(len=80) :: levelfile    !
  integer           :: A            !
  integer           :: Anat         !
  integer           :: i            ! counter
  integer           :: ia           !
  integer           :: k0           !
  integer           :: MT           !
  integer           :: nbr          !
  integer           :: nlev         !
  integer           :: nlin         !
  integer           :: Z            !
!
! ***************************** Initialization *************************
!
  Eexc = 0.
  Eexc2 = 0.
  Elev = 0.
  Z = Z_xc5
  A = A_xc5
  k0 = k0_xc5
!
! Attempt to assign MT51-90 numbers for discrete inelastic scattering
! using discrete level file
!
  if (flagdisc) then
    if (k0 == 1 .and. MT >= 51 .and. MT <=90) flaginel = .true.
    if (k0 == 2 .and. MT >= 600 .and. MT <= 649) flaginel = .true.
    if (k0 == 3 .and. MT >= 650 .and. MT <= 699) flaginel = .true.
    if (k0 == 4 .and. MT >= 700 .and. MT <= 749) flaginel = .true.
    if (k0 == 5 .and. MT >= 750 .and. MT <= 799) flaginel = .true.
    if (k0 == 6 .and. MT >= 800 .and. MT <= 849) flaginel = .true.
  endif
  if (flagdiscang) then
    if (k0 == 1 .and. MT >= 51 .and. MT <=90) flaginelang = .true.
    if (k0 == 2 .and. MT >= 600 .and. MT <= 649) flaginelang = .true.
    if (k0 == 3 .and. MT >= 650 .and. MT <= 699) flaginelang = .true.
    if (k0 == 4 .and. MT >= 700 .and. MT <= 749) flaginelang = .true.
    if (k0 == 5 .and. MT >= 750 .and. MT <= 799) flaginelang = .true.
    if (k0 == 6 .and. MT >= 800 .and. MT <= 849) flaginelang = .true.
  endif
  Anat = A
  if (Anat == 0) Anat = mainis(Z)
  if (MT >= 103 .and. MT <= 107) return
  read(valstring(7), '(e9.2)') Eexc
  read(valstring(8), '(e9.2)') Eexc2
  Eexc = abs(Eexc / 1.e6)
  Eexc2 = abs(Eexc2 / 1.e6)
  if (flaginel .or. flaginelang) then
!   Zstring = 'z000'
!   write(Zstring(2:4), '(i3.3)') Z
!   levelfile = trim(levpath) //Zstring
    levelfile = trim(levpath) //trim(nuc(Z))//'.lev'
    open (unit = 8, status = 'unknown', file = levelfile)
  100   read(8, '(4x, i4, i5)', err = 120, end = 120) ia, nlin
    if (Anat == ia) then
      read(8, '()')
  110     read(8, '(i4, f11.6, f6.1, i5, i3)', end = 120) nlev, Elev, Jlev, Plev, nbr
      if (Eexc - 0.05 < Elev .and. Eexc + 0.05 >= Elev) then
        MT_xc5 = MT - 1 + min(nlev, 40)
        goto 200
      endif
      do i = 1, nbr
        read(8, '()')
      enddo
      goto 110
    else
      do i = 1, nlin
        read(8, '()')
      enddo
      goto 100
    endif
  else
    if (Eexc /= 0.) goto 200
  endif
  120 MT_xc5 = MT + 39
  200 close (8)
  return
end subroutine inelastic
! Copyright A.J. Koning 2019
