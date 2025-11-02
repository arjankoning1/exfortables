subroutine eview(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Make file for ECISVIEW
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
!              author1_E, & ! author
!              author2, &     ! author name
!              Einc, &        ! incident energy
!              flagelang, &   ! flag for elastic angular distribution
!              flagMT, &      ! flag for MT cross section
!              flagxs, &      ! flag for cross section
!              k0_xc5, &      ! incident particle
!              mainis, &      ! main isotope
!              MT_xc5, &      ! MT number
!              path, &        ! path of nuclide
!              year_E, &      ! year
!              Z_xc5          ! charge number
!
! *** Declaration of local data
!
  implicit none
  logical           :: eviewsub     ! flag for ECISVIEW
  logical           :: lexist       !
  character(len=29) :: code         !
  character(len=50) :: eviewpath    ! path for ECISVIEW
  character(len=76) :: eviewfile    ! file for ECISVIEW
  character(len=80) :: cmd          !
  integer           :: A            !
  integer           :: iset         !
  integer           :: k0           !
  integer           :: MT           !
  integer           :: Z            !
!
! ************************* Create directory for ECISVIEW **************
!
  k0 = k0_xc5
  Z = Z_xc5
  A = A_xc5
  MT = MT_xc5
  eviewsub = .false.
  if (flagelang) eviewsub = .true.
  if (flagMT .and. ((k0 == 1 .and. MT == 1) .or. (k0 > 1 .and. k0 <= 6 .and. MT == 3))) eviewsub = .true.
  if ( .not. eviewsub) return
  eviewpath = '                                                 '
  eviewpath = trim(path)//'eview/'
  inquire (file = eviewpath, exist = lexist)
  if ( .not. lexist) then
    cmd = 'mkdir -p '//trim(eviewpath)
    call command(cmd)
  endif
  code = author2
  if (flagelang) then
    eviewfile = 'elang000.000.dat'
    write(eviewfile(6:12), '(f7.3)') Einc
    write(eviewfile(6:8), '(i3.3)') int(Einc)
  else
    eviewfile = trim(code)//'.tot'
  endif
  open (unit = 12, status = 'unknown', file = trim(eviewpath) //eviewfile)
  write(12, '("#Z = ", i3)') Z
  if (A == 0) then
    write(12, '("#A = ", i3)') mainis(Z)
  else
    write(12, '("#A = ", i3)') A
  endif
  if (k0 == 1) write(12, '("#particle = neutron")')
  if (k0 == 2) write(12, '("#particle = proton")')
  if (k0 == 3) write(12, '("#particle = deuteron")')
  if (k0 == 4) write(12, '("#particle = triton")')
  if (k0 == 5) write(12, '("#particle = helium-3")')
  if (k0 == 6) write(12, '("#particle = alpha")')
  write(12, '("#label = EXP")')
  write(12, '("#id = ", a25, i4)') author1_E(iset), year_E(iset)
  if (flagelang) then
    write(12, '("#energy = ", f6.2)') Einc
    write(12, '("#type = angdis")')
  endif
  if (flagxs) then
    if (k0 == 1) then
      write(12, '("#type = total")')
    else
      write(12, '("#type = totalreact")')
    endif
  endif
  return
end subroutine eview
! Copyright A.J. Koning 2019
