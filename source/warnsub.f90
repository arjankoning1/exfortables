subroutine warnsub
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Give warning for suspicious data set
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
!              A_xc5, &        ! mass number
!              angle, &        ! angle
!              author1_xc5, &  ! author
!              cosang, &       ! cosine of angle
!              date_xc5, &     ! date
!              dE, &           ! incident energy uncertainty
!              dxs, &          ! cross section uncertainty
!              E, &            ! incident energy
!              entry_xc5, &    ! entry
!              flagddx, &      ! flag for DDX emission spectrum
!              flagdiscang, &  ! flag for discrete angular distribution
!              flagelang, &    ! flag for elastic angular distribution
!              flagspec, &     ! flag for emission spectrum
!              flagxs, &       ! flag for cross section
!              k0_xc5, &       ! incident particle
!              mainis, &       ! main isotope
!              MT_xc5, &       ! MT number
!              Npoints_xc5, &  ! number of points per subentry
!              reaction_xc5, & ! XC5 reaction
!              subentry_xc5, & ! XC5 subentry number
!              xs, &           ! cross section
!              year, &         ! year
!              year_xc5, &     ! year
!              Z_xc5           ! charge number
!
! *** Declaration of local data
!
  implicit none
  logical :: flagwarn               ! flag for warning
  integer :: A                      ! mass number
  integer :: compday                ! day of compilation
  integer :: compmonth              ! month of compilation
  integer :: compyear               ! year of compilation
  integer :: k                      ! counter
  integer :: k0                     ! incindet particle
  integer :: MT                     ! MT number
  integer :: Np                     ! number of points
  integer :: Z                      ! charge number
!
! ************************* Read dataset *******************************
!
  Np = Npoints_xc5
  k0 = k0_xc5
  Z = Z_xc5
  A = A_xc5
  MT = MT_xc5
  flagwarn = .false.
!
! General warnings
!
  if (year_xc5 < 1934 .or. year_xc5 > year) then
    write(17, '(" Year out of range for Entry: ", a9, " Year: ", i4)') entry_xc5, year_xc5
    flagwarn = .true.
  endif
  compyear = date_xc5 / 10000
  compmonth = mod(date_xc5, 10000) / 100
  compday = mod(date_xc5, 100)
  if (compyear < 1965 .or. compyear > year .or. compmonth < 1 .or. compmonth > 12 .or. compday < 0 .or. compday > 31) then
      write(17, '(" Compilation date wrong for Subentry: ", a9, " Date: ", i8)') subentry_xc5, date_xc5
    flagwarn = .true.
  endif
  if ((A < mainis(max(Z, 1)) - 10 .or. A > mainis(max(Z, 1)) + 10) .and. A /= 0) then
      write(17, '(" A maybe out of range for Subentry: ", a9, " Z: ", i3, " A: ", i3)') subentry_xc5, Z, A
    flagwarn = .true.
  endif
  do k = 1, Np
    if (dE(k) < 0.) then
      write(17, '("Negative energy uncertainty ", 1p, e12.5)') dE(k)
      flagwarn = .true.
    endif
!   if (E(k) > 1..and.xs(k) < 0.) then
!      write(17, '("Negative cross section, E = ", 1p, e12.5, " xs = ", e12.5)') E(k), xs(k)
!     flagwarn = .true.
!   endif
    if (dxs(k) < 0.) then
      write(17, '("Negative cross section uncertainty ", 1p, e12.5)') dxs(k)
      flagwarn = .true.
    endif
  enddo
!
! Specific warnings
!
  if (flagxs) then
    do k = 1, Np
      if (E(k) < 0.) then
        write(17, '("Negative energy ", 1p, e12.5)') E(k)
        flagwarn = .true.
      endif
      if (E(k) > 1. .and. xs(k) > 4000. .and. Z > 3 .and. MT > 2 .and. k0 <= 6) then
        write(17, '("xs > 4 barn for E > 1 MeV: E = ", 1p, e12.5, " MeV xs = ", e12.5, " mb")') E(k), xs(k)
        flagwarn = .true.
      endif
    enddo
  endif
  if (flagelang .or. flagdiscang .or. flagspec .or. flagddx) then
    do k = 1, Np
      if (cosang(k) <  - 1. .or. cosang(k) > 1.) then
        write(17, '("Possible angle problem: cosine < 1 or > 1", " cosang = ", 1p, e12.5)') cosang(k)
        flagwarn = .true.
      endif
      if (k0 <= 1 .and. xs(k) > 1.e6) then
        write(17, '("xs > 1.e6 mb/sr for Angle = ", f7.3, " Cross section = ", 1p, e12.5)') angle(k), xs(k)
        flagwarn = .true.
      endif
    enddo
  endif
!
! Write subentry
!
  if (flagwarn) write(17, '("# Above error(s) for Subentry: ", a9, " Author: ", a25, " Year: ", i4, " Reaction: ", a40)') &
 &    subentry_xc5, author1_xc5, year_xc5, reaction_xc5
  return
end subroutine warnsub
! Copyright A.J. Koning 2019
