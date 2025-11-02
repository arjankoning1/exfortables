subroutine writedata
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write numerical data
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
!              Afy, &          ! A of fission yield
!              author2, &      ! author name
!              dE, &           ! incident energy uncertainty
!              dxs, &          ! cross section uncertainty
!              dxsr, &         ! uncertainty of cross section divided by Rutherford
!              E, &            ! incident energy
!              flagddx, &      ! flag for DDX emission spectrum
!              flagdiscang, &  ! flag for discrete angular distribution
!              flagelang, &    ! flag for elastic angular distribution
!              flagfy, &       ! flag for fission yields
!              flagnubar, &    ! flag for nubar
!              flagratio, &    ! flag for cross section ratio
!              flagrespar, &   ! flag for resonance parameters
!              flagri, &       ! flag for resonance integral
!              flagspec, &     ! flag for emission spectrum
!              flagspecav, &   ! flag for spectrum average
!              flagxs, &       ! flag for cross section
!              iso_xc5, &      ! isomer
!              isochar_xc5, &  ! character for isomer
!              isom_xc5, &     ! number of isomer
!              Ify             ! isomer of fission yield
!              k0_xc5, &       ! incident particle
!              MT_xc5, &       ! MT number
!              Npoints_xc5, &  ! number of points per subentry
!              subentry_xc5, & ! XC5 subentry number
!              xs, &           ! cross section
!              xsfile, &       ! cross section file
!              xsr, &          ! cross section divided by Rutherford
!              year_xc5, &     ! year
!              Z_xc5, &        ! charge number
!              Zfy             ! Z of fission yield
!
! *** Declaration of local data
!
  implicit none
  character(len=1)  :: tis         !
  character(len=9)  :: subentry    !
  character(len=25) :: aut         !
  integer           :: A           !
  integer           :: is          !
  integer           :: k           ! counter
  integer           :: k0          !
  integer           :: kbase       !
  integer           :: kfile       !
  integer           :: MT          !
  integer           :: yy          !
  integer           :: Z           !
!
! *************************** Write dataset ****************************
!
! Normalize Levkovski data  by 0.82
!              
  if (subentry_xc5(1:5) == 'A0510') then
    if (flagxs) then
      do k = 1, Npoints_xc5
        xs(k) = xs(k) * 0.82
      enddo
    endif
  endif
!
! Cross sections
!
  k0 = k0_xc5
  MT = MT_xc5
  if (flagxs) then
    do k = 1, Npoints_xc5
      write(1, '(5es15.6)') E(k), dE(k), xs(k), dxs(k), Fnorm(k)
    enddo
  endif
!
! Special files for thermal and 30 keV
!
  if (flagxs .or. flagnubar .or. flagspecav .or. flagrespar .or. flagri) then
    if (flagxs .or. flagnubar) then
      kbase = 40
    else
      if (flagspecav) then
        kbase = 50
      else
        kbase = 60
      endif
    endif
    if (k0 == 1 .and. Npoints_xc5 == 1) then
      kfile = 0
      if (E(1) >= 2.5E-8 .and. E(1) <= 2.55E-8) then
        if (MT == 1) kfile = kbase + 1
        if (MT == 2) kfile = kbase + 2
        if (MT == 18) kfile = kbase + 3
        if (MT == 102) kfile = kbase + 4
        if (MT == 103) kfile = kbase + 5
        if (MT == 107) kfile = kbase + 6
        if (MT == 452) kfile = kbase + 7
        if (MT == 455) kfile = kbase + 8
        if (MT == 456) kfile = kbase + 9
      endif
      if (E(1) >= 2.4E-2 .and. E(1) <= 3.4E-2 .and. MT == 102) kfile = kbase + 10
      if (MT == 18) then
        if (flagri) kfile = kbase + 2
      endif
      if (MT == 102) then
        if (flagri) kfile = kbase + 1
        if (flagrespar) kfile = kbase + 3
      endif
      if (MT == 101) then
        if (flagrespar) kfile = kbase + 4
      endif
      if (kfile /= 0) then
        Z = Z_xc5
        A = A_xc5
        if (iso_xc5 > 0) then
          tis = isochar_xc5
        else
          tis = ' '
        endif
        is = isom_xc5
        subentry = subentry_xc5
        aut = author2
        yy = year_xc5
        write(kfile, '(4i4, 3es13.5, 1x, a25, i4, 1x, a9)') Z, A, iso_xc5, is, xs(1), dxs(1), E(1), aut, yy, subentry
      endif
    endif
  endif
!
! Other data
!
  if (flagrespar .or. flagri .or. flagspecav .or. flagratio .or. flagnubar) then
    do k = 1, Npoints_xc5
      write(1, '(5es15.6)') E(k), dE(k), xs(k), dxs(k), Fnorm(k)
    enddo
  endif
  if (flagfy) then
    do k = Eindex, Nend_new
      write(1, '(3(6x,i3,6x), 3es15.6)') Zfy(k), Afy(k), Ify(k), xs(k), dxs(k), Fnorm(k)
    enddo
  endif
  if (flagtke) then
    do k = 1, Npoints_xc5
      write(1, '(5es15.6)') E(k), dE(k), xs(k), dxs(k), Fnorm(k)
    enddo
  endif
  if (flagelang .or. flagdiscang) then
    do k = Eindex, Nend_new
      write(1, '(4es15.6)') angle(k), xs(k), dxs(k), Fnorm(k)
    enddo
  endif
  if (flagspec) then
    do k = Eindex, Nend_new
      write(1, '(5es15.6)') E(k), dE(k), xs(k), dxs(k), Fnorm(k)
    enddo
  endif
  if (flagddx) then
    do k = Eindex, Nend_new
      write(1, '(6es15.6)') E(k), dE(k), xs(k), dxs(k), angle(k), Fnorm(k)
    enddo
  endif
!
! For charged-particle elastic scattering, make extra file with cross sections divided by Rutherford value
!
  if (flagelang .and. k0 > 1) then
    open (unit = 33, status = 'unknown', file = trim(xsfile)//'.ruth')
    do k = Eindex, Nend_new
      write(33, '(4es15.6)') angle(k), xsr(k), dxsr(k), Fnorm(k)
    enddo
    close (33)
  endif
  return
end subroutine writedata
! Copyright A.J. Koning 2019
