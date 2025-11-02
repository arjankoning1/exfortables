 subroutine reacstat(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Output of (sorted) F values
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
!              Aix, &          ! index for A
!              Asset, &        ! average As value per subentry
!              author1_E, &  ! author
!              chi2set, &      ! average Chi - 2 per subentry
!              compdir, &      ! directory with statistics
!              Emax_E, &         ! maximum energy (MeV) of subentry
!              Emin_E, &         ! minimum energy (MeV) of subentry
!              Ffile, &        ! file with F values
!              flaglib, &      ! flag for library comparison
!              Fset, &         ! average F value per subentry
!              iso_xc5, &      ! isomer of character
!              isochar, &      ! character for isomer
!              isom_xc5, &     ! number of isomer
!              k0_xc5, &       ! incident particle
!              libstring, &    ! string with library names
!              MT_xc5, &       ! MT number
!              MTix, &         ! index for MT
!              Nlibs, &        ! number of data libraries for comparison
!              Npoints, &      ! number of points compared per library
!              partype, &      ! symbol of particle
!              pset, &         ! average p value per subentry
!              Qscore, &       ! quality score
!              reaction_E, & ! XC5 reaction
!              subentry_E, & ! XC5 subentry number
!              uncertainty, &  ! average uncertainty of subentry
!              xsexpav, &      ! average experimental cross section
!              xsthset, &      ! average theoretical cross section per
!              year_E, &     ! year
!              Z_xc5           ! charge number
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist           !
  character(len=4)   :: MTstr            !
  character(len=13)  :: MTfile           !
  character(len=13)  :: isostring        !
  character(len=84)  :: setstring        !
  character(len=266) :: headstring       !
  character(len=266) :: reacfile         !
  character(len=266) :: string           !
  integer            :: A                !
  integer            :: ia               !
  integer            :: iset             !
  integer            :: imt              !
  integer            :: isor             !
  integer            :: k                ! counter
  integer            :: k0               !
  integer            :: lib              !
  integer            :: MT               !
  integer            :: tiso             !
  integer            :: Z                !
!
! ************** Output of individual F values *************************
!
  k0 = k0_xc5
  Z = Z_xc5
  A = A_xc5
  tiso = iso_xc5
  isor = isom_xc5
  ia = Aix(k0, Z, A)
  MT = MT_xc5
  MTstr = '    '
  write(MTstr(1:3), '(i3.3)') MT
  if (isor >= 0) MTstr(4:4) = isochar(isor)
  imt = MTix(MT)
  isostring = '             '
  if (isor == 0) isostring = ' ground state'
  if (isor == 1) isostring = ' m isomer    '
  if (isor == 2) isostring = ' n isomer    '
!
! F values per Z,A, reaction
!
  headstring = '# Z   A   T   M  SUBENT      AUTHOR     YEAR'// '     N            Reaction               F       '// &
 &  '   A      chi2    p-value Quality World     Exp     Dexp(%)    E-min     E-max '
  if (flaglib) headstring = trim(headstring)//'   '//libstring
  MTfile = partype(k0) // '-MT' // trim(MTstr) // '.F'
  string = ' '
  do k = 1, 3
    if (k == 1) reacfile = Ffile
    if (k == 2) reacfile = trim(compdir) //'MT/'//MTfile
    if (k == 3) reacfile = trim(compdir) //'total/'//partype(k0)//'-allreac'
    inquire (file = reacfile, exist = lexist)
    open (unit = 7, status = 'unknown', file = reacfile, position = 'append')
    if (.not. lexist) write(7, '(a)') trim(headstring)
!
! Fill inserted string
!
    write(string, '(2i4, 2(i3, 1x), a9, a14, 2i6, 1x, a30, 1p, 4g10.3, " ", &
 &    a2, 5g10.3)') Z, A, tiso, isom_xc5, subentry_E(iset), author1_E(iset), &
 &    year_E(iset), Npoints(0), reaction_E(iset), Fset(0), Asset(0), chi2set(0), pset(iset), Qscore, &
      xsthset(0), xsexpav(iset), uncertainty(iset), Emin_E(iset), Emax_E(iset)
    if (flaglib) then
      write(setstring, '(1p, 8g10.3)') (Fset(lib), lib = 1, Nlibs)
      string = string(1:175) //setstring
    endif
    write(7, '(a)') trim(string)
  enddo
  return
end subroutine reacstat
! Copyright A.J. Koning 2019
