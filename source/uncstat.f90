subroutine uncstat(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Output of uncertainties
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
!              author1_E, &  ! author
!              Emax_E, &         ! maximum energy (MeV) of subentry
!              Emin_E, &         ! minimum energy (MeV) of subentry
!              Eunc, &         ! average energy uncertainty of subentry
!              Euncabs, &      ! average absolute energy uncertainty of subentry
!              isochar, &      ! character for isomer
!              isom_xc5, &     ! number of isomer
!              k0_xc5, &       ! incident particle
!              MT_xc5, &       ! MT number
!              MTexist, &      ! flag for existence of MT
!              MTuncbin, &     ! uncertainty per bin
!              Npoints_E, &  ! number of points per subentry
!              numbin, &       ! maximum number of bins
!              partype, &      ! symbol of particle
!              reaction_E, & ! XC5 reaction
!              subentry_E, & ! XC5 subentry number
!              uncertainty, &  ! average uncertainty of subentry
!              xsexpav, &      ! average experimental cross section
!              uncbin, &       ! uncertainty bin
!              year_E        ! year
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist             !
  character(len=9)   :: subdir             !
  character(len=4)   :: MTstr              !
  character(len=15)  :: MTfile             !
  character(len=250) :: headstring         !
  character(len=250) :: string             !
  character(len=250) :: uncfile            !
  integer            :: isor               !
  integer            :: k                  ! counter
  integer            :: iset               ! counter
  integer            :: k0                 !
  integer            :: MT                 !
  integer            :: nbin                      !
!
! ************** Output of individual F values *************************
!
  string = ' '
  subdir = 'stat/unc/'
  MT = MT_xc5
  k0 = k0_xc5
  isor = isom_xc5
  MTstr = '    '
  write(MTstr(1:3), '(i3.3)') MT
  if (isor >= 0) MTstr(4:4) = isochar(isor)
  MTfile = partype(k0)//'-MT'//MTstr
  MTfile = trim(MTfile)//'.unc'
!
! Uncertainty values per Z,A, reaction
!
  headstring = ' SUBENT      AUTHOR      YEAR     N         '// &
    '    Reaction           Av. Exp   Rel. Err.  E-min     E-max'// '   Abs. Err. Rel. Err.'
  string = ' '
  do k = 1, 2
    if (k == 1) uncfile = subdir//'MT/'//MTfile
    if (k == 2) uncfile = subdir//'total/'//partype(k0)//'-unc'
    inquire (file = uncfile, exist = lexist)
    open (unit = 7, status = 'unknown', file = uncfile, position = 'append')
    if (.not. lexist) write(7, '(a)') trim(headstring)
    write(7, '(a9, a14, 2i6, 1x, a30, 1p, 6g10.3)') subentry_E(iset), author1_E(iset), year_E(iset), Npoints_E(iset), &
 &    reaction_E(iset), xsexpav(iset), uncertainty(iset), Emin_E(iset), Emax_E(iset), Euncabs(iset), Eunc(iset)
    close (7)
  enddo
!
! Put uncertainties in bins
!
  if (uncertainty(iset) > uncbin(numbin)) then
    nbin = numbin
  else
    call locate(uncbin, 0, numbin, uncertainty(iset), nbin)
  endif
  if (nbin > 0) then
    MTuncbin(k0, MT, isor, nbin) = MTuncbin(k0, MT, isor, nbin) + 1
    MTuncbin(k0, 0, - 1, nbin) = MTuncbin(k0, 0, - 1, nbin) + 1
    MTuncsets(k0, MT, isor) = MTuncsets(k0, MT, isor) + 1
    MTexist(k0, MT, isor) = .true.
  endif
  return
end subroutine uncstat
! Copyright A.J. Koning 2019
