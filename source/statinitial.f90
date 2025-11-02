subroutine statinitial
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialization of variables for statistics
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
!              Aexist_xc5, &          ! flag for existence of A
!              Afy, &                 ! A of fission yield
!              Aix, &                 ! index for A
!              angle, &               ! angle
!              Ares, &                ! residual A
!              author2, &             ! author name
!              authorcode, &          ! author
!              authordone, &          ! flag to check whether this entry is already covered
!              compdir, &             ! directory with statistics
!              cosang, &              ! cosine of angle
!              dE, &                  ! incident energy uncertainty
!              dxs, &                 ! cross section uncertainty
!              dxsr, &                ! uncertainty of cross section divided by Rutherford
!              E, &                   ! incident energy
!              elib, &                ! energy of library
!              Emax, &                ! maximum energy (MeV) of subentry
!              Emin, &                ! minimum energy (MeV) of subentry
!              Fall, &                ! average F value for all reactions
!              FallE, &               ! total F per energy bin
!              FallElog, &            ! log of total F per energy bin
!              Falllog, &             ! average log F value for all reactions
!              FAMT, &                ! average F for all MT numbers
!              fbin, &                ! boundaries for F bins
!              Fentry, &              ! average F value per entry
!              Fentrylog, &           ! average log F value per entry
!              Ffinal, &              ! Final F value per bin
!              flagMT, &              ! flag for MT cross section
!              flagres, &             ! flag for residual production cross section
!              FMT, &                 ! average F value per MT number
!              FMTE, &                ! F per MT number and energy bin
!              FMTElog, &             ! log of F per MT number and energy bin
!              FMTlog, &              ! average log F value per MT number
!              Fnuc, &                ! average F value per nucleus
!              FnucE, &               ! F per nucleus and energy bin
!              FnucElog, &            ! log of F per nucleus and energy bin
!              Fnuclog, &             ! average log F value per nucleus
!              FnucMT, &              ! average log F value per nucleus and MT
!              FnucMTlog, &           ! average F value per nucleus and MT num
!              Fpar, &                ! average F value per particle
!              FparE, &               ! F per particle and energy bin
!              FparElog, &            ! log of F per particle and energy bin
!              Fparlog, &             ! average log F value per particle
!              frame, &               ! reference fram
!              isom_xc5, &            ! number of isomer
!              iso_xc5, &             ! isomer of target
!              Ify, &                 ! isomer of fission yield
!              k0_xc5, &              ! incident particle
!              kres, &                ! type of outgoing particle
!              Liso, &                ! number of isomer
!              Lrat, &                ! isomer for ratio
!              MTbin, &               ! number of F values in MT bin
!              MTbinav, &             ! average F value per bin
!              MTbincum, &            ! cumulative F value per MT number
!              MTbinsigma, &          ! average F deviation per bin
!              MTbintot, &            ! total number of F values in MT bin
!              MTexist, &             ! flag for existence of MT
!              MTexist_xc5, &         ! flag for existence of MT
!              MTix, &                ! index for MT
!              MTrat, &               ! MT number for ratio
!              MTsum, &               ! number of reactions per MT number
!              MTsumtot, &            ! total F value per particle per MT
!              MTuncbin, &            ! uncertainty per bin
!              NAix, &                ! number of A's per k0, Z
!              Nlib, &                ! number of data libraries for comparison
!              NMTix, &               ! number of MT sections
!              NPgroup, &             ! number of points in group
!              Npointsall, &          ! total number of points
!              NpointsallE, &         ! number of points per energy bin
!              NpointsAMT, &          ! total number of points per MT number
!              Npointsentry, &        ! number of points per entry
!              NpointsMT, &           ! number of points per MT number
!              NpointsMTE, &          ! number of points per MT number and ene
!              Npointsnuc, &          ! number of points per nucleus
!              NpointsnucE, &         ! number of points per nucleus and energ
!              NpointsnucMT, &        ! number of points per nucleus and MT nu
!              Npointspar, &          ! number of points per particle
!              NpointsparE, &         ! number of points per particle and ener
!              Npointsset, &          ! number of points per subentry
!              Npointstot_tal, &      ! number of points for TALYS comparison
!              NQ, &                  ! number of items with quality information
!              Nsetsall, &            ! total number of data sets
!              NsetsallE, &           ! total number of data sets for all ener
!              NsetsallMT, &          ! all MT sets
!              NsetsallMTtot, &       ! all MT sets
!              NsetsAMT, &            ! all MT sets
!              Nsetsentry, &          ! number of data sets per entry
!              NsetsMT, &             ! number of data sets per MT number
!              NsetsMTE, &            ! number of data sets per MT number and
!              NsetsMTtot, &          ! number of sets per MT number
!              Nsetsnuc, &            ! number of data sets per nucleus
!              NsetsnucE, &           ! number of data sets per nuclide and en
!              NsetsnucMT, &          ! number of data sets per nucleus and MT
!              Nsetspar, &            ! number of data sets per particle
!              NsetsparE, &           ! number of data sets per particle and e
!              Nsub_tal, &            ! number of subentries for TALYS comparison
!              numbin, &              ! maximum number of bins
!              proj, &                ! projectile
!              Qaction, &             ! recommended action
!              Qaction0, &            ! recommended action
!              Qcomment, &            ! comments
!              Qdate, &               ! quality date
!              Qdate0, &              ! quality date
!              Qentry, &              ! entry with quality information
!              QMT, &                 ! quality per MT number
!              QMTall, &              ! total quality per MT number
!              Qscore, &              ! quality score
!              Qscore0, &             ! quality score
!              Qsub0, &               ! subentry with quality information
!              reac, &                ! reaction identifier
!              resiso, &              ! isomer of residual nuclide
!              talfile, &             ! name of TALYS output file
!              isochar_xc5, &         ! isomer of target
!              uncdir, &              ! directory with uncertainties
!              uncertainty, &         ! average uncertainty of subentry
!              xs, &                  ! cross section
!              xsexpav, &             ! average experimental cross section
!              xsr, &                 ! cross section divided by Rutherford
!              ZAP_xc5, &             ! projectile
!              ZAexist, &             ! logical for Z, A existence
!              ZAres, &               ! ZA of residual nucleus
!              ZAresexist, &          ! logical for Z, A, rp existence
!              Zfy, &                 ! Z of fission yield
!              Zres                   ! residual Z
!
! *** Declaration of local data
!
  implicit none
  character(len=80) :: cmd       ! command
  integer           :: j                !
!
! ********************** Initialization **********************
!
! readsub
!
  ZAexist = .false.
! ZAMTexist = .false.
  ZAresexist = .false.
  isochar_xc5 = ' '
  resiso = ' '
  ZAP_xc5 = 0
  proj = ' '
  k0_xc5 = -1
  frame = 'L'
  iso_xc5 = 0
  Liso = 99
  isom_xc5 = -1
  MTrat = 0
  Lrat = 99
  flagMT = .false.
  flagres = .false.
  ZAres = 0
  kres = 0
!
! readdata
!
  Emin = 0.
  Emax = 0.
  E = 0.
  dE = 0.
  xs = 0.
  dxs = 0.
  xsexpav = 0.
  Zres = 0
  Ares = 0
  Zfy = 0
  Afy = 0
  Ify = -1
  uncertainty = 0.
  xsr = 0.
  dxsr = 0.
  cosang = 0.
  angle = 0.
!
! authorsub
!
  authordone = .false.
  author2 = '                         '
  authorcode = '                             '
!
!
!
  NPgroup = 0
  Nlib = 0
  elib = 0.
! reacinitial
!
  reac = -1
!
! readquality
!
  Qsub0 = '         '
  Qscore0 = '  '
  Qdate0 = '          '
  Qaction0 = '                                                       '
  NQ = 0
!
! compare
!
  compdir = 'stat/comp/'
  uncdir = 'stat/unc/'
!
! comparesum
!
  Aexist_xc5 = .false.
  MTexist_xc5 = .false.
  NAix = 0
  Aix = 0
  MTix = 0
  NMTix = 0
  NpointsparE = 0
  NpointsMTE = 0
  NpointsallE = 0
  Nsetsnuc = 0
  Nsetspar = 0
  NsetsMT = 0
  NsetsnucMT = 0
  NsetsAMT = 0
  Nsetsentry = 0
  Nsetsall = 0
  NsetsallMT = 0
  NsetsallMTtot = 0
  NsetsMTtot = 0
  Npointsset = 0
  Npointspar = 0
  NpointsMT = 0
  Npointsnuc = 0
  Npointsall = 0
  Npointsentry = 0
  NpointsnucE = 0
  NpointsnucMT = 0
  NpointsAMT = 0
  NsetsparE = 0
  NsetsnucE = 0
  NsetsMTE = 0
  NsetsallE = 0
  FMT = 0.
  FMTlog = 0.
  Fnuc = 0.
  Fnuclog = 0.
  FnucMTlog = 0.
  FnucMT = 0.
  FAMT = 0.
  Fpar = 0.
  Fall = 0.
  Fparlog = 0.
  Falllog = 0.
  FMTElog = 0.
  FMTE = 0.
  FnucE = 0.
  FnucElog = 0.
  FparE = 0.
  FparElog = 0.
  Fentrylog = 0.
  Fentry = 0.
  FallE = 0.
  FallElog = 0.
!
! quality
!
  Qdate = '          '
  Qaction = '                                                        '
  Qscore = '  '
  Qcomment = '                                                       '
  Qentry = 0
  QMT = 0
  QMTall = 0
!
! totalstat
!
  MTexist = .false.
  MTsum = 0
  MTsumtot = 0
  MTbin = 0
  MTbintot = 0
  MTbinav = 0.
  MTbincum = 0.
  MTbinsigma = 0.
  Nsub_tal = 0
  Npointstot_tal = 0
  MTuncbin = 0
  MTuncsets = 0
  fbin = 0.
  do j = 0, 40
    fbin(j) = 1. + 0.05 * j
  enddo
  do j = 41, numbin
    fbin(j) = 2. + 10 **(0.30 * (j - 40))
  enddo
  fbin(numbin) = 1000.
  Ffinal = 0.
  cmd = 'cp -r ? FY stat/'
  call command(cmd)
  return
end subroutine statinitial
! Copyright A.J. Koning 2019
