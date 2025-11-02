subroutine subentryinitial
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialization of variables per subentry
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
!              A_xc5, &               ! mass number
!              Afy, &                 ! A of fission yield
!              angle, &               ! angle
!              Ares, &                ! residual A
!              author2, &             ! author name
!              authorcode, &          ! author
!              authordone, &          ! flag to check whether this entry is already covered
!              cosang, &              ! cosine of angle
!              date_xc5, &            ! date
!              dE, &                  ! incident energy uncertainty
!              dxs, &                 ! cross section uncertainty
!              dxsr, &                ! uncertainty of cross section divided by Rutherford
!              E, &                   ! incident energy
!              elib, &                ! energy of library
!              Emax, &                ! maximum energy (MeV) of subentry
!              Emin, &                ! minimum energy (MeV) of subentry
!              flagcomp, &            ! flag for comparison with libraries and TALYS
!              flagconv, &            ! flag for conversion to new database
!              flagddx, &             ! flag for DDX emission spectrum
!              flagdisc, &            ! flag for discrete reaction
!              flagdiscang, &         ! flag for discrete angular distribution
!              flagelang, &           ! flag for elastic angular distribution
!              flagfy, &              ! flag for fission yields
!              flagtke, &             ! flag for total kinetic energy
!              flaginel, &            ! flag for inelastic scattering
!              flaginelang, &         ! flag for inelastic angular distribution
!              flagMT, &              ! flag for MT cross section
!              flagnubar, &           ! flag for nubar
!              flagratio, &           ! flag for cross section ratio
!              flagres, &             ! flag for residual production cross section
!              flagrespar, &          ! flag for resonance parameters
!              flagri, &              ! flag for resonance integral
!              flagspec, &            ! flag for emission spectrum
!              flagspecav, &          ! flag for spectrum average
!              flagxs, &              ! flag for cross section
!              frame, &               ! reference fram
!              isom_xc5, &            ! number of isomer
!              iso_xc5, &             ! isomer of target
!              Ify, &                 ! isomer of fission yield
!              k0_xc5, &              ! incident particle
!              kres, &                ! type of outgoing particle
!              Liso, &                ! number of isomer
!              Lrat, &                ! isomer for ratio
!              MF_xc5, &              ! MF number
!              MT_xc5, &              ! MT number
!              MTrat, &               ! MT number for ratio
!              Nlib, &                ! number of data libraries for comparison
!              NPgroup, &             ! number of points in group
!              Npoints_xc5, &         ! number of points per subentry
!              Npoints_new, &         ! number of points per new data set
!              proj, &                ! projectile
!              reac, &                ! reaction identifier
!              reaction_xc5, &        ! XC5 reaction
!              resiso, &              ! isomer of residual nuclide
!              string_xc5, &          ! date according to NDRC file
!              subentry_xc5, &        ! XC5 subentry number
!              talfile, &             ! name of TALYS output file
!              xs, &                  ! cross section
!              xsr, &                 ! cross section divided by Rutherford
!              Z_xc5, &               ! charge number
!              ZAP_xc5, &             ! projectile
!              ZAres, &               ! ZA of residual nucleus
!              Zfy, &                 ! Z of fission yield
!              Zres                   ! residual Z
!
! *** Declaration of local data
!
  implicit none
!
! ********************** Initialization **********************
!
! readsub
!
  subentry_xc5 = ' '
  date_xc5 = 0
  reaction_xc5 = ' '
  Z_xc5 = 0
  A_xc5 = 0
  iso_xc5 = 0
  MF_xc5 = 0
  MT_xc5 = 0
  Npoints_xc5 = 0
  Npoints_new = 0
  Nend_new = 0
  string_xc5 = ' '
!
! reactionsub
!
  resiso = ' '
  ZAP_xc5 = 0
  proj = ' '
  k0_xc5 = -1
  frame = 'L'
  Liso = 99
  isom_xc5 = -1
  flagratio = .false.
  MTrat = 0
  Lrat = 99
  flagconv = .false.
  flagri = .false.
  flagspecav = .false.
  flagrespar = .false.
  flagfy = .false.
  flagtke = .false.
  flagnubar = .false.
  flagMT = .false.
  flagdisc = .false.
  flagres = .false.
  ZAres = 0
  kres = 0
  flagelang = .false.
  flagdiscang = .false.
  flagddx = .false.
  flagspec = .false.
!
! inelastic
!
  flaginel = .false.
  flaginelang = .false.
!
! readdata
!
  flagxs = .false.
  flagcomp = .false.
  Emin = 0.
  Emax = 0.
  E = 0.
  dE = 0.
  xs = 0.
  dxs = 0.
  Fnorm = 1.
  Zres = 0
  Ares = 0
  Zfy = 0
  Afy = 0
  Ify = -1
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
end subroutine subentryinitial
! Copyright A.J. Koning 2019
