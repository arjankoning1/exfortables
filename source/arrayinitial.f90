subroutine arrayinitial
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialization of arrays
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
!              Aix, &                 ! index for A
!              ebin, &                ! lower bound of energy bin
!              elib, &                ! energy of library
!              entry_sub, &           ! entry of subentry
!              Ffinal, &              ! Final F value per bin
!              libinclude, &          ! flag to include library
!              libstring, &           ! string with library names
!              MTexist, &             ! flag for existence of MT
!              MTexist_xc5, &         ! flag for existence of MT
!              MTix, &                ! index for MT
!              NAix, &                ! number of A's per k0, Z
!              Nentry_xc5, &          ! number of entries according to NRDC file
!              Nentry_xc5_nrdc, &     ! number of entries according to NRDC file
!              Nlib, &                ! number of data libraries for comparison
!              NMTix, &               ! number of MT sections
!              Nobs_xc5, &            ! number of obsolete cases
!              NPgroup, &             ! number of points in group
!              Npointstot_new, &      ! total number of points in new database
!              Npointstot_tal, &      ! number of points for TALYS comparison
!              Npointstot_xc5, &      ! total number of points
!              Npointstot_xc5_nrdc, & ! number of total points according to NDRC file
!              Nsub_new, &            ! counter for new subentries
!              Nsub_tal, &            ! number of subentries for TALYS comparison
!              Nsub_x4_nrdc, &        ! number of subentries according to NRDC file
!              Nsub_xc5, &            ! counter for XC5 subentries
!              Nsub_xc5_nrdc, &       ! number of subentries according to NRDC file
!              reac, &                ! reaction identifier
!              talfile, &             ! name of TALYS output file
!              uncbin, &              ! uncertainty bin
!              ZAexist, &             !
!              ZAresexist             !
!
! *** Declaration of local data
!
  implicit none
!
! ********************** Initialization of arrays **********************
!
! reacinitial
!
  talfile = ' '
  reac = -1
  libstring = '                                                      '
  libinclude = .false.
  ebin = 0.
  uncbin = 0.
  entry_sub = ' '
!
! readsub
!
  Nentry_xc5 = 0
  NPgroup = 0
  Nlib = 0
  elib = 0.
  Nsub_xc5 = 0
  Nsub_new = 0
  Npointstot_xc5 = 0
!
! reactionsub
!
  Nobs_xc5 = 0
!
! filesub
!
  ZAexist = .false.
! ZAMTexist = .false.
  ZAresexist = .false.
!
! readdata
!
  Npointstot_new = 0
  Nentry_xc5_nrdc = 0
  Nsub_xc5_nrdc = 0
  Nsub_x4_nrdc = 0
  Npointstot_xc5_nrdc = 0
!
! comparesum
!
  Aexist_xc5 = .false.
  MTexist_xc5 = .false.
  MTexist_xc5(0) = .true.
  NAix = 0
  Aix = 0
  MTix = 0
  NMTix = 0
!
! totalstat
!
  MTexist = .false.
  Nsub_tal = 0
  Npointstot_tal = 0
  Ffinal = 0.
  return
end subroutine arrayinitial
! Copyright A.J. Koning 2019
