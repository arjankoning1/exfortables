subroutine comparesum(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process statistics of all goodness-of-fit estimators
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A0_kinds_mod, only: & ! Definition of single and double precision variables
!              sgl               ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              A_xc5, &          ! mass number
!              Aexist_xc5, &     ! flag for existence of A
!              Aix, &            ! index for A
!              Aslogpoint, &     ! log of asymmetry
!              Aspoint, &        ! asymmetry
!              Asset, &          ! average As value per subentry
!              base, &           ! base number for RMS
!              chi2point, &      ! chi - square
!              chi2set, &        ! average Chi - 2 per subentry
!              comppoint, &      ! designator for comparison of point
!              Eexp, &           ! incident energy
!              E1mb, &           ! energy at 1 mb threshold
!              ebin, &           ! lower bound of energy bin
!              ecbin, &          ! energy bin
!              Fall, &           ! average F value for all reactions
!              FallElog, &       ! log of total F per energy bin
!              Falllog, &        ! average log F value for all reactions
!              fbin, &           ! boundaries for F bins
!              Fentry, &         ! average F value per entry
!              Fentrylog, &      ! average log F value per entry
!              Ffinal, &         ! Final F value per bin
!              Flogpoint, &      ! log of F value
!              Fmax, &           ! maximum F value
!              FMT, &            ! average F value per MT number
!              FMTElog, &        ! log of F per MT number and energy bin
!              FMTlog, &         ! average log F value per MT number
!              Fnuc, &           ! average F value per nucleus
!              FnucElog, &       ! log of F per nucleus and energy bin
!              Fnuclog, &        ! average log F value per nucleus
!              FnucMT, &         ! average log F value per nucleus and MT
!              FnucMTlog, &      ! average F value per nucleus and MT num
!              Fpar, &           ! average F value per particle
!              FparElog, &       ! log of F per particle and energy bin
!              Fparlog, &        ! average log F value per particle
!              Fpoint, &         ! F value
!              Fset, &           ! average F value per subentry
!              isom_xc5, &       ! number of isomer
!              k0_xc5, &         ! incident particle
!              limit, &          ! limit for exponent in RMS
!              MTbin, &          ! number of F values in MT bin
!              MTexist, &        ! flag for existence of MT
!              MT_xc5, &         ! MT number
!              MTexist_xc5, &    ! flag for existence of MT
!              MTix, &           ! index for MT
!              MTsum, &          ! number of reactions per MT number
!              NAix, &           ! number of A's per k0, Z
!              Nlibs, &          ! number of data libraries for comparison
!              NMTix, &          ! number of MT sections
!              Npoints, &        ! number of points compared per library
!              Npoints_E, &    ! number of points per subentry
!              Npointsall, &     ! total number of points
!              NpointsallE, &    ! number of points per energy bin
!              Npointsentry, &   ! number of points per entry
!              NpointsMT, &      ! number of points per MT number
!              NpointsMTE, &     ! number of points per MT number and ene
!              Npointsnuc, &     ! number of points per nucleus
!              NpointsnucE, &    ! number of points per nucleus and energ
!              NpointsnucMT, &   ! number of points per nucleus and MT nu
!              Npointspar, &     ! number of points per particle
!              NpointsparE, &    ! number of points per particle and ener
!              Npointsset, &     ! number of points per subentry
!              Npointstot_tal, & ! number of points for TALYS comparison
!              Nsetsall, &       ! total number of data sets
!              NsetsallE, &      ! total number of data sets for all ener
!              Nsetsentry, &     ! number of data sets per entry
!              NsetsMT, &        ! number of data sets per MT number
!              NsetsMTE, &       ! number of data sets per MT number and
!              Nsetsnuc, &       ! number of data sets per nucleus
!              NsetsnucE, &      ! number of data sets per nuclide and en
!              NsetsnucMT, &     ! number of data sets per nucleus and MT
!              Nsetspar, &       ! number of data sets per particle
!              NsetsparE, &      ! number of data sets per particle and e
!              Nsub_tal, &       ! number of subentries for TALYS comparison
!              numaix, &         ! maximum index for mass number
!              numEbin, &        ! maximum number of energy bins
!              numbin, &         ! maximum number of bins
!              nummtix, &        ! maximum index for MT
!              pointref, &       ! reference for pointwise comparison
!              pset, &           ! average p value per subentry
!              pvalue, &         ! p value
!              Rpoint, &         ! R value
!              Rset, &           ! average R per subentry
!              xsthpoint, &      ! theoretical cross section
!              xsthset, &        ! average theoretical cross section per
!              Z_xc5             ! charge number
!
! *** Declaration of local data
!
  implicit none
  integer   :: A                               !
  integer   :: bin                             !
  integer   :: binc                            !
  integer   :: bincexist(0:numEbin)            !
  integer   :: binexist(0:numEbin)             !
  integer   :: ia                              !
  integer   :: ientry                          !
  integer   :: imt                             !
  integer   :: ip                              !
  integer   :: is                              !
  integer   :: iset                            !
  integer   :: k0                              !
  integer   :: i
  integer   :: lib                             !
  integer   :: MT                              !
  integer   :: N                               !
  integer   :: nbin                            !
  integer   :: NP                              !
  integer   :: Npo                             !
  integer   :: NPplus                          !
  integer   :: Z                               !
  real(sgl) :: Aslog                           !
  real(sgl) :: Aslogsum                        !
  real(sgl) :: chi2                            !
  real(sgl) :: chi2sum                         !
  real(sgl) :: Ei                              !
  real(sgl) :: Ec                              !
  real(sgl) :: expo                            !
  real(sgl) :: F                               !
  real(sgl) :: Flog                            ! log of F value
  real(sgl) :: Flogsum                         !
  real(sgl) :: R                               ! fraction of non-elastic cross section
  real(sgl) :: Rsum                            !
  real(sgl) :: psum                            !
  real(sgl) :: xst                             !
  real(sgl) :: xsthsum                         ! help variable
  real(sgl) :: sweight
!
! ***************************** Average values *************************
!
  Z = Z_xc5
  A = A_xc5
  k0 = k0_xc5
  MT = MT_xc5
  is = isom_xc5
  ientry = 1
  Nsub_tal = Nsub_tal + 1
  if (Aexist_xc5(k0, Z, A)) then
    ia = Aix(k0, Z, A)
  else
    Aexist_xc5(k0, Z, A) = .true.
    NAix(k0, Z) = NAix(k0, Z) + 1
    ia = NAix(k0, Z)
    Aix(k0, Z, A) = ia
    if (ia > numaix) then
      write(*, '(" EXFORTABLES-error: ia > numaix")')
      stop
    endif
  endif
  if (MTexist_xc5(MT)) then
    imt = MTix(MT)
  else
    MTexist_xc5(MT) = .true.
    NMTix = NMTix + 1
    imt = NMTix
    MTix(MT) = imt
    if (imt > nummtix) then
      write(*, '(" EXFORTABLES-error: imt > nummtix")')
      stop
    endif
  endif
  sweight = 1.
  if (flagoutliers) then
    do i = 1, NW
      if (trim(subentry_E(iset)) == trim(subweight(i))) then
        sweight = weight0(i)
        exit
      endif
    enddo
  endif
  Npointsset = 0
  Fset = 0
  Rset = 0
  Asset = 0
  pset = 0
  chi2set = 0
  xsthset = 0
  do lib = 0, Nlibs
    Npo = Npoints(lib)
    if (Npo == 0) cycle
    xsthsum = 0.
    Flogsum = 0.
    chi2sum = 0.
    Rsum = 0.
    Aslogsum = 0.
    NP = 0
    NPplus = 0
    binexist = 0
    bincexist = 0
    do ip = 1, Npoints_E(iset)
      if (comppoint(lib, ip) == 0) cycle
!
! Values per point
!
      xst = xsthpoint(lib, ip)
      Flog = Flogpoint(lib, ip)
      chi2 = chi2point(lib, ip)
      R = Rpoint(lib, ip)
      Aslog = Aslogpoint(lib, ip)
!
! Summation per set
!
      xsthsum = xsthsum + xst
      Flogsum = Flogsum + Flog
      chi2sum = chi2sum + chi2
      Rsum = Rsum + R
      Aslogsum = Aslogsum + Aslog
      Fentrylog(lib, ientry) = Fentrylog(lib, ientry) + Flog
!
! locate     : subroutine to find value in ordered table
!
! For overall statistics, outliers can be excluded
!
      if (Fpoint(lib, ip) > Fmax) cycle
      Ei = Eexp(iset, ip)
      if (sweight > 0.) then
!
! Summation per particle, Z, A, MT number
!
        FnucMTlog(lib, k0, Z, ia, imt, is) = FnucMTlog(lib, k0, Z, ia, imt, is) + Flog
!
! Summation per particle, Z, A
!
        Fnuclog(lib, k0, Z, A) = Fnuclog(lib, k0, Z, A) + Flog
!
! Summation per particle, Z, A, energy bin
!
        call locate(ebin, 0, numEbin, Ei, bin)
        binexist(bin) = 1
        FnucElog(lib, k0, Z, ia, bin) = FnucElog(lib, k0, Z, ia, bin) + Flog
        NpointsnucE(lib, k0, Z, ia, bin) = NpointsnucE(lib, k0, Z, ia, bin) + 1
!
! Summation per MT number
!
        FMTlog(lib, k0, imt, is) = FMTlog(lib, k0, imt, is) + Flog
!
! Summation per MT number, per energy
!
        binc = 1
        Ec = Ei - E1mb
        if (Ec <= 0.) then
          if (E1mb > 0.) binc = 11
        else
          call locate(ecbin, 0, numEbin, Ec, binc)
        endif
        bincexist(binc) = 1
        FMTElog(lib, k0, imt, is, binc) = FMTElog(lib, k0, imt, is, binc) + Flog
        NpointsMTE(lib, k0, imt, is, binc) = NpointsMTE(lib, k0, imt, is, binc) + 1
!
! Summation per particle
!
        Fparlog(lib, k0) = Fparlog(lib, k0) + Flog
!
! Summation per particle, per energy
!
        FparElog(lib, k0, bin) = FparElog(lib, k0, bin) + Flog
        NpointsparE(lib, k0, bin) = NpointsparE(lib, k0, bin) + 1
!
! Summation for all
!
        Falllog(lib) = Falllog(lib) + Flog
!
! Summation for all, per energy
!
        FallElog(lib, bin) = FallElog(lib, bin) + Flog
        NpointsallE(lib, bin) = NpointsallE(lib, bin) + 1
      endif
!
! Asymmetry
!
      if (Aspoint(lib, ip) >= 1.) NPplus = NPplus + 1
      NP = NP + 1
    enddo
!
! Count number of points
!
    if (sweight > 0.) then
      Npointsset(lib) = Npointsset(lib) + Npo
      Npointspar(lib, k0) = Npointspar(lib, k0) + NP
      NpointsMT(lib, k0, imt, is) = NpointsMT(lib, k0, imt, is) + NP
      Npointsnuc(lib, k0, Z, A) = Npointsnuc(lib, k0, Z, A) + NP
      Npointsentry(lib, ientry) = Npointsentry(lib, ientry) + NPo
      Npointsall(lib) = Npointsall(lib) + NP
      NpointsnucMT(lib, k0, Z, ia, imt, is) = NpointsnucMT(lib, k0, Z, ia, imt, is) + NP
    endif
!
! Average values per dataset (subentry)
!
    xsthset(lib) = xsthsum / NPo
    expo = min(sqrt(Flogsum / NPo), limit)
    Fset(lib) = base **expo
    expo = min(Aslogsum / NPo, limit)
    Asset(lib) = base **expo
    if (sweight > 0.) then
      F = FnucMTlog(lib, k0, Z, ia, imt, is)
      N = NpointsnucMT(lib, k0, Z, ia, imt, is)
      if (N > 0) then
        expo = min(sqrt(F / N), limit)
        FnucMT(lib, k0, Z, ia, imt, is) = base **expo
      endif
      F = Fnuclog(lib, k0, Z, A)
      N = Npointsnuc(lib, k0, Z, A)
      if (N > 0) then
        expo = min(sqrt(F / N), limit)
        Fnuc(lib, k0, Z, A) = base **expo
      endif
      F = FMTlog(lib, k0, imt, is)
      N = NpointsMT(lib, k0, imt, is)
      if (N > 0) then
        expo = min(sqrt(F / N), limit)
        FMT(lib, k0, imt, is) = base **expo
      endif
      F = Fparlog(lib, k0)
      N = Npointspar(lib, k0)
      if (N > 0) then
        expo = min(sqrt(F / N), limit)
        Fpar(lib, k0) = base **expo
      endif
      F = Fentrylog(lib, ientry)
      N = Npointsentry(lib, ientry)
      if (N > 0) then
        expo = min(sqrt(F / N), limit)
        Fentry(lib, ientry) = base **expo
      endif
    endif
    F = Falllog(lib)
    N = Npointsall(lib)
    if (N > 0) then
      expo = min(sqrt(F / N), limit)
      Fall(lib) = base **expo
    endif
    chi2set(lib) = chi2sum / NPo
    Rset(lib) = Rsum / NPo
!
! Number of sets per type
!
    if (sweight > 0.) then
      Nsetsentry(lib, ientry) = Nsetsentry(lib, ientry) + 1
      if (NP > 0) then
        NsetsMT(lib, k0, imt, is) = NsetsMT(lib, k0, imt, is) + 1
        Nsetsnuc(lib, k0, Z, A) = Nsetsnuc(lib, k0, Z, A) + 1
        NsetsnucMT(lib, k0, Z, ia, imt, is) = NsetsnucMT(lib, k0, Z, ia, imt, is) + 1
        Nsetspar(lib, k0) = Nsetspar(lib, k0) + 1
        Nsetsall(lib) = Nsetsall(lib) + 1
        do bin = 0, numEbin
          if (binexist(bin) == 1) then
            NsetsnucE(lib, k0, Z, ia, bin) = NsetsnucE(lib, k0, Z, ia, bin) + 1
            NsetsparE(lib, k0, bin) = NsetsparE(lib, k0, bin) + 1
            NsetsallE(lib, bin) = NsetsallE(lib, bin) + 1
          endif
          if (bincexist(bin) == 1) NsetsMTE(lib, k0, imt, is, bin) = NsetsMTE(lib, k0, imt, is, bin) + 1
        enddo
      endif
    endif
  enddo
  if (sweight > 0.) Npointstot_tal = Npointstot_tal + Npoints(0)
!
! Average p-value
!
  pset = 0.
  if (Npoints_E(iset) > 0) then
    psum = 0.
    do ip = 1, Npoints_E(iset)
      psum = psum + pvalue(iset, ip)
    enddo
    pset = psum / Npoints_E(iset)
  endif
!
! Put F values in bins
!
  imt = MTix(MT)
  lib = pointref
  if (Fset(lib) > fbin(numbin)) then
    nbin = numbin
  else
    call locate(fbin, 0, numbin, Fset(lib), nbin)
  endif
  if (nbin >= 0) then
    if (sweight > 0.) then
      MTsum(k0, imt, is) = MTsum(k0, imt, is) + 1
      MTbin(k0, imt, is, nbin) = MTbin(k0, imt, is, nbin) + 1
      MTbin(k0, 0, - 1, nbin) = MTbin(k0, 0, - 1, nbin) + 1
      Ffinal(k0, imt, is, nbin) = Ffinal(k0, imt, is, nbin) + Fset(lib)
      Ffinal(k0, 0, - 1, nbin) = Ffinal(k0, 0, - 1, nbin) + Fset(lib)
    endif
    MTexist(k0, MT, is) = .true.
  endif
  return
end subroutine comparesum
! Copyright A.J. Koning 2019
