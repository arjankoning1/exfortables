subroutine totalstat
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Total statistics of all goodness-of-fit estimators
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
!              sgl              ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              Aexist_xc5, &    ! flag for existence of A
!              Aix, &           ! index for A
!              base, &          ! base number for RMS
!              FallE, &         ! total F per energy bin
!              FallElog, &      ! log of total F per energy bin
!              FAMT, &          ! average F for all MT numbers
!              fbin, &          ! boundaries for F bins
!              Ffinal, &        ! Final F value per bin
!              FMTE, &          ! F per MT number and energy bin
!              FMTElog, &       ! log of F per MT number and energy bin
!              FnucE, &         ! F per nucleus and energy bin
!              FnucElog, &      ! log of F per nucleus and energy bin
!              FnucMT, &        ! average log F value per nucleus and MT
!              FparE, &         ! F per particle and energy bin
!              FparElog, &      ! log of F per particle and energy bin
!              limit, &         ! limit for exponent in RMS
!              MTbin, &         ! number of F values in MT bin
!              MTbinav, &       ! average F value per bin
!              MTbincum, &      ! cumulative F value per MT number
!              MTbinsigma, &    ! average F deviation per bin
!              MTbintot, &      ! total number of F values in MT bin
!              MTexist, &       ! flag for existence of MT
!              MTexist_xc5, &   ! flag for existence of MT
!              mtinclude, &     ! flag to include MT number
!              MTix, &          ! index for MT
!              MTsumtot, &      ! total F value per particle per MT
!              Nlibs, &         ! number of data libraries for comparison
!              NpointsallE, &   ! number of points per energy bin
!              NpointsAMT, &    ! total number of points per MT number
!              NpointsMTE, &    ! number of points per MT number and ene
!              NpointsnucE, &   ! number of points per nucleus and energ
!              NpointsnucMT, &  ! number of points per nucleus and MT nu
!              NpointsparE, &   ! number of points per particle and ener
!              NsetsallMT, &    ! all MT sets
!              NsetsallMTtot, & ! all MT sets
!              NsetsAMT, &      ! all MT sets
!              NsetsMT, &       ! number of data sets per MT number
!              NsetsMTtot, &    ! number of sets per MT number
!              NsetsnucMT, &    ! number of data sets per nucleus and MT
!              numA, &          ! maximum number of masses
!              numbin, &        ! maximum number of bins
!              numEbin, &       ! maximum number of energy bins
!              numisom, &       ! maximum number of isomers
!              nummt, &         ! maximum number of MT numbers
!              numpar, &        ! maximum number of particles
!              numZ, &          ! maximum number of elements
!              parinclude, &    ! flag to include particle
!              pointref         ! reference for pointwise comparison
!
! *** Declaration of local data
!
  implicit none
  integer   :: A                         !
  integer   :: A0                        !
  integer   :: bin                       !
  integer   :: dmass                     !
  integer   :: ia                        !
  integer   :: imt                       !
  integer   :: is                        !
  integer   :: j                         ! counter
  integer   :: jj                        !
  integer   :: k0                        !
  integer   :: lib                       !
  integer   :: MT                        !
  integer   :: N                         !
  integer   :: nA                        !
  integer   :: nbin                      !
  integer   :: np                        !
  integer   :: sMT(0:numbin)             !
  integer   :: Z                         !
  real(sgl) :: binsigma                  !
  real(sgl) :: expo                      !
  real(sgl) :: F                         !
  real(sgl) :: Fav(0:numbin)             !
  real(sgl) :: fba                       !
  real(sgl) :: fbb                       !
  real(sgl) :: fca                       !
  real(sgl) :: fcb                       !
  real(sgl) :: Fcum(0:numbin)            !
  real(sgl) :: frac                      !
  real(sgl) :: Fsum                      !
  real(sgl) :: sF                        !
  real(sgl) :: sigma(2)                  !
!
! Put F values in bins
!
  write(*, *) "Calculating total statistics....."
  do k0 = 0, numpar
    if ( .not. parinclude(k0)) cycle
    do MT = 1, nummt
      if ( .not. mtinclude(MT)) cycle
      imt = MTix(MT)
      do is = - 1, numisom
        if ( .not. MTexist(k0, MT, is)) cycle
        do j = 0, numbin
          Fav(j) = 0.
          Fcum(j) = 0.
          sF = 0.
          sMT(j) = 0
          do jj = 0, j
            sF = sF + Ffinal(k0, imt, is, jj)
            sMT(j) = sMT(j) + MTbin(k0, imt, is, jj)
          enddo
          if (sMT(j) > 0) Fav(j) = sF / sMT(j)
        enddo
        MTbintot(k0, imt, is) = sMT(numbin)
        do j = 0, numbin
          if (sMT(numbin) > 0) Fcum(j) = sMT(j) / real(sMT(numbin))
          MTbinav(k0, imt, is, j) = Fav(j)
          MTbincum(k0, imt, is, j) = 100. * Fcum(j)
        enddo
        sigma(1) = 0.6827
        sigma(2) = 0.9545
        do j = 1, 2
          binsigma = 0.
          if (sigma(j) < Fcum(0)) then
            nbin = 0
          else
            call locate(Fcum, 0, numbin - 1, sigma(j), nbin)
          endif
          fba = fbin(nbin)
          fbb = fbin(nbin + 1)
          fca = fcum(nbin)
          fcb = fcum(nbin + 1)
          if (fcb /= fca) then
            frac = (sigma(j) - fca) / (fcb - fca)
            binsigma = fbb + frac * (fbb - fba)
          endif
          MTbinsigma(k0, imt, is, j) = binsigma
        enddo
        MTsumtot(k0) = MTsumtot(k0) + MTbintot(k0, imt, is)
        NsetsMTtot(k0) = NsetsMTtot(k0) + NsetsMT(pointref, k0, imt, is)
        NsetsallMTtot(k0) = NsetsallMTtot(k0) + NsetsallMT(k0, MT, is)
      enddo
    enddo
  enddo
!
! F values per energy bin
!
  do lib = 0, Nlibs
    do k0 = 0, numpar
      if ( .not. parinclude(k0)) cycle
      do Z = 1, numZ
        do A = 0, numA
          if ( .not. Aexist_xc5(k0, Z, A)) cycle
          ia = Aix(k0, Z, A)
          do bin = 1, numEbin
            F = FnucElog(lib, k0, Z, ia, bin)
            N = NpointsnucE(lib, k0, Z, ia, bin)
            if (N > 0) then
              expo = min(sqrt(F / N), limit)
              FnucE(lib, k0, Z, ia, bin) = base **expo
            endif
          enddo
        enddo
      enddo
      do MT = 1, nummt
        if ( .not. MTexist_xc5(MT)) cycle
        imt = MTix(MT)
        do is = - 1, numisom
          if ( .not. MTexist(k0, MT, is)) cycle
          do bin = 1, numEbin
            F = FMTElog(lib, k0, imt, is, bin)
            N = NpointsMTE(lib, k0, imt, is, bin)
            if (N > 0) then
              expo = min(sqrt(F / N), limit)
              FMTE(lib, k0, imt, is, bin) = base **expo
            endif
          enddo
        enddo
      enddo
      do bin = 1, numEbin
        F = FparElog(lib, k0, bin)
        N = NpointsparE(lib, k0, bin)
        if (N > 0) then
          expo = min(sqrt(F / N), limit)
          FparE(lib, k0, bin) = base **expo
        endif
      enddo
    enddo
    do bin = 1, numEbin
      F = FallElog(lib, bin)
      N = NpointsallE(lib, bin)
      if (N > 0) then
        expo = min(sqrt(F / N), limit)
        FallE(lib, bin) = base **expo
      endif
    enddo
  enddo
!
! F values per mass bin
!
  dmass = 3
  do lib = 0, Nlibs
    do k0 = 0, numpar
      if ( .not. parinclude(k0)) cycle
      do MT = 1, nummt
        if ( .not. MTexist_xc5(MT)) cycle
        imt = MTix(MT)
        do is = - 1, numisom
          do A0 = 0, numA
            Fsum = 0.
            nA = 0
            np = 0
            do A = max(A0 - dmass, 0), min(A0 + dmass, numA)
              do Z = 1, numZ
                if ( .not. Aexist_xc5(k0, Z, A)) cycle
                ia = Aix(k0, Z, A)
                N = NsetsnucMT(lib, k0, Z, ia, imt, is)
                if (N > 0) then
                  nA = nA + N
                  np = np + NpointsnucMT(lib, k0, Z, ia, imt, is)
                  Fsum = Fsum + N * FnucMT(lib, k0, Z, ia, imt, is)
                endif
              enddo
            enddo
            if (nA > 0) then
              FAMT(lib, k0, A0, imt, is) = Fsum / nA
              NsetsAMT(lib, k0, A0, imt, is) = nA
              NpointsAMT(lib, k0, A0, imt, is) = np
            endif
          enddo
        enddo
      enddo
    enddo
  enddo
  write(*, *) "Calculating total statistics done"
  return
end subroutine totalstat
! Copyright A.J. Koning 2019
