subroutine xsinterpol(Nsets)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Interpolate library and TALYS cross section on experimental energy grid
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
!              sgl            ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              allemax, &     ! maximum energy (MeV) for TALYS or library comparison
!              allemin, &     ! minimum energy (MeV) for TALYS or library comparison
!              dxsexp, &         ! cross section uncertainty
!              dxsexpint, &          ! cross section uncertainty
!              dxsexplib, &          ! cross section uncertainty
!              dxslib, &       ! cross section uncertainty
!              Eexp, &           ! incident energy
!              elib, &        ! energy of library
!              Enon, &        ! energy for non - elastic cross section
!              etal, &        ! energy of TALYS
!              flagtalys, &   ! flag for TALYS comparison
!              libexist, &    ! flag for existence of library
!              MT_xc5, &      ! MT number
!              Nexp, &        ! number of experiments
!              Nlib, &        ! number of data libraries for comparison
!              Nlibs, &       ! number of data libraries for comparison
!              Nnon, &        ! number of non - elastic energies
!              Npoints_E  , & ! number of points per subentry
!              Ntal, &        ! number of TALYS points
!              numlib, &    ! maximum number of data libraries
!              numpoint, &    ! maximum number of data points in subentry
!              talysemax, &   ! maximum energy (MeV) for TALYS comparison
!              talysemin, &   ! minimum energy (MeV) for TALYS comparison
!              xsexp, &          ! cross section
!              xsexplib, &          ! cross section
!              xsexpint, &          ! cross section
!              dxsexpint, &          ! cross section uncertainty
!              xslib, &       ! cross section of library
!              xsnonel, &       ! non - elastic cross section
!              xsnon, &       ! non - elastic cross section
!              xstal          ! cross section of TALYS
!
! *** Declaration of local data
!
  implicit none
  integer   :: iset                          !
  integer   :: Nsets                          !
  integer   :: ip                            !
  integer   :: iset2                         !
  integer   :: ip2                           !
  integer   :: lib                           !
  integer   :: nen                           ! energy counter
  integer   :: MT                            !
  integer   :: NP                            !
  integer   :: NT                            !
  integer   :: N                             !
  real(sgl) :: dxsmb                         ! uncertainty of cross section
  real(sgl) :: dxsmb2                        ! uncertainty of cross section
  real(sgl) :: e1                            !
  real(sgl) :: e2                            !
  real(sgl) :: egrid(0:numpoint)             ! energy ofgrid
  real(sgl) :: EMeV                          ! incident energy
  real(sgl) :: EMeV2                         ! incident energy
  real(sgl) :: xsn                           !
  real(sgl) :: loge1                         !
  real(sgl) :: loge2                         !
  real(sgl) :: logx1                         !
  real(sgl) :: logx2                         !
  real(sgl) :: logxst                        !
  real(sgl) :: frac                          !
  real(sgl) :: x1                            !
  real(sgl) :: x1non                         !
  real(sgl) :: x2                            !
  real(sgl) :: x2non                         !
  real(sgl) :: xs0                           !
  real(sgl) :: xsgrid(0:numpoint)            ! cross section of grid
  real(sgl) :: xsmb                          ! cross section
  real(sgl) :: xsmb2                         ! cross section
  real(sgl) :: xst                           !
!
! ***************************** Comparison *****************************
!
  libexist(0) = .true.
  if (flagtalys) libexist(1) = .true.
  xsexplib = 0.
  dxsexplib = 0.
  xsexpint = 0.
  dxsexpint = 0.
  xsnonel = 0.
  Nexp = 0
  MT = MT_xc5
!
! Loop over data points
!
  do iset = 1, Nsets
    do ip = 1, Npoints_E(iset)
      EMeV = Eexp(iset, ip)
      xsmb = xsexp(iset, ip)
      dxsmb = dxsexp(iset, ip)
      if (EMeV < allemin) cycle
      if (EMeV > allemax) cycle
!
! Nonelastic cross section
!
! locate       : subroutine to find value in ordered table
! pol1         : subroutine for interpolation of first order
!
      xsn = xsnon(0)
      if (EMeV >= Enon(Nnon)) then
        xsn = xsnon(Nnon)
      else
        call locate(Enon, 0, Nnon, EMeV, NT)
        e1 = Enon(NT)
        e2 = Enon(NT + 1)
        x1non = xsnon(NT)
        x2non = xsnon(NT + 1)
        if (MT == 102 .and. e1 /= 0. .and. e2 /= 0. .and. x1non /= 0. .and. x2non /= 0. .and. e1 /= e2) then
          loge1 = log(e1)
          loge2 = log(e2)
          logx1 = log(x1non)
          logx2 = log(x2non)
          call pol1(loge1, loge2, logx1, logx2, log(EMeV), logxst)
          xsn = exp(logxst)
        else
          if (e1 /= e2) call pol1(e1, e2, x1non, x2non, EMeV, xsn)
        endif
      endif
      xsnonel(iset, ip) = xsn
!
! TALYS (lib=1) + library comparison
!
      do lib = 1, Nlibs
        if ( .not. libexist(lib)) cycle
        xst = 0.
        egrid = 0.
        if (lib == 1) then
          if (EMeV < talysemin) cycle
          if (EMeV > talysemax) cycle
          NP = Ntal
          do nen = 0, NP
            egrid(nen) = etal(nen)
            xsgrid(nen) = xstal(nen)
          enddo
        else
          NP = Nlib(lib)
          do nen = 0, NP
            egrid(nen) = elib(lib, nen)
            xsgrid(nen) = xslib(lib, nen)
          enddo
        endif
!
! Interpolate values for comparison
!
        if ((EMeV < egrid(1) .or. EMeV > egrid(NP))) cycle
        call locate(egrid, 0, NP, EMeV, NT)
        if (NT < 0) cycle
        e1 = egrid(NT)
        e2 = egrid(min(NT + 1, NP))
        x1 = xsgrid(NT)
        if (NT < NP) then
          x2 = xsgrid(NT + 1)
          if (MT == 102 .and. e1 /= 0. .and. e2 /= 0. .and. x1 /= 0. .and. x2 /= 0. .and. e1 /= e2) then
            loge1 = log(e1)
            loge2 = log(e2)
            logx1 = log(x1)
            logx2 = log(x2)
            call pol1(loge1, loge2, logx1, logx2, log(EMeV), logxst)
            loge1 = log(e1)
            xs0 = exp(logxst)
          else
            if (e1 /= e2) then
              call pol1(e1, e2, x1, x2, EMeV, xs0)
            else
              cycle
            endif
          endif
        else
          xs0 = x1
        endif
        xsexplib(lib, iset, ip)  = xs0
        dxsexplib(lib, iset, ip)  = dxslib(lib, NT)
      enddo
!
! Interpolate values from other measurements
!
      N = 0
      do iset2 = 1, Nsets
        if (N == numlib) exit
        do ip2 = 1, Npoints_E(iset2)
          EMeV2 = Eexp(iset2, ip2)
          if (EMeV2 > 0.) then
            frac= EMeV / EMeV2
            if (frac > 0.98 .and. frac < 1.02) then
              xsmb2 = xsexp(iset2, ip2)
              dxsmb2 = dxsexp(iset2, ip2)
              N = N + 1
              xsexpint(N, iset, ip)  = xsmb2
              dxsexpint(N, iset, ip)  = dxsmb2
              exit
            endif
          endif
        enddo
      enddo
      Nexp(iset, ip) = N
    enddo
  enddo
  return
end subroutine xsinterpol
