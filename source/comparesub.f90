subroutine comparesub(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Compare experimental data with TALYS and libraries
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
!              Aslogpoint, &  ! log of asymmetry
!              Aspoint, &     ! asymmetry
!              chi2point, &   ! chi - square
!              comppoint, &   ! designator for comparison of point
!              dxsexp, &      ! cross section uncertainty
!              Eexp, &        ! incident energy
!              flagdexp, &    ! flag to use experimental uncertainty in F factor
!              flagerf, &     ! flag to use error function for Frms
!              flagexpo, &    ! flag to use exponential RMS instead of power of 10
!              Flogpoint, &   ! log of F value
!              Fpoint, &      ! F value
!              k0_xc5, &      ! incident particle
!              libexist, &    ! flag for existence of library
!              MT_xc5, &      ! MT number
!              Nlibs, &       ! number of data libraries for comparison
!              Npoints, &     ! number of points compared per library
!              Npoints_E  , & ! number of points per subentry
!              Rpoint, &      ! R value
!              xsexp, &       ! cross section
!              xsexplib, &    ! cross section
!              xseps, &       ! minimum cross section (mb) for TALYS + library comparison
!              xsnonel, &     ! non - elastic cross section
!              xsthpoint, &   ! theoretical cross section
!              Z_xc5          ! charge number
!
! *** Declaration of local data
!
  implicit none
  integer   :: iset                          !
  integer   :: ip                            !
  integer   :: k0                            !
  integer   :: lib                           !
  integer   :: MT                            !
  integer   :: Nlibpoint                     ! number of points per library
  integer   :: Z                             !
  real(sgl) :: Aslog                         !
  real(sgl) :: Aslogsum                      !
  real(sgl) :: Assum                         !
  real(sgl) :: CE
  real(sgl) :: chi2                          !
  real(sgl) :: chi2sum                       !
  real(sgl) :: dxsmb                         ! uncertainty of cross section
  real(sgl) :: EMeV                          ! incident energy
  real(sgl) :: Flog                          ! log of F value
  real(sgl) :: Flogsum                       !
  real(sgl) :: Fsum                          !
  real(sgl) :: R                             ! fraction of non-elastic cross section
  real(sgl) :: Ri                            !
  real(sgl) :: Rsum                          !
  real(sgl) :: p
  real(sgl) :: x
  real(sgl) :: xsmin 
  real(sgl) :: xsmax 
  real(sgl) :: xsmb                          ! cross section
  real(sgl) :: xsn                           ! non-elastic cross section
  real(sgl) :: xst                           !
  real(sgl) :: xsthsum                       ! help variable
!
! ***************************** Comparison *****************************
!
! readtalys  : subroutine to read data from TALYS output files
! readlib    : subroutine to read data from nuclear data libraries
!
  Z = Z_xc5
  MT = MT_xc5
  k0 = k0_xc5
!
! Loop over data points
!
  comppoint = 0
  chi2point = 0.
  Rpoint = 0.
  Flogpoint = 0.
  Aslogpoint = 0.
  xsthpoint = 0.
  Fpoint = 0.
  Aspoint = 0.
  Npoints = 0.
  do ip = 1, Npoints_E(iset)
    xsthsum = 0.
    chi2sum = 0.
    Rsum = 0.
    Fsum = 0.
    Assum = 0.
    Flogsum = 0.
    Aslogsum = 0.
    EMeV = Eexp(iset, ip)
    xsmb = xsexp(iset, ip)
    dxsmb = dxsexp(iset, ip)
    xsn = xsnonel(iset, ip)
    if (EMeV < allemin) cycle
    if (EMeV > allemax) cycle
    if (MT == 102 .and. (author1_E(iset) == 'Gamgam' .or. author1_E(iset) == 'Kadonis' .or. &
 &    author1_E(iset) == 'Profil') .and. EMeV <= resEnergy) cycle
    Nlibpoint = 0
    do lib = 1, Nlibs
      if ( .not. libexist(lib)) cycle
      xst = xsexplib(lib, iset, ip)
!
! Only do statistical factors for xs > xseps mb
!
      if (xst > xseps .and. xsmb > xseps) then
        Nlibpoint = Nlibpoint + 1
        Npoints(lib) = Npoints(lib) + 1
        comppoint(lib, ip) = 1
        xsthpoint(lib, ip) = xst
        if (dxsmb /= 0.) then
          chi2 = ((xst - xsmb) / dxsmb) **2
        else
          chi2 = 0.
        endif
        chi2point(lib, ip) = chi2
        if (xsn > 0..and.MT > 3) then
          R = xst / xsn
        else
          R = 0.
        endif
        Rpoint(lib, ip) = R
        CE = xst / xsmb
        if (flagdexp .and. dxsmb > 0.) then
          if (flagerf) then
            x = (xst - xsmb)/(dxsmb*sqrt(2.))
!
! Cumulative probability derived as follows:
!
!           cdf=0.5*(1.+erf(x))
!           p=2.*(cdf-0.5)
!
            if (xst >= xsmb) then
              p=erf(x)
            else
              p=-erf(x)
            endif
            Ri=1.+(CE-1.)*p
          else
            xsmin = xsmb - dxsmb
            xsmax = xsmb + dxsmb
            Ri = 1.
            if (xst < xsmin) Ri = xst / xsmin
            if (xst > xsmax) Ri = xst / xsmax
          endif
        else
          Ri = CE
        endif
        if (flagexpo) then
          Aslog = log(Ri)
          Flog = log(Ri) **2
        else
          Aslog = log10(Ri)
          Flog = log10(Ri) **2
        endif
        Aspoint(lib, ip) = Ri
        if (Ri > 1.) then
          Fpoint(lib, ip) = Ri
        else
          Fpoint(lib, ip) = 1. / Ri
        endif
        Aslogpoint(lib, ip) = Aslog
        Flogpoint(lib, ip) = Flog
        Fsum = Fsum + Fpoint(lib, ip)
        Assum = Assum + Aspoint(lib, ip)
        Flogsum = Flogsum + Flog
        Aslogsum = Aslogsum + Aslog
        xsthsum = xsthsum + xst
        chi2sum = chi2sum + chi2
        Rsum = Rsum + R
      endif
    enddo
!
! Values averaged over TALYS + libraries
!
    if (Nlibpoint > 0) then
      comppoint(0, ip) = 1
      xsthpoint(0, ip) = xsthsum / Nlibpoint
      Fpoint(0, ip) = Fsum / Nlibpoint
      Aspoint(0, ip) = Assum / Nlibpoint
      Flogpoint(0, ip) = Flogsum / Nlibpoint
      Aslogpoint(0, ip) = Aslogsum / Nlibpoint
      chi2point(0, ip) = chi2sum / Nlibpoint
      Rpoint(0, ip) = Rsum / Nlibpoint
    endif
  enddo
  do lib = 1, Nlibs
    Npoints(0) = max(Npoints(0), Npoints(lib))
  enddo
  return
end subroutine comparesub
! Copyright A.J. Koning 2019
