subroutine readdata
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read XC5 data
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
  use A1_error_handling_mod
!
! use A0_kinds_mod, only: & ! Definition of single and double precision variables
!              sgl               ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              angle, &          ! angle
!              angmin, &         ! minimum angle
!              angmax, &         ! maximum angle
!              Afy, &            ! A of fission yield
!              cosang, &         ! cosine of angle
!              dE, &             ! incident energy uncertainty
!              dxs, &            ! cross section uncertainty
!              dxsr, &           ! uncertainty of cross section divided by Rutherford
!              E, &              ! incident energy
!              e2c, &            ! square of elementary charge in MeV.fm
!              Einc, &           ! incident energy
!              elib, &           ! energy of library
!              Emax, &           ! maximum energy (MeV) of subentry
!              Emin, &           ! minimum energy (MeV) of subentry
!              flagcomp, &       ! flag for comparison with libraries and TALYS
!              flagddx, &        ! flag for DDX emission spectrum
!              flagdiscang, &    ! flag for discrete angular distribution
!              flagelang, &      ! flag for elastic angular distribution
!              flagfy, &         ! flag for fission yields
!              flaggroup, &      ! flag to group resonance data
!              flagMT, &         ! flag for MT cross section
!              flagnubar, &      ! flag for nubar
!              flagratio, &      ! flag for cross section ratio
!              flagres, &        ! flag for residual production cross section
!              flagrespar, &     ! flag for resonance parameters
!              flagri, &         ! flag for resonance integral
!              flagspec, &       ! flag for emission spectrum
!              flagspecav, &     ! flag for spectrum average
!              flagxs, &         ! flag for cross section
!              k0_xc5, &         ! incident particle
!              libinclude, &     ! flag to include library
!              MT_xc5, &         ! MT number
!              Nlib, &           ! number of data libraries for comparison
!              Nlibs, &          ! number of data libraries for comparison
!              NPgroup, &        ! number of points in group
!              Npoints_xc5, &    ! number of points per subentry
!              Npointstot_new, & ! total number of points in new database
!              numpoint, &       ! maximum number of data points in subentry
!              parZ, &           ! charge number of particle
!              pi, &             ! pi
!              string_xc5, &     ! date according to NDRC file
!              xs, &             ! cross section
!              xsr, &            ! cross section divided by Rutherford
!              Z_xc5, &          ! charge number
!              Zfy               ! Z of fission yield
! use A1_error_handling_mod, only: & ! Error handling
!          read_error ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  character(len=9)   :: fnstring
  integer, parameter :: numgroup=2000             !
  integer            :: istat                     ! error code
  integer            :: k                         ! counter
  integer            :: k0                        !
  integer            :: lib                       !
  integer            :: MT                        !
  integer            :: nen                       ! energy counter
  integer            :: Ngroup                    !
  integer            :: Ngroupsum(0:numgroup)     !
  integer            :: Np                        !
  integer            :: Nporg                     !
  integer            :: Z                         !
  real(sgl)          :: angrad                    !
  real(sgl)          :: cosangcor                 !
  real(sgl)          :: dx                        !
  real(sgl)          :: dxsgroupsum(0:numgroup)   !
  real(sgl)          :: dy                        !
  real(sgl)          :: e4mb                      !
  real(sgl)          :: Egroup(0:numpoint)        !
  real(sgl)          :: fac2                      !
  real(sgl)          :: factor                    !
  real(sgl)          :: rA                        !
  real(sgl)          :: rZ                        !
  real(sgl)          :: rZA                       !
  real(sgl)          :: Ein                       ! 
  real(sgl)          :: Einprev                   ! 
  real(sgl)          :: angprev                   ! 
  real(sgl)          :: x                         ! cosine of the angle
  real(sgl)          :: xsgroupsum(0:numgroup)    !
  real(sgl)          :: xsruth                    ! Rutherford cross section
  real(sgl)          :: y                         !
!
! ************************* Read dataset *******************************
!
  Np = Npoints_xc5
  Nporg = Npoints_xc5
  k0 = k0_xc5
  flagxs = flagMT .or. flagres
!
! Cross sections, resonance parameters, resonance integrals, ratios, fission yields, nubar
!
  if (flagxs .or. flagrespar .or. flagri .or. flagspecav .or. flagratio .or. flagfy .or. flagtke .or. flagnubar) then
    if (flagxs) flagcomp = .true.
    Emin = 1.e20
    Emax = 0.
    do k = Eindex, Np
      read(string_xc5(k), '(22x, 4e9.2,t249,a9)') x, dx, y, dy, fnstring
      E(k) = x * 1.e-6
      dE(k) = dx * 1.e-6
      xs(k) = y
      dxs(k) = dy
      if (fnstring == '         ') then
        Fnorm(k) = 1.
      else
        read(fnstring,*) Fnorm(k)
      endif
      if (flagxs .or. flagspecav) then
        if (y <= 1.e35) xs(k) = y * 1.e3
        if (dy <= 1.e35) dxs(k) = dy * 1.e3
!       if (flagdisc) then
!         if (k == Npoints_xc5) then
!           Npoints_new = k - Eindex + 1
!           Nend_new = k
!           flagsubdone = .true.
!           exit
!         endif
!         if (k == Npoints_new - Eindex) then
!           Npoints_new = k - Eindex
!           Nend_new = k - 1
!           Eindexnew = k
!           flagsubdone = .false.
!           exit
!         endif
!       endif
      endif
      if (flagtke) then
        if (y <= 1.e35) xs(k) = y * 1.e-6
        if (dy <= 1.e35) dxs(k) = dy * 1.e-6
      endif
      if (flagfy) then
        if (flagZAProd) then
          read(string_xc5(k), '(67x, e9.2,t248,a9)') rZA, fnstring
          rZ = rZA / 1000.
          rA = rZA - int(rZ) * 1000.
        else
          read(string_xc5(k), '(58x, 2e9.2,t248,a9)') rZ, rA, fnstring
        endif
        if (fnstring == '         ') then
          Fnorm(k) = 1.
        else
          read(fnstring,*) Fnorm(k)
        endif
        Zfy(k) = int(rZ)
        Afy(k) = int(rA)
        if (string_xc5(k)(20:20) == 'G') Ify(k) = 0
        if (string_xc5(k)(20:20) == 'M') Ify(k) = 1
        if (string_xc5(k)(20:20) == 'N') Ify(k) = 2
        if (k == Eindex) Einprev = E(k)
        if (k == Npoints_xc5) then
          Npoints_new = k - Eindex + 1
          Nend_new = k
          flagsubdone = .true.
          exit
        endif
        if (E(k) /= Einprev) then
          Npoints_new = k - Eindex
          Nend_new = k - 1
          Eindexnew = k
          flagsubdone = .false.
          exit
        endif
        if (k == Npoints_xc5) flagsubdone = .true.
      endif
      Emin = min(Emin, E(k))
      Emax = max(Emax, E(k))
    enddo
  endif
!
! Make resonance data groupwise for easier testing
!
  if (flaggroup .and. flagxs .and. k0 == 1) then
    MT = MT_xc5
    if (MT == 1 .or. MT == 2 .or. MT == 18 .or. MT == 102) then
      Ngroupsum = 0
      xsgroupsum = 0.
      dxsgroupsum = 0.
      Ngroup = 0
      Egroup = 0.
      do lib = Nlibs, 1, - 1
        if (libinclude(lib)) then
          Ngroup = Nlib(lib)
          do nen = 0, Ngroup
            Egroup(nen) = elib(lib, nen)
          enddo
          exit
        endif
      enddo
      if (Ngroup > 0 .and. Ngroup <= numgroup) then
        do k = 1, Np
          if (E(k) >= Egroup(0) .and. E(k) <= Egroup(Ngroup)) then
            call locate(Egroup, 0, Ngroup, E(k), nen)
            xsgroupsum(nen) = xsgroupsum(nen) + xs(k)
            dxsgroupsum(nen) = dxsgroupsum(nen) + dxs(k)
            Ngroupsum(nen) = Ngroupsum(nen) + 1
          endif
        enddo
        Np = 0
        NPgroup = 0
        do nen = 1, Ngroup
          if (Ngroupsum(nen) > 0) then
            Np = Np + 1
            E(Np) = Egroup(nen)
            NPgroup(Np) = Ngroupsum(nen)
            xs(Np) = xsgroupsum(nen) / Ngroupsum(nen)
            dxs(Np) = dxsgroupsum(nen) / Ngroupsum(nen)
          endif
        enddo
        Npoints_xc5 = Np
      endif
    endif
  endif
!
! Angular distributions and energy spectra
!
  if (flagelang .or. flagdiscang .or. flagspec .or. flagddx) then
    angmin = 1.e20
    angmax = 0.
    do k = Eindex, Npoints_xc5
      read(string_xc5(k), '(22x, e9.2, 9x, 3e9.2,t248,a9)', iostat = istat) Ein, y, dy, cosang(k), fnstring
      if (istat > 0) call read_error(string_xc5(k), istat)
      if (k == Npoints_xc5) flagsubdone = .true.
      if (y <= 1.e30) xs(k) = y * 1.e3
      if (dy <= 1.e30) dxs(k) = dy * 1.e3
      if (fnstring == '         ') then
        Fnorm(k) = 1.
      else
        read(fnstring,*) Fnorm(k)
      endif
      cosangcor = max(min(cosang(k), 1.), - 1.)
      angle(k) = acos(cosangcor) / pi * 180.
      if (flagspec .or. flagddx) then
        read(string_xc5(k), '(76x, 2e9.2,t248,a9)', iostat = istat) x, dx, fnstring
        if (istat /= 0) cycle
        E(k) = x * 1.e-6
        dE(k) = dx * 1.e-6
        if (fnstring == '         ') then
          Fnorm(k) = 1.
        else
          read(fnstring,*) Fnorm(k)
        endif
      endif
      angmin = min(angmin, angle(k))
      angmax = max(angmax, angle(k))
      if (k == Eindex) then
        Einprev = Ein
        angprev = cosang(k)
      endif
      if (flagelang .or. flagdiscang) then
        if (k > Eindex .and. cosang(k) == angprev) then
          Eindex = Np
          flagsubdone = .true.
          flagconv = .false.
          exit
        endif
      endif
      if (k == Npoints_xc5) then
        Npoints_new = k - Eindex + 1
        Nend_new = k
        flagsubdone = .true.
        exit
      endif
      if (Ein /= Einprev) then
        Npoints_new = k - Eindex
        Nend_new = k - 1
        Eindexnew = k
        flagsubdone = .false.
        exit
      endif
    enddo
  endif
  if (.not. flagMTequiv) Npointstot_new = Npointstot_new + Nporg
!
! For charged-particle elastic scattering, make extra data set with
! cross sections divided by Rutherford value
!
  if (flagelang .and. k0 > 1) then
    Z = Z_xc5
    e4mb = e2c * e2c * 10.
    if (Einc > 0.) then
      factor = e4mb * (parZ(k0) * Z / (4. * Einc)) **2
      do k = 1, Np
        angrad = (angle(k) / 180.) * pi
        fac2 = sin(0.5 * angrad) **4
        if (fac2 > 0.) then
          xsruth = factor / fac2
          xsr(k) = xs(k) / xsruth
          dxsr(k) = dxs(k) / xsruth
        endif
      enddo
    endif
  endif
  return
end subroutine readdata
! Copyright A.J. Koning 2019
