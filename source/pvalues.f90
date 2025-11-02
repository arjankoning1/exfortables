subroutine pvalues(Nsets)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Calculate p-value for each EXFOR point using other EXFOR points, data libraries and optionally TALYS
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
!              authors_E, &    ! authors of paper
!              compdir, &      ! directory with statistics
!              dxsexp, &         ! cross section uncertainty
!              dxsexpint, &         ! cross section uncertainty
!              dxsexplib, &         ! cross section uncertainty
!              Eexp, &           ! incident energy
!              flagout, &     ! flag for main output
!              flagtalys, &   ! flag for TALYS comparison
!              libexist, &    ! flag for existence of library
!              Nexp, &        ! number of experiments
!              Nlibs, &       ! number of data libraries for comparison
!              Npoints_E  , & ! number of points per subentry
!              pvalue, &          ! p-value
!              reaction_E, &   ! reaction identidier
!              subentry_E, &   ! subentry
!              xsexp, &          ! cross section
!              xsexpint, &          ! cross section
!              xsexplib          ! cross section
!
! *** Declaration of local data
!
  implicit none
  character(len=80)  :: pfile
  integer   :: iset                          !
  integer   :: Nsets                          !
  integer   :: ip                            !
  integer   :: lib                           !
  integer   :: NL                            !
  integer   :: i                             !
  integer   :: iexp                          !
  integer   :: j                             !
  integer   :: k                             !
  integer   :: N                             !
  integer   :: Ngrid                         !
  integer   :: Nscale                        !
  integer   :: Nmult                         !
  integer   :: Ndiv                          !
  integer, parameter :: numgrid = 20000                !
  real(sgl) :: EMeV                          ! incident energy
  real(sgl) :: xsmb                          ! cross section
  real(sgl) :: dxsmb                         ! uncertainty of cross section
  real(sgl) :: dxs0                          !
  real(sgl) :: fac1                          !
  real(sgl) :: fac2                          !
  real(sgl) :: gauss                         !
  real(sgl) :: sqrttwopi                     !
  real(sgl) :: pdf(0:numgrid)            ! PDF
  real(sgl) :: cdf(0:numgrid)            ! CDF
  real(sgl) :: xgrid(0:numgrid)            ! cross section of grid
  real(sgl) :: extreme                       !
  real(sgl) :: sum                           !
  real(sgl) :: sumpdf                        !
  real(sgl) :: sumcdf                        !
  real(sgl) :: xst                           !
  real(sgl) :: xsav                          !
  real(sgl) :: xsbegin                       !
  real(sgl) :: xsend                       !
  real(sgl) :: p                             !
!
! ***************************** Comparison *****************************
!
  if (flagtalys) libexist(1) = .true.
  sqrttwopi = sqrt(2.*3.1415926535)
  pvalue = 0.
  extreme = 10.
  Nscale = 6
  Nmult = 2
!
! Loop over experimental data points
!
  do iset = 1, Nsets
    do ip = 1, Npoints_E(iset)
      EMeV = Eexp(iset, ip)
      xsmb = xsexp(iset, ip)
      dxsmb = dxsexp(iset, ip)
      Ngrid = 0
      if (EMeV < allemin) cycle
      if (EMeV > allemax) cycle
!
! *** PDF from other data ***
!
! Average library value
!
      xsav = 0.
      sum = 0.
      NL = 0
      do lib = 1, Nlibs
        if ( .not. libexist(lib)) cycle
        if (xsexplib(lib, iset, ip) > 0.) then
          NL = NL + 1
          sum = sum + xsexplib(lib, iset, ip)
        endif
      enddo
!
! Make grid for PDF
!
      if (sum > 0. ) then
        xsav = sum / NL
        k = 0
        Ndiv = 1
        do i = 1, Nscale - 1
          xsbegin = real(i) / Nscale * xsav
          xsend = real(i+1) / Nscale * xsav
          Ndiv = Ndiv * Nmult
          dxs0 = (xsend - xsbegin) / Ndiv
          do j = 0, Ndiv - 1
            k = k + 1
            xgrid(k) = xsbegin + j * dxs0
          enddo
        enddo
        Ngrid = k
        Ndiv = Ndiv * Nmult
        do i = 1, Nscale
          xsbegin = i * xsav
          xsend = real(i+1)  * xsav
          if (i == 1) then
            Ndiv = Ngrid
          else
            Ndiv = Ndiv / Nmult
          endif
          dxs0 = (xsend - xsbegin) / Ndiv
          do j = 0, Ndiv - 1
            k = k + 1
            xgrid(k) = xsbegin + j * dxs0
          enddo
        enddo
        Ngrid = k
        pdf = 0.
!
! Loop over libraries and other experimental data sets to determine p-value
!
! Libraries
!
        do lib = 1, Nlibs
          if ( .not. libexist(lib)) cycle
          xst = xsexplib(lib, iset, ip)
          if (xst > 0.) then
            dxs0 = dxsexplib(lib, iset, ip)
            if (dxs0 == 0.) dxs0 = 0.10 * xst
            fac1=1./(dxs0*sqrttwopi)
            fac2=1./(2.*dxs0**2)
            do i = 1, Ngrid
              gauss=fac1*exp(-(xst-xgrid(i))**2*fac2)
              pdf(i) = pdf(i) + gauss
            enddo
          endif
        enddo
!
! Other experimental data sets
!
        do iexp = 1, Nexp(iset, ip)
          xst = xsexpint(iexp, iset, ip)
          if (xst > 0.) then
            dxs0 = dxsexpint(iexp, iset, ip)
            if (dxs0 == 0.) dxs0 = 0.10 * xst
            fac1=1./(dxs0*sqrttwopi)
            fac2=1./(2.*dxs0**2)
            do i = 1, Ngrid
              gauss=fac1*exp(-(xst-xgrid(i))**2*fac2)
              pdf(i) = pdf(i) + gauss
            enddo
          endif
        enddo
!
! Make pdf and cdf
!
        sumpdf = 0.
        do i = 1, Ngrid
          sumpdf = sumpdf + pdf(i) *(xgrid(i) - xgrid(i-1))
        enddo
        N = 0
        if (sumpdf > 0.) then
          sumcdf = 0.
          cdf = 0.
          do i = 1, Ngrid
            pdf(i) = pdf(i) / sumpdf
            sumcdf = sumcdf + pdf(i) *(xgrid(i) - xgrid(i-1))
            cdf(i) = sumcdf
          enddo
          call locate(xgrid, 0, Ngrid, xsmb, N)
          if (N >= 0) then
            if (cdf(N) < 0.5) then
              p = cdf(N)
            else
              p = 1. - cdf(N)
            endif
            pvalue(iset, ip) = abs(p)
          endif
        endif
      endif
    enddo
    if (flagout .and. Npoints_E(iset) == 1) then
      pfile=trim(compdir)//'pdist/'//trim(subentry_E(iset))//'.p'
      open (unit = 27, status = 'unknown', file = pfile)
      write(27,'("# Subentry ",a)') trim(subentry_E(iset))
      write(27,'("# Reaction ",a)') trim(reaction_E(iset))
      write(27,'("# Authors  ",a)') trim(authors_E(iset))
      write(27,'("# xsmb     ",es15.6)') xsmb
      write(27,'("# xs-av    ",es15.6)') xsav
      write(27,'("# p-value  ",es15.6)') pvalue(iset, 1)
      write(27,'("#      i              xs               pdf                cdf")')
      do i = 1, Ngrid
        write(27,'(i6,9x,3es15.6)') i, xgrid(i), pdf(i), cdf(i)
      enddo
      close(27)
    endif
  enddo
  return
end subroutine pvalues
