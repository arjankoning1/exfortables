subroutine readexp
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read numerical data from file
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
! use A1_error_handling_mod, only: & ! Error handling
!                      read_error ! Message for file reading error
! use A1_exfortables_mod, only: & ! All global variables
!              author1_E, &    ! author
!              authors_E, &    ! authors of paper
!              dEexp, &        ! incident energy uncertainty
!              dxsexp, &       ! cross section uncertainty
!              Eexp, &         ! incident energy
!              Emax_E, &       ! maximum energy (MeV) of subentry
!              Emin_E, &       ! minimum energy (MeV) of subentry
!              Eunc, &         ! average energy uncertainty of subentry
!              Euncabs, &      ! average absolute energy uncertainty of subentry
!              expfile, &      ! experimental data file
!              Npoints_E, &    ! number of points per subentry
!              Nset, &         ! number of experimental data sets
!              reaction_E, &   ! reaction identidier
!              reference_E, &  ! reference of paper
!              subentry_E, &   ! subentry
!              title_E, &      ! title of paper
!              uncertainty, &  ! average uncertainty of subentry
!              xsexp, &        ! cross section
!              xsexpav, &      ! average experimental cross section
!              year_E          ! year
!
! *** Declaration of local data
!
  implicit none
  character(len=132) :: line
  character(len=132) :: key
  integer            :: i           !
  integer            :: keyix
  integer            :: istat        !
  real(sgl)          :: Euncabssum                !
  real(sgl)          :: Euncsum                   !
  real(sgl)          :: xssum                     !
  real(sgl)          :: uncsum                    !
!
! *************************** Initialization **************************
!
  if (Nset == 1) then
    subentry_E = ''
    author1_E = ''
    reaction_E = ''
    title_E = ''
    authors_E = ''
    reference_E = ''
    Npoints_E = 0
    year_E = 0
    Emax_E = 0.
    Emin_E = 0.
    Eexp = 0.
    dEexp = 0.
    xsexp = 0.
    dxsexp = 0.
    Eunc = 0.
    Euncabs = 0.
    uncertainty = 0.
    xsexpav = 0.
  endif
!
! *************************** Read dataset ****************************
!
  open(unit=11, file = expfile(Nset), status = 'unknown', iostat = istat)
  if (istat /= 0) call read_error(expfile(Nset), istat)
  do
    read(11, '(a132)', iostat = istat) line
    if (istat == -1) exit
    if (line(1:12) == '#   subentry') read(line(15:23), '(a9)') subentry_E(Nset)
    if (line(1:15) == '#   X4 reaction') read(line(18:47), '(a30)') reaction_E(Nset)
    if (line(1:10) == '#   author') read(line(13:26), '(a14)') author1_E(Nset)
    if (line(1:8) == '#   year') read(line(11:14), '(i4)') year_E(Nset)
    key='entries'
    keyix=index(line,trim(key))
    if (keyix > 0) then
      read(line(keyix+len_trim(key)+2:80),*, iostat = istat)  Npoints_E(Nset)
      if (istat /= 0) call read_error(expfile(Nset), istat)
      read(11,'(/)')
!   if (line(1:13) == '# Data points') then
!     read(line(17:22), '(i6)') Npoints_E(Nset)
!     read(11, '(a)', iostat = istat) line
      xssum = 0.
      uncsum = 0.
      Euncsum = 0.
      Euncabssum = 0.
      Emin_E(Nset) = 1.e20
      Emax_E(Nset) = 0.
      do i = 1, Npoints_E(Nset)
        read(11, '(4es15.6)', iostat = istat) Eexp(Nset, i), dEexp(Nset, i), xsexp(Nset, i), dxsexp(Nset, i)
        if (istat /= 0) call read_error(expfile(Nset), istat)
        if (xsexp(Nset, i) > 0.) then
          xssum = xssum + xsexp(Nset, i)
          uncsum = uncsum + dxsexp(Nset, i) / xsexp(Nset, i)
        endif
        if (Eexp(Nset, i) > 0.) then
          Euncabssum = Euncabssum + dEexp(Nset, i)
          Euncsum = Euncsum + dEexp(Nset, i) / Eexp(Nset, i)
        endif
        Emin_E(Nset) = min(Emin_E(Nset), Eexp(Nset, i))
        Emax_E(Nset) = max(Emax_E(Nset), Eexp(Nset, i))
      enddo
      xsexpav(Nset) = xssum / Npoints_E(Nset)
      uncertainty(Nset) = 100. * uncsum / Npoints_E(Nset)
      Euncabs(Nset) = Euncabssum / Npoints_E(Nset)
      Eunc(Nset) = 100. * Euncsum / Npoints_E(Nset)
    endif
    if (line(1:12) == '# Reference:') then
      read(11, '(1x,a)', iostat = istat) authors_E(Nset)
      read(11, '(1x,a)', iostat = istat) title_E(Nset)
      read(11, '(1x,a)', iostat = istat) reference_E(Nset)
    endif
  enddo
  close(11)
end subroutine readexp
! Copyright A.J. Koning 2019
