subroutine writeF(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write F values on reaction file
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
! use A1_error_handling_mod, only: & ! Error handling
!                      read_error ! Message for file reading error
! use A1_exfortables_mod, only: & ! All global variables
!              Aspoint, &        ! asymmetry
!              Asset, &          ! average As value per subentry
!              chi2point, &      ! chi - square
!              chi2set, &        ! average Chi - 2 per subentry
!              expfile, &      ! experimental data file
!              Fpoint, &         ! F value
!              Fset, &           ! average F value per subentry
!              library, &     ! nuclear data library
!              Npoints_E, &  ! number of points per subentry
!              numpoint, &     ! maximum number of data points in subentry
!              pointref, &     ! reference for pointwise comparison
!              pset, &     ! p-value per subentry
!              pvalue, &     ! p-value
!              xsthpoint         ! theoretical cross section
!
! *** Declaration of local data
!
  implicit none
  character(len=132) :: string(numpoint)         !
  integer            :: i           !
  integer            :: iset        !
  integer            :: k           !
  integer            :: kp          !
  integer            :: N           !
  integer            :: istat        !
!
! *************************** Read dataset ****************************
!
  string = ' '
  open(unit=11, file = expfile(iset), status = 'unknown', iostat = istat)
  if (istat /= 0) call read_error(expfile(iset), istat)
  i = 0
  kp = 0
  do
    i = i + 1
    read(11, '(a132)', iostat = istat) string(i)
    if (istat == -1) exit
    if (string(i)(1:13) == '# Data points') then
      read(string(i)(17:22), '(i6)') Npoints_E(iset)
      kp = i + 1
    endif
  enddo
  N = i - 1
  close(11)
  if (kp == 0) return
  open(unit=11, file = expfile(iset), status = 'unknown', iostat = istat)
  do i = 1, N
    write(11, '(a)', iostat = istat) trim(string(i))
  enddo
! if (flaggroup .and. k0_xc5 == 1 .and. (MT_xc5 == 1 .or. MT_xc5 == 2 .or. MT_xc5 == 18 .or. MT_xc5 == 102)) then
    write(string(kp)(53:132), '(4x, a10, "    F        A(C/E)       Chi-2     p-value      Nset     N")') library(pointref)
! else
!   write(string(kp)(53:132), '(4x, a10, "    F        A(C/E)       Chi-2")') library(pointref)
! endif
  write(11, '("#")')
  write(11, '("# Library comparison")')
  write(11, '("#")')
  write(11, '(a)', iostat = istat) trim(string(kp))
  do k = 1, Npoints_E(iset)
    i = kp + k
    write(string(i)(53:132), '(1p, e13.5, 4g12.4,2i6)') xsthpoint(pointref, k), Fpoint(pointref, k), Aspoint(pointref, k), &
 &    chi2point(pointref, k), pvalue(iset, k), iset, k
    string(i)(1:1) = '#'
    write(11, '(a)', iostat = istat) trim(string(i))
  enddo
  write(11, '("#")')
  write(11, '("# Average deviation:", 45x, 1p, 4g12.4)') Fset(pointref), Asset(pointref), chi2set(pointref), pset(iset)
  close(11)
end subroutine writeF
! Copyright A.J. Koning 2019
