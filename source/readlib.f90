subroutine readlib
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read data from nuclear data libraries
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
! use A1_exfortables_mod, only: & ! All global variables
!              A_xc5, &       ! mass number
!              dxslib, &      ! cross section uncertainty of library
!              elib, &        ! energy of library
!              xsnon, &       ! non - elastic cross section
!              flaggroup, &   ! flag to group resonance data
!              flagMT, &      ! flag for MT cross section
!              flagres, &     ! flag for residual production cross section
!              isochar, &     ! symbol of isomer
!              isom_xc5, &    ! number of isomer
!              k0_xc5, &      ! incident particle
!              libexist, &    ! flag for existence of library
!              libinclude, &  ! flag to include library
!              library, &     ! nuclear data library
!              libspath, &    ! directory containing data libraries
!              MT_xc5, &      ! MT number
!              Nlib, &        ! number of data libraries for comparison
!              Nlibs, &       ! number of data libraries for comparison
!              Nnon, &        ! number of non - elastic energies
!              nuclide_xc5, & ! target nuclide
!              numpoint, &    ! maximum number of data points in subentry
!              proj, &        ! projectile
!              xslib, &       ! cross section of library
!              Enon, &        ! energy for non - elastic cross section
!              Z_xc5, &       ! charge number
!              ZAres          ! ZA of residual nucleus
! use A1_error_handling_mod, only: & ! Error handling
!          read_error ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist      !
  character(len=4)   :: MTstr       !
  character(len=7)   :: ZAstr      !
  character(len=10)  :: libr
  character(len=80)  :: MTfile      !
  character(len=80)  :: resfile      !
  character(len=80)  :: MTstring    !
  character(len=80)  :: resstring    !
  character(len=132) :: key
  character(len=132) :: line
  character(len=132) :: libfile     !
  character(len=132) :: reacpath    !
  integer            :: A           !
  integer            :: i           ! counter
  integer            :: istat       ! error code
  integer            :: k0          !
  integer            :: N
  integer            :: Ncol
  integer            :: keyix
  integer            :: lib         !
  integer            :: MT          !
  integer            :: Z           !
  integer            :: iz
  integer            :: ia
  real               :: xslow           !
  real               :: xsup           !
  real               :: resE
!
! ************************* Determine library file name ****************
!
  libexist = .false.
  Z = Z_xc5
  A = A_xc5
  k0 = k0_xc5
  if (flagMT) then
    MT = MT_xc5
    MTstr = '    '
    write(MTstr(1:3), '(i3.3)') MT
    if (isom_xc5 >= 0) MTstr(4:4) = isochar(isom_xc5)
    MTstring = proj//'-'//trim(nuclide_xc5)//'-MT'//trim(MTstr)
  endif
  if (flagres) then
    ZAstr = '       '
    write(ZAstr(1:6), '(i6.6)') ZAres
    if (isom_xc5 >= 0) ZAstr(7:7) = isochar(isom_xc5)
  endif
  elib = 0.
  xslib = 0.
  dxslib = 0.
  resEnergy = 0.
  do lib = 2, Nlibs
    if ( .not. libinclude(lib)) cycle
    libfile = ' '
    if (k0 /= 1 .and. lib == 2) then
      libr = 'tendl.2025'
    else
      libr = trim(library(lib))
    endif
    reacpath = trim(libspath)//proj//'/'//trim(nuclide_xc5) // '/' // trim(libr) // '/tables/'
    if (flagMT) then
      if (flaggroup .and. k0 == 1 .and. (MT == 1 .or. MT == 2 .or. MT == 18 .or. MT == 102)) then
        MTfile = trim(MTstring)//'-G1102.'//libr
        if (resEnergy == 0.) then
          open (unit = 28, status = 'unknown', file = trim(filespath)//'resEnergy.dat')
          do
            read(28,*,iostat=istat) iz, ia, resE
            if (istat == -1) exit
            if (iz == Z .and. ia == A) then
              resEnergy = resE * 1.e-6
              exit
            endif
          enddo
          close(28)
        endif
      else
        MTfile = trim(MTstring)//'.'//libr
      endif
      libfile = trim(reacpath)//'xs/'//MTfile
    endif
    if (flagres) then
      resstring = proj//'-'//trim(nuclide_xc5)//'-rp'// trim(ZAstr)
      resfile = trim(resstring)//'.'//libr
      libfile = trim(reacpath)//'residual/'//resfile
    endif
    inquire (file = libfile, exist = lexist)
    if ( .not. lexist) cycle
    libexist(lib) = .true.
    if (libfile(1:1) == ' ') cycle
    open (unit = 10, status = 'unknown', file = libfile)
    do
      read(10,'(a)',iostat = istat) line
      if (istat == -1) exit
      key='columns'
      keyix=index(line,trim(key)) 
      if (keyix > 0) read(line(keyix+len_trim(key)+2:80),*, iostat = istat) Ncol
      key='entries'
      keyix=index(line,trim(key)) 
      if (keyix > 0) then
        read(line(keyix+len_trim(key)+2:80),*, iostat = istat) N
        if (istat /= 0) call read_error(libfile, istat)
        read(10,'(/)')
        N=min(N,numpoint)
        if (Ncol == 4) then
          do i=1,N
            read(10, * , iostat = istat) elib(lib, i), xslib(lib, i), xslow, xsup
            if (istat == -1) exit
            if (istat == -2) call read_error(libfile, istat, eor = 'continue')
            if (istat > 0) call read_error(libfile, istat)
            if (xslib(lib, i) > 0. .and. xslow > 0. .and. xsup > 0.) dxslib(lib ,i) = xsup / xslib(lib, i) - 1.
          enddo
          exit
        else
          do i=1,N
            read(10, * , iostat = istat) elib(lib, i), xslib(lib, i)
            if (istat == -1) exit
            if (istat == -2) call read_error(libfile, istat, eor = 'continue')
            if (istat > 0) call read_error(libfile, istat)
          enddo
          exit
        endif
      endif
    enddo
    Nlib(lib) = N
    elib(lib, 0) = elib(lib, 1)
    xslib(lib, 0) = xslib(lib, 1)
    close (10)
    if (lib == 2) then
!
! Read non-elastic cross section
!
      if (k0 /= 1) then
        libr = 'tendl.2025'
      else
        libr = trim(library(lib))
      endif
      MTfile = proj//'-'//trim(nuclide_xc5)//'-MT003'//'.'//libr
      libfile = trim(reacpath)//'xs/'//MTfile
      inquire (file = libfile, exist = lexist)
      if ( .not. lexist) cycle
      open (unit = 10, status = 'unknown', file = libfile)
      do
        read(10,'(a)',iostat = istat) line
        if (istat == -1) exit
        key='entries'
        keyix=index(line,trim(key)) 
        if (keyix > 0) then
          read(line(keyix+len_trim(key)+2:80),*, iostat = istat) Nnon
          if (istat /= 0) call read_error(libfile, istat)
          read(10,'(/)')
          do i = 1, Nnon
            read(10, * , iostat = istat) Enon(i), xsnon(i)
            if (istat /= 0) cycle
          enddo
          Enon(0) = 0.
          xsnon(0) = xsnon(1)
          exit
        endif
      enddo
      close (10)
    endif
  enddo
  return
end subroutine readlib
! Copyright A.J. Koning 2019
