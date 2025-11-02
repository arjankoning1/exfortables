subroutine pathsub
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Determine path name
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
!              flagddx, &     ! flag for DDX emission spectrum
!              flagdiscang, & ! flag for discrete angular distribution
!              flagelang, &   ! flag for elastic angular distribution
!              flagfy, &      ! flag for fission yields
!              flagtke, &     ! flag for TKE
!              flagMT, &      ! flag for MT cross section
!              flagnubar, &   ! flag for nubar
!              flagratio, &   ! flag for cross section ratio
!              flagres, &     ! flag for residual production cross section
!              flagrespar, &  ! flag for resonance parameters
!              flagri, &      ! flag for resonance integral
!              flagspec, &    ! flag for emission spectrum
!              flagspecav, &  ! flag for spectrum average
!              isochar, &     ! character for isomer
!              isom_xc5, &    ! isomer of final state
!              k0_xc5, &      ! incident particle
!              kres, &        ! type of outgoing particle
!              MT_xc5, &      ! MT number
!              Nsub_xc5, &    ! counter for XC5 subentries
!              nuclide_xc5, & ! nuclide
!              partype, &     ! symbol of particle
!              path, &        ! path
!              xspath, &      ! path of cross section
!              ZAP_xc5, &     ! projectile
!              ZAres          ! ZA of residual nucleus
!
! *** Declaration of local data
!
  implicit none
  character(len=4) :: MTstr      !
  character(len=7) :: ZAPstr      !
  character(len=7) :: ZAstr      !
  character(len=80) :: cmd      !
  character(len=132) :: path0      !
  logical          :: lexist     !
  integer          :: isub       !
  integer          :: k0         !
  integer          :: MT         !
!
! ************************* Create directory for reaction **************
!
! Make directory for the element, if it does not exist yet.
!
  isub = Nsub_xc5
  k0 = k0_xc5
  MT = MT_xc5
  MTstr = '    '
  write(MTstr(1:3), '(i3.3)') MT
  if (isom_xc5 >= 0) MTstr(4:4) = isochar(isom_xc5)
  path = ' '
  xspath = ' '
  if (flagfy .or. flagtke) then
    path = 'FY/'//partype(k0)//'/'
  else
    path = partype(k0)//'/'
  endif
  if (k0 == 7) then
    ZAPstr = '      /'
    write(ZAPstr(1:6), '(i6.6)') ZAP_xc5
    path = trim(path) // ZAPstr
  endif
  inquire (file = path, exist = lexist)
  if (.not. lexist) then
    cmd='mkdir -p '//trim(path)
    call command(cmd)
  endif
!
! Make directory for the nuclide, if it does not exist yet.
!
  path = trim(path)//trim(nuclide_xc5)//'/'
  inquire (file = path, exist = lexist)
  if (.not. lexist) then
    cmd = 'mkdir -p '//trim(path)
    call command(cmd)
  endif
!
! Make directory for the reaction channel, if it does not exist yet.
!
  path0 = trim(path)
  if (flagMT) xspath = trim(path0)//'xs/'
  if (flagelang .or. flagdiscang) xspath = trim(path0)//'angle/'
  if (flagfy .or. flagtke) xspath = trim(path0)
  if (flagnubar) xspath = trim(path0)//'fission/'
  if (flagrespar) xspath = trim(path0)//'resonance/'
  if (flagri) xspath = trim(path0)//'resint/'
  if (flagspecav) xspath = trim(path0)//'specav/'
  if (flagreacstyle) then
    reacstring(MT)(1:1) = partype(k0)
    path = trim(xspath) // trim(reacstring(MT)) // '/'
  else
    path = trim(xspath) // trim(MTstr) // '/'
  endif
  if (flagres) path = trim(path0)//'residual/'
  if (flagratio) path = trim(path0)//'ratio/'
  if (flagspec) path = trim(path0)//'spectrum/'//partype(kres)//'/'
  if (flagddx) path = trim(path0)//'ddx/'//partype(kres)//'/'
  if (flagres .or. flagratio) then
    inquire (file = path, exist = lexist)
    if (.not. lexist) then
      cmd = 'mkdir -p '//trim(path)
      call command(cmd)
    endif
    ZAstr = '       '
    write(ZAstr(1:6), '(i6.6)') ZAres
    if (isom_xc5 >= 0) ZAstr(7:7) = isochar(isom_xc5)
    xspath = trim(path)
    path = trim(xspath) // trim(ZAstr) // '/'
  endif
  inquire (file = trim(path), exist = lexist)
  if (.not. lexist) then
    cmd = 'mkdir -p '//trim(path)
    call command(trim(cmd))
  endif
  return
end subroutine pathsub
! Copyright A.J. Koning 2019
