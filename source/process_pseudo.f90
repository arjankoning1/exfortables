subroutine process_pseudo(Z,A,nuclide)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process pseudo data for neutron capture
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
!
  use A0_exfortables_mod
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist
  integer            :: istat
  integer            :: iloop
  integer            :: Z
  integer            :: A
  character(len=6)   :: nuclide    ! nuclide
  character(len=8)   :: dname(3)
  character(len=8)   :: subE
  character(len=10)   :: dir(3)
  character(len=200) :: cmd       ! command
  character(len=132) :: pseudofile   ! filename
  character(len=132) :: xspathname ! filename
  character(len=132) :: pathname   ! filename
  character(len=132) :: filename   ! filename
  character(len=132) :: xslist     ! 
!
! **************** Read data and process it into new database **********
!
  dir(1)='MACSxs/'
  dir(2)='gamgamxs/'
  dir(3)='Profil/'
  dname(1)='Kadonis'
  dname(2)='Gamgam'
  dname(3)='Profil'
  subE = '00000000'
  write(subE(3:5),'(i3.3)') Z
  write(subE(6:8),'(i3.3)') A
  do iloop = 1, 3
    write(subE(2:2),'(i1)') iloop
    filename = 'n-'//trim(nuclide)//'-MT102-'//trim(dname(iloop))//'-'//subE//'.2024'
    pseudofile = trim(filespath)//trim(dir(iloop))//trim(filename)
    inquire (file = pseudofile, exist = lexist)
    if (lexist) then
      xspathname = 'n/'//trim(nuclide_xc5)//'/xs/'
      pathname = trim(xspathname)//'102/'
      cmd = 'mkdir -p '//trim(pathname)
      call command(cmd)
      cmd = 'cp '//trim(pseudofile)//' '//trim(pathname)
      call command(cmd)
      xslist = trim(xspathname)//'xslist'
      open (unit = 79, status = 'unknown', file = xslist, position = 'append', iostat = istat)
      if (istat == 0) write(79, '(i6, 1x, a, 1x, a8)') 102, trim(filename), subE
      close (79)
      xslist = trim(pathname)//'n-'//trim(nuclide_xc5)//'-MT102.list'
      open (unit = 79, status = 'unknown', file = xslist, position = 'append', iostat = istat)
      if (istat == 0) then
        if (iloop == 1) write(79, '(a60,i6,2es12.5)') filename, 20, 0.005, 0.1
        if (iloop == 2) write(79, '(a60,i6,2es12.5)') filename, 2, 0.005, 0.010
        if (iloop == 3) write(79, '(a60,i6,2es12.5)') filename, 33, 0.001, 2.
      endif
      close (79)
    endif
  enddo
  return
end subroutine process_pseudo
! Copyright A.J. Koning 2024
