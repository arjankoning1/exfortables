subroutine readquality
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialize quality information
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
!              filespath, & ! directory containing X4 and structure files to be read
!              NQ, &        ! number of items with quality information
!              Qaction0, &  ! recommended action
!              Qdate0, &    ! quality date
!              Qscore0, &   ! quality score
!              Qsub0        ! subentry with quality information
! use A1_error_handling_mod, only: & ! Error handling
!              read_error ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist   ! logical for existence
  character(len=80)  :: cmd      ! command
  character(len=160) :: str      ! string
  integer            :: i        ! counter
  integer            :: istat    ! error code
!
! Initialize quality information
!
! Following 2 lines not active while doing quality scoring in file quality.all
!
!     cmd='cat quality/*.q > quality/quality.all'
!     call command(cmd)
!
  write(*, *) "Reading quality database....."
  cmd = 'rm quality/*.q'
  call command(cmd)
  NQ = 0
  inquire (file = trim(filespath)//'quality.all', exist = lexist)
  if (lexist) then
    open (unit = 2, status = 'old', file = trim(filespath)//'quality.all')
    i = 0
    do
      read(2, '(a160)', iostat = istat) str
      if (istat == -1) exit
      if (istat > 0) call read_error(trim(filespath)//'quality.all', istat)
      if (str(1:9) == '# Quality') then
        i = i + 1
        Qsub0(i) = str(12:20)
        Qscore0(i) = str(23:24)
      endif
      if (str(1:6) == '# Date') Qdate0(i) = str(23:32)
      if (str(1:8) == '# Action') Qaction0(i) = str(23:160)
    enddo
    close (unit = 2)
    NQ = i
  endif
  write(*, *) "Reading quality database done"
  write(*, *) "Reading weights database....."
  NW = 0
  inquire (file = trim(filespath)//'score.tab', exist = lexist)
  if (lexist) then
    open (unit = 2, status = 'old', file = trim(filespath)//'score.tab')
    i = 0
    do
      read(2, '(a)', iostat = istat) str
      if (istat == -1) exit
      if (istat > 0) call read_error(trim(filespath)//'score.tab', istat)
      if (str(1:1) == '#') cycle
      if (str(1:1) == ' ') cycle
      i = i + 1
      subweight(i) = str(1:9)
      read(str(11:11),'(i1)') weight0(i)
    enddo
    close (unit = 2)
    NW = i
  endif
  return
end subroutine readquality
! Copyright A.J. Koning 2019
