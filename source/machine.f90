subroutine machine
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Machine dependent statements
!
! Revision    Date      Author      Quality  Description
! =====================================================
!    1     20-02-2023   A.J. Koning    A     Original code
!    2     2026-08-27   A.J. Koning    A     Runtime definition of EXFORTABLES directory and user
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
!
  use A0_exfortables_mod
!
! *** Declaration of local data
!
  implicit none
  logical             :: lexist
  integer             :: values(8)
  integer             :: i
  integer             :: n
  integer             :: envstat
  character(len=1024) :: code_dir
  character(len=1024) :: base_dir
  character(len=1024) :: exfortables_dir
  character(len=1024) :: exfortables_user
!
! ************************ Set directories *****************************
!
! The preferred option is to set the environment variable
! EXFORTABLES_DIR, for example in ~/.profile or ~/.zshrc:
!
! export EXFORTABLES_DIR=/path/to/exfortables
!
  call get_environment_variable('EXFORTABLES_DIR', exfortables_dir, length=n, status=envstat)
  if (envstat == 0 .and. n > 0) then
    code_dir = trim(exfortables_dir)
  else
!
! If the environment variable cannot be used, the code directory can be
! changed here manually.
!
    code_dir = '/path/to/exfortables/'
  endif
!
! Remove a trailing slash, if present, and determine the parent directory.
!
  i = len_trim(code_dir)
  if (i > 1) then
    if (code_dir(i:i) == '/') code_dir = code_dir(:i - 1)
  endif
  i = scan(trim(code_dir), '/', back=.true.)
  if (i > 0) then
    base_dir = code_dir(:i)
  else
    base_dir = './'
  endif
!
! Default paths. These can be overridden with the existing input keywords
! talyspath, filespath and libspath.
!
  talyspath = trim(base_dir)//'drip/'
  filespath = trim(code_dir)//'/files/'
  libspath = trim(base_dir)//'libraries/'
  levpath = trim(filespath)//'levels/'
!
! Check that EXFORTABLES_DIR points to the distributed database.
! The raw files/ reconstruction tree is not required merely to use the
! distributed database and is therefore not used as this directory check.
!
  inquire (file=trim(code_dir)//'/special/exfor_30keV.txt', exist=lexist)
  if (.not. lexist) then
    write(*, '(a)') 'EXFORTABLES error: database not found.'
    write(*, '(2a)') 'Expected file: ', trim(code_dir)//'/special/exfor_30keV.txt'
    write(*, '(a)') 'Set the EXFORTABLES_DIR environment variable:'
    write(*, '(a)') '  export EXFORTABLES_DIR=/path/to/exfortables'
    write(*, '(a)') 'Alternatively, edit code_dir in source/machine.f90'
    write(*, '(a)') 'and rebuild EXFORTABLES.'
    error stop 77
  endif
!
! ************************ Set date ***********************************
!
  call date_and_time(VALUES=values)
  year=values(1)
  month=values(2)
  day=values(3)
  date='xxxx-xx-xx'
  write(date(1:4),'(i4.4)') year
  write(date(6:7),'(i2.2)') month
  write(date(9:10),'(i2.2)') day
!
! Set user name for generated output. The input keyword "user" can
! override this value for an individual run.
!
  call get_environment_variable('EXFORTABLES_USER', exfortables_user, length=n, status=envstat)
  if (envstat == 0 .and. n > 0) then
    user = trim(exfortables_user)
  else
    user = 'Unknown User'
  endif
  return
end subroutine machine
! Copyright A.J. Koning 2026
