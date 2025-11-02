subroutine machine
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Machine dependent statements
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     20-02-2023   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A1_exfortables_mod, only: & ! All global variables
!              day, &       ! day
!              filespath, & ! directory containing X4 and structure files to be read
!              levpath, &   ! directory containing level files to be read
!              libspath, &  ! directory containing data libraries
!              month, &     ! month
!              talyspath, & ! directory containing TALYS results
!              year         ! year
!
! *** Declaration of local data
!
  implicit none
  integer :: values(8)              ! date and time values
  integer :: ix
  character(len=132) :: codedir     ! code directory
  character(len=132) :: basedir     ! base directory
!
! ************************ Set directories *****************************
!
  codedir = '/Users/koning/exfortables/'
  ix = index(codedir,'/exfortables/')
  basedir = codedir(1:ix)
  talyspath = trim(basedir)//'drip/'
  filespath = trim(codedir)//'files/'
  libspath = trim(basedir)//'libraries/'
  levpath = trim(filespath)//'levels/'
!
! Set date
!
  call date_and_time(VALUES=values)
  year=values(1)
  month=values(2)
  day=values(3)
  date='xxxx-xx-xx'
  write(date(1:4),'(i4.4)') year
  write(date(6:7),'(i2.2)') month
  write(date(9:10),'(i2.2)') day
  user = 'Arjan Koning'
  return
end subroutine machine
! Copyright A.J. Koning 2023
