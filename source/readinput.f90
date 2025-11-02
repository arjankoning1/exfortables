subroutine readinput
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read user input
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
!              inline, &     ! input line
!              nlines, &     ! number of input lines
!              numinlines    ! maximum number of input lines
! use A1_error_handling_mod, only: & ! Error handling
!          range_integer_error, & ! Test if integer variable is out of range
!          read_error             ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  integer            :: i      ! counter
  integer            :: istat  ! logical for file access
!
! ************************** User Input ********************************
!
! We read the complete input file first as a set of character strings.
! The actual keywords will be read from these later on.
!
  i = 1
  do
    read(*, '(a132)', iostat = istat) inline(i)
    if (istat ==  - 1) exit
    if (istat /= 0) call read_error(inline(i), istat)
    i = i + 1
    call range_integer_error('inline', i, 1, numinlines)
  enddo
  nlines = i - 1
!
! ************** Convert uppercase to lowercase characters *************
!
! For easy handling of all the input parameters, the whole input is converted to lowercase characters,
! with the exception of filenames or other character strings.
!
! convert: subroutine to convert input line from upper case to lowercase
!
  do i = 1, nlines
    call convert(i)
  enddo
  return
end subroutine readinput
! Copyright A.J. Koning 2019
