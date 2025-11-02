subroutine writesub
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write subentry in new format
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
!              author1_xc5, &  ! author
!              flagcomp, &     ! flag for comparison with libraries and TALYS
!              flagconv, &     ! flag for conversion to new database
!              reaction_xc5, & ! XC5 reaction string
!              subentry_xc5, & ! XC5 subentry number
!              xsfile          ! cross section file
!
! *** Declaration of local data
!
  implicit none
  integer           :: ff          !
!
! *************************** Write dataset ****************************
!
! header      : subroutine to write header for output file
! writedata   : subroutine to write numerical data
! writerefqual: subroutine to write bibliography and quality information
!
  if (flagconv) then
    open (unit = 1, status = 'unknown', file = trim(xsfile))
    call header
    call writedata
    call writeref
    close (unit = 1)
  endif
!
! Write subentries which are and which are not converted from XC5
!
  if (flagconv) then
    ff = 9
  else
    ff = 4
  endif
  write(ff, '(1x, a9, 1x, a25, 1x, a)') subentry_xc5, author1_xc5, reaction_xc5
  write(ff, '(a132)') string_xc5(1)
!
! Write subentries which are and which are not compared with TALYS and libraries.
!
  if (flagconv) then
    if (flagcomp) then
      ff = 19
    else
      ff = 14
    endif
    write(ff, '(1x, a9, 1x, a25, 1x, a)') subentry_xc5, author1_xc5, reaction_xc5
  endif
  return
end subroutine writesub
! Copyright A.J. Koning 2019
