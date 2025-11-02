subroutine writeref
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write reference
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
!              authors_xc5, &   ! authors of paper
!              nauthors, &      ! number of lines with authors
!              nref, &          ! number of lines with reference
!              ntitle, &        ! number of lines with title
!              reference_xc5, & ! reference of paper
!              title_xc5        ! title of paper
!
! *** Declaration of local data
!
  implicit none
  character(len=200) :: rstring    !
  character(len=1000) :: string    !
  integer            :: i          ! counter
!
! *************************** Write bibliography ***********************
!
  write(1, '("# reference:")')
  string = '#   author:'
  do i = 0, (nauthors - 1) / 3
    rstring = trim(authors_xc5(3 * i + 1)) //" " // trim(authors_xc5(3 * i + 2)) //" " // trim(authors_xc5(3 * i + 3))
    string = trim(string) // ' ' // rstring
  enddo
  write(1, '(a)') trim(string)
  string = '#   title:'
  do i = 0, (ntitle - 1) / 3
    rstring = trim(title_xc5(3 * i + 1)) //" " // trim(title_xc5(3 * i + 2)) //" " // trim(title_xc5(3 * i + 3))
    string = trim(string) // ' ' // rstring
  enddo
  write(1, '(a)') trim(string)
  string = '#   journal:'
  do i = 0, (nref - 1) / 3
    rstring = trim(reference_xc5(3 * i + 1)) //" " // trim(reference_xc5(3 * i + 2)) //" " // trim(reference_xc5(3 * i + 3))
    string = trim(string) // ' ' // rstring
  enddo
  write(1, '(a)') trim(string)
  return
end subroutine writeref
! Copyright A.J. Koning 2019
