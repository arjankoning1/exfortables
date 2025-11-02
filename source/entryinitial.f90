subroutine entryinitial
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialization of variables per subentry
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
!              author1_xc5, &         ! author
!              authors_xc5, &         ! authors of paper
!              entry_xc5, &           ! entry
!              nauthors, &            ! number of lines with authors
!              nref, &                ! number of lines with reference
!              ntitle, &              ! number of lines with title
!              reference_xc5, &       ! reference of paper
!              title_xc5, &           ! title of paper
!              year_xc5               ! year
!
! *** Declaration of local data
!
  implicit none
!
! ********************** Initialization **********************
!
! readsub
!
  author1_xc5 = '                         '
  year_xc5 = 0
  title_xc5 = ' '
  ntitle = 0
  authors_xc5 = ' '
  nauthors = 0
  reference_xc5 = ' '
  nref = 0
  return
end subroutine entryinitial
! Copyright A.J. Koning 2019
