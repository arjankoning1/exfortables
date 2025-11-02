subroutine checkkeyword
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Check for errors in keywords
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     2025-10-25   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A1_exfortables_mod, only: & ! All global variables
!              flagtalys, &   ! flag for TALYS comparison
!              inline, &      ! input line
!              libspath, &    ! directory containing data libraries
!              nlines, &      ! number of input lines
!              talyspath      ! directory containing TALYS results
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist            ! logical for existence
  integer, parameter :: numkey=46         ! number of keywords
  character(len=132) :: keyword(numkey)   ! keyword
  character(len=132) :: word(40)          ! words on input line
  character(len=132) :: key               ! keyword
  character(len=132) :: libfile           ! file name
  character(len=132) :: talysfile         ! file name
  integer            :: i                 ! counter
  integer            :: j                 ! counter
!
! Although it is difficult to prevent the user from all possible input errors, we can check for the use of wrong keywords
! and for unphysical values for most of the input variables.
!
! *********************** Check for wrong keywords *********************
!
! EXFORTABLES will stop if a keyword is incorrect
!
  data (keyword(i), i = 1, numkey) / ' ', 'amin', 'amax', 'cendl', 'dexp', 'eaf', 'emin', 'emax', 'endfb', 'erf', &
 &  'expo', 'eview', 'filespath', 'fmax', ' format', 'group', 'irdff', 'jeff', 'jendl', 'lib', 'libspath', 'mt', 'maxentry', &
 &  'outliers', 'outprocess', 'particle', 'pointcomp', 'qualitycomp', 'reacstyle', 'remove', 'source', 'statistics', 'tables', &
 &  'talys', 'talysemin', 'talysemax', 'talyspath', 'tendl',  'uncertainty', 'user', 'x4', 'xc5', 'xseps', 'xsonly', 'zmin', &
 &  'zmax'/
!
! A keyword can be de-activated by putting a # in front of it.
! All first words of the input lines are checked against the list of keywords.
!
! getkeywords: subroutine to retrieve keywords and values from input line
!
! The keyword is identified.
!
Loop1:  do i = 1, nlines
    call getkeywords(inline(i), word)
    key = word(1)
    if (key(1:1) == '#') cycle
    do j = 1, numkey
      if (keyword(j) == key) cycle Loop1
    enddo
    write(*, '(/" EXFORTABLES-error: Wrong keyword: ", a20)') key
    stop
  enddo Loop1
  libfile = trim(libspath) // 'n/Al027/endfb8.1/tables/xs/n-Al027-MT016.endfb8.1'
  inquire (file = libfile, exist = lexist)
  if ( .not. lexist) then
    write(*, '(" EXFORTABLES-warning: Non-existent library ", a132)') libfile
    return
  endif
  if (flagtalys) then
    talysfile = trim(talyspath)//'n/Al027/tables/xs/xs200000.tot'
    inquire (file = talysfile, exist = lexist)
    if ( .not. lexist) then
      write(*, '(" EXFORTABLES-warning: Non-existent TALYS file ", a132)') talysfile
      return
    endif
  endif
  return
end subroutine checkkeyword
! Copyright A.J. Koning 2019
