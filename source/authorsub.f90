subroutine authorsub
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Beautify author name
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
!              author1_xc5, & ! author
!              author2, &     ! author name
!              authorcode, &  ! author
!              authordone     ! flag to check whether this entry is already covered
!
! *** Declaration of local data
!
  implicit none
  logical           :: first     !
  character(len=25) :: auth      !
  character(len=29) :: code      !
  integer           :: ii        ! counter
  integer           :: jj        !
  integer           :: k         ! counter
  integer           :: kbeg      !
  integer           :: kend      !
  integer           :: kk        !
!
! * Beautify author name, to be put in filename with experimental data *
!
! Extract author from total author string
!
  if (authordone) return
  auth = author1_xc5
  if (auth(2:4) == '   ') auth(1:3) = 'XXX'
  code = '                             '
  do ii = 25, 2, - 1
    if ((auth(ii:ii) == ' ' .or. auth(ii:ii) == '+') .and. ((auth(ii-1:ii-1) /= ' ' .and. auth(ii-1:ii-1) /= '+'))) then
      kend = ii - 1
      first = .true.
      do jj = kend, 1, - 1
        if (auth(jj:jj) == '.' .or. jj == 1) then
          if (jj == 1) then
            kbeg = 1
          else
            kbeg = jj + 1
          endif
          code(1:kend - kbeg + 1) = auth(kbeg:kend)
!
! Remove blanks and strange characters from author names
!
          do k = 1, kend - kbeg + 1
            if ( .not. ((code(k:k) >= 'a' .and. code(k:k) <= 'z') .or. (code(k:k) >= 'A' .and. code(k:k) <= 'Z'))) then
              do kk = k + 1, kend
                code(kk - 1:kk - 1) = code(kk:kk)
              enddo
            endif
          enddo
          if (first) then
            author2 = '                         '
            author2 = trim(code)
            first = .false.
          endif
          authorcode = trim(code)
          authordone = .true.
          return
        endif
      enddo
    endif
  enddo
  return
end subroutine authorsub
! Copyright A.J. Koning 2019
