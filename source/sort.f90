subroutine sort(fname, L, type, columnB, columnE, skipB, skipE)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Sort file for specific column
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
! use A1_error_handling_mod, only: & ! Error handling
!                      read_error ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  integer, parameter :: numlines=150000    ! number of lines
  character(len=1)   :: type               ! type of variable
  character(len=80)  :: fname              ! file to be sorted
  character(len=300) :: string(numlines)   ! string
  character(len=300) :: strB(numlines)     ! string
  character(len=300) :: stringtmp          ! help variable
  character(len=132) :: c(numlines)        ! variable
  character(len=132) :: ctmp               ! variable
  integer            :: columnB            ! begin of column to sort
  integer            :: columnE            ! end of column to sort
  integer            :: skipB              ! skip first skipB lines for sorting
  integer            :: skipE              ! skip last skipE lines for sorting
  integer            :: i                  ! counter
  integer            :: iA                 ! begin variable
  integer            :: iB                 ! end variable
  integer            :: j                  ! counter
  integer            :: m                  ! counter
  integer            :: L                  ! length of filename
  integer            :: N                  ! number of lines to sort
  integer            :: istat              ! logical for file access
  integer            :: k(numlines)        ! variable
  integer            :: ktmp               ! help variable
  real               :: x(numlines)        ! variable
  real               :: xtmp               ! help variable
!
! ***************************** Read file *****************************
!
  string = ' '
  strB = ' '
  open(unit=22, file = fname(1:L), status = 'unknown', iostat = istat)
  if (istat /= 0) call read_error(fname(1:L), istat)
  i = 1
  do
    read(22, '(a)', iostat = istat) string(i)
    if (istat == -1) exit
    i = i + 1
  enddo
  N = i - 1
  close(22)
  iA = 1 + skipB
  iB = N - skipE
  if (iB <= iA) return
!
! ***************************** Read variables *****************************
!
  if (type == 'I') then
    do i = iA, iB
      read(string(i)(columnB:columnE),*) k(i)
    enddo
  endif
  if (type == 'R') then
    do i = iA, iB
      read(string(i)(columnB:columnE),*) x(i)
    enddo
  endif
  if (type == 'C') then
    do i = iA, iB
      read(string(i)(columnB:columnE),'(a)') c(i)
    enddo
  endif
!
! ***************************** Sorting *****************************
!
  if (type == 'I') then
    do i = iA, iB
      do j = i, iB
        if (k(i) < k(j)) cycle
        stringtmp = string(i)
        ktmp = k(i)
        string(i) = string(j)
        k(i) = k(j)
        string(j) = stringtmp
        k(j) = ktmp
      enddo
    enddo
  endif
  if (type == 'R') then
    do i = iA, iB
      do j = i, iB
        if (x(i) < x(j)) cycle
        stringtmp = string(i)
        xtmp = x(i)
        string(i) = string(j)
        x(i) = x(j)
        string(j) = stringtmp
        x(j) = xtmp
      enddo
    enddo
  endif
  if (type == 'C') then
    do i = iA, iB
      do j = i, iB
        if (c(i) < c(j)) cycle
        stringtmp = string(i)
        ctmp = c(i)
        string(i) = string(j)
        c(i) = c(j)
        string(j) = stringtmp
        c(j) = ctmp
      enddo
    enddo
  endif
!
! ***************************** Remove duplications ********************
!
  m = 1
  strB(1) = string(1)
  do i = 2, N
    if (string(i) /= string(i-1)) then
      m = m + 1
      strB(m) = string(i)
    endif
  enddo
  N = m
  string = strB
!
! ***************************** Write file *****************************
!
  open(unit=22, file = fname(1:L), status = 'unknown', iostat = istat)
  do i = 1, N
    write(22, '(a)', iostat = istat) trim(string(i))
  enddo
  close(22)
end subroutine sort
! Copyright A.J. Koning 2019
