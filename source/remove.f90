subroutine remove
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Remove previous results
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
!              flagout, &     ! flag for main output
!              flagtables, &
!              flagstat, &
!              flagunc, &     ! flag for uncertainty analysis
!              parinclude, &  ! flag to include particle
!              partype        ! symbol of particle
!
! *** Declaration of local data
!
  implicit none
  character(len=80) :: cmd       ! command
  integer           :: i         ! counter
!
! Cleanup of previous results
!
  write(*, *) "Removing directories from previous run....."
  if (flagtables) then
    do i = 0, 7
      cmd = 'rm -rf'
      if (parinclude(i)) then
        cmd=trim(cmd)// ' ' // partype(i)
        call command(cmd)
      endif
    enddo
    cmd = 'rm -rf fy'
    call command(cmd)
    if (parinclude(1)) then
      cmd = 'rm -rf special'
      call command(cmd)
      cmd = 'mkdir special'
      call command(cmd)
    endif
  endif
  cmd = 'rm -rf stat; mkdir stat                                     '
  call command(cmd)
  cmd = 'mkdir stat/total                                            '
  call command(cmd)
  cmd = 'mkdir stat/json                                             '
  call command(cmd)
  if (flagstat) then
    cmd = 'mkdir stat/comp                                           '
    call command(cmd)
    cmd = 'mkdir stat/comp/MT stat/comp/reaction stat/comp/entry     '
    call command(cmd)
    cmd = 'mkdir stat/comp/nucMT stat/comp/histo stat/comp/total     '
    call command(cmd)
    cmd = 'mkdir stat/comp/nucE stat/comp/MTE stat/comp/parE        '
    call command(cmd)
    cmd = 'mkdir stat/comp/MTA stat/comp/lists '
    call command(cmd)
    if (flagout) then
      cmd = 'mkdir stat/comp/pdist '
      call command(cmd)
    endif
    if (flagunc) then
      cmd = 'mkdir stat/unc                                            '
      call command(cmd)
      cmd = 'mkdir stat/unc/MT stat/unc/histo stat/unc/total           '
      call command(cmd)
    endif
    cmd = 'rm -rf quality; mkdir quality                               '
    call command(cmd)
  endif
  return
end subroutine remove
! Copyright A.J. Koning 2019
