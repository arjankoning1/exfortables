subroutine generalinfo
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: General info from C4 and C5 databases
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! *** Declaration of local data
!
  implicit none
  integer            :: i          ! counter
  integer            :: istat      ! error code
  character(len=132) :: listfile   ! filename
  character(len=132) :: infofile   ! filename
  character(len=132) :: line       ! line
!
! Read general information from C5 or C4 files
!
  if (flagxc5) then
    listfile = trim(filespath)//'C5sub/xc5.list'
    infofile = trim(filespath)//'C5sub/xc5.info'
    open (unit = 2, status = 'old', file = infofile)
    do
      read(2, '(a)', iostat = istat) line
      if (istat == -1) exit
      if (line(1:12) == '#C5.2.1     ') then
        read(line(13:132), * ) date_xc5_nrdc, time_xc5_nrdc
        cycle
      endif
      if (line(1:12) == '#/C5.2.1    ') then
        read(line(13:132), * ) Nentry_xc5_nrdc, Nsub_xc5_nrdc, Nsub_x4_nrdc, i, Npointstot_xc5_nrdc
        exit
      endif
    enddo
    close(unit = 2)
  else
    listfile = trim(filespath)//'C4all/xc4.list'
    infofile = trim(filespath)//'C4all/xc4.info'
    open (unit = 2, status = 'old', file = infofile)
    do
      read(2, '(a)', iostat = istat) line
      if (istat == -1) exit
      if (line(1:12) == '#C4REQUEST  ') then
        read(line(13:132), * ) date_xc5_nrdc, time_xc5_nrdc, date_x4_nrdc
        cycle
      endif
      if (line(1:12) == '#/C4REQUEST ') then
        read(line(13:132), * ) Nentry_xc5_nrdc, Nsub_xc5_nrdc, Nsub_x4_nrdc, i, Npointstot_xc5_nrdc
        exit
      endif
    enddo
    close(unit = 2)
  endif
  return
end subroutine generalinfo
! Copyright A.J. Koning 2021
