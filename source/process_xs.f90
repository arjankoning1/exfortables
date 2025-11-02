subroutine process_xs(ptype, nuclide, list)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process cross sections
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
  character(len=1)   :: ptype      ! particle type
  character(len=6)   :: nuclide    ! nuclide
  character(len=9)   :: subentry   ! subentry
  character(len=60)  :: reaction   ! reaction 
  character(len=132) :: list       ! filename
  integer            :: istat      ! error code
  integer            :: MF         ! MF number
  integer            :: MF_file    ! MF number
  integer            :: MT         ! MT number
  integer            :: MT_file    ! MT number
!
! ************* Cross sections ********************
!
! 1. Total cross sections
!
  MF = 3
  do MT = 1, 3
    open (unit = 2, status = 'old', file = trim(list))
    do
      read(2,'(a9,1x,2i6,1x,a60)', iostat=istat) subentry, MF_file, MT_file, reaction
      if (istat == -1) exit
      if (MF == MF_file .and. MT == MT_file) call processsubentry(ptype, nuclide, subentry)
    enddo
    close(2)
  enddo
!
! 2. Exclusive cross sections
!
  MF = 3
  do MT = 4, 200
    if (MT >= 5 .and. MT <= 10) cycle
    if (MT >= 50 .and. MT <= 90) cycle
    open (unit = 2, status = 'old', file = trim(list))
    do
      read(2,'(a9,1x,2i6,1x,a60)', iostat=istat) subentry, MF_file, MT_file, reaction
      if (istat == -1) exit
      if (MF == MF_file .and. MT == MT_file) call processsubentry(ptype, nuclide, subentry)
    enddo
    close(2)
  enddo
!
! 3. Level cross sections
!
  do MT = 50, 848
    if (MT >= 90 .and. MT < 600) cycle
    open (unit = 2, status = 'old', file = trim(list))
    do
      read(2,'(a9,1x,2i6,1x,a60)', iostat=istat) subentry, MF_file, MT_file, reaction
      if (istat == -1) exit
      if (MF == MF_file .and. MT == MT_file) call processsubentry(ptype, nuclide, subentry)
    enddo
    close(2)
  enddo
!
! 4. Total particle production cross section
!
  do MT = 201, 207
    open (unit = 2, status = 'old', file = trim(list))
    do
      read(2,'(a9,1x,2i6,1x,a60)', iostat=istat) subentry, MF_file, MT_file, reaction
      if (istat == -1) exit
      if (MF == MF_file .and. MT == MT_file) call processsubentry(ptype, nuclide, subentry)
    enddo
    close(2)
  enddo
!
! 5. Residual production cross sections
!
  do MT = 9000, 9001
    open (unit = 2, status = 'old', file = trim(list))
    do
      read(2,'(a9,1x,2i6,1x,a60)', iostat=istat) subentry, MF_file, MT_file, reaction
      if (istat == -1) exit
      if (MF == MF_file .and. MT == MT_file) call processsubentry(ptype, nuclide, subentry)
    enddo
    close(2)
  enddo
  return
end subroutine process_xs
! Copyright A.J. Koning 2021
