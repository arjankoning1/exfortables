subroutine process_respar(ptype, nuclide, list)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process resonance parameters
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
  integer            :: MT_file    ! MT number
!
! ************* Resonance parameters ********************
!
  do MF = 401, 405
    open (unit = 2, status = 'old', file = trim(list))
    do
      read(2,'(a9,1x,2i6,1x,a60)', iostat=istat) subentry, MF_file, MT_file, reaction
      if (istat == -1) exit
      if (MF == MF_file) call processsubentry(ptype, nuclide, subentry)
    enddo
    close(2)
  enddo
  return
end subroutine process_respar
! Copyright A.J. Koning 2021
