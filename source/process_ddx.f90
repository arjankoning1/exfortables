subroutine process_ddx(ptype, nuclide, list)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process (double-differential) spectra
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
  integer            :: MTbeg(4)   ! begin MT number
  integer            :: MTend(4)   ! end MT number
  integer            :: i          ! counter
!
! ************* Emission spectra *************************
!
  MTbeg(1) = 4
  MTend(1) = 4
  MTbeg(2) = 102
  MTend(2) = 107
  MTbeg(3) = 201
  MTend(3) = 207
  MTbeg(4) = 9000
  MTend(4) = 9000
  do MF = 5, 6
    do i= 1, 4
      do MT = MTbeg(i), MTend(i)
        open (unit = 2, status = 'old', file = trim(list))
        do
          read(2,'(a9,1x,2i6,1x,a60)', iostat=istat) subentry, MF_file, MT_file, reaction
          if (istat == -1) exit
          if (MF == MF_file .and. MT == MT_file) call processsubentry(ptype, nuclide, subentry)
        enddo
        close(2)
      enddo
    enddo
  enddo
  return
end subroutine process_ddx
! Copyright A.J. Koning 2021
