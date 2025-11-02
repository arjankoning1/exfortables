subroutine processsubentry(ptype,nuclide,subentry)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process subentry
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
!              filespath, &   ! directory containing X4 and structure files to be read
!              flagconv, &    ! flag for conversion to new database
!              flagMTequiv, & ! flag for equivalence of MT number and residual product
!              flagxs, &      ! flag for cross section
!              Nsub_new, &    ! counter for new subentries
!
! *** Declaration of local data
!
  implicit none
  integer            :: i          ! counter
  character(len=1)   :: ptype      ! particle type
  character(len=6)   :: nuclide    ! nuclide
  character(len=9)   :: subentry   ! subentry
  character(len=132) :: subfile    ! filename
!
! **************** Read data and process it into new database **********
!
! readsub    : subroutine to read XC5 subentry
! reactionsub: subroutine to check XC5 subentry and determine reaction type
! readdata   : subroutine to read XC5 data
! pathsub    : subroutine to determine path name
! warnsub    : subroutine to give warning for suspicious data set
! authorsub  : subroutine to beautify author name
! filesub    : subroutine to determine file name
! writesub   : subroutine to write subentry in new format
!
  if (flagxc5) then
    subfile = trim(filespath)//'C5reac/'//ptype//'/'//trim(nuclide)//'/'//trim(subentry)//'.xc5'
  else
    subfile = trim(filespath)//'C4all/'//trim(subentry)//'.xc5'
  endif
  open (unit = 11, status = 'old', file = trim(subfile))
  call readsub
  do 
    call reactionsub
    flagMTequiv= .false.
    do i = 1, 2
      if (flagconv) then
        call readdata
        call pathsub
        call warnsub
        call authorsub
        call filesub
      else
        Nsub_new = Nsub_new - 1
      endif
      call writesub
      if (flagxs) call MTequiv
      if (.not. flagMTequiv) exit
    enddo
    if (flagsubdone) exit
  enddo
  close (11)
  return
end subroutine processsubentry
! Copyright A.J. Koning 2021
