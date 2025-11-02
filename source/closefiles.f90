subroutine closefiles
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Close files
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
!
! ********** Close files for statistics and thermal cross sections etc. **********
!
  close (4)
  close (9)
  close (14)
  close (17)
  close (18)
  close (19)
  if (parinclude(1)) then
    close (41)
    close (42)
    close (43)
    close (44)
    close (45)
    close (46)
    close (47)
    close (48)
    close (49)
    close (50)
    close (51)
    close (52)
    close (53)
    close (54)
    close (55)
    close (56)
    close (57)
    close (58)
    close (59)
    close (60)
    close (61)
    close (62)
    close (63)
    close (64)
  endif
  return
end subroutine closefiles
! Copyright A.J. Koning 2021
