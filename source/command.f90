      subroutine command(cmd)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Linux command
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Declaration of local data
!
      implicit none
      character(len=*) :: cmd
      integer          :: isys
      integer          :: system
!
! Linux command
!
      isys = system(cmd)
!     call execute_command_line(cmd)
      return
      end
! Copyright A.J. Koning 2019
