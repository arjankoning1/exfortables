subroutine mainout
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Main output
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     2025-12-18   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A0_kinds_mod, only: & ! Definition of single and double precision variables
!              sgl                    ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              date_x4_nrdc, &        ! date according to NDRC file
!              date_xc5_nrdc, &       ! date according to NDRC file
!              flagtables, &         ! flag for new database
!              flagstat, &            ! flag for statistics
!              flagx4, &              ! flag to use original EXFOR information
!              Nentry_xc5, &          ! number of entries according to NRDC file
!              Nentry_xc5_nrdc, &     ! number of entries according to NRDC file
!              Npointstot_new, &      ! total number of points in new database
!              Npointstot_tal, &      ! number of points for TALYS comparison
!              Npointstot_xc5, &      ! total number of points
!              Npointstot_xc5_nrdc, & ! number of total points according to NDRC file
!              Nsub_new, &            ! counter for new subentries
!              Nsub_tal, &            ! number of subentries for TALYS comparison
!              Nsub_x4_nrdc, &        ! number of subentries according to NRDC file
!              Nsub_xc5, &            ! counter for XC5 subentries
!              Nsub_xc5_nrdc, &       ! number of subentries according to NRDC file
!              time_xc5_nrdc          ! time according to NDRC file
!
! *** Declaration of local data
!
  implicit none
  character(len=11) :: subdir      !
  real(sgl)         :: Rnewxc5P    !
  real(sgl)         :: Rnewxc5S    !
  real(sgl)         :: RtalnewP    !
  real(sgl)         :: RtalnewS    !
!
! *************************** Code and version *************************
!
  subdir = 'stat/total/'
  open (unit = 2, status = 'unknown', file = subdir//'statistics')
  write(2, '(/"    EXFORTABLES-2.2    (Version: December 18, 2025)"/)')
  write(2, '(" Copyright (C) 2025  A.J. Koning"/)')
!
! ***************************** Write main statistics ******************
!
  if (flagtables) then
    if (Nsub_xc5 > 0) then
      Rnewxc5S = 100. * real(Nsub_new) / Nsub_xc5
      Rnewxc5P = 100. * real(Npointstot_new) / Npointstot_xc5
    else
      Rnewxc5S = 0.
      Rnewxc5P = 0.
    endif
    write(2, '(/"         EXFORTABLES Statistics"/)')
    write(2, '(" XC5 subentries read: ", i9)') Nsub_xc5
    write(2, '(" XC5 data points read: ", i9)') Npointstot_xc5
    write(2, '(" EXFORTABLES subentries: ",i9, " (", f5.1, "%) ")') Nsub_new, Rnewxc5S
    write(2, '(" EXFORTABLES data points: ",i9, " (", f5.1, "%) ")') Npointstot_new, Rnewxc5P
  endif
  if (flagstat) then
    if (Nsub_new > 0) then
      RtalnewS = 100. * real(Nsub_tal) / Nsub_new
      RtalnewP = 100. * real(Npointstot_tal) / Npointstot_new
    else
      RtalnewS = 0.
      RtalnewP = 0.
    endif
    write(2, '(" Libraries subentries: ", i9, " (", f5.1, "%) ")') Nsub_tal, RtalnewS
    write(2, '(" Libraries data points: ", i9, " (", f5.1, "%) ")') Npointstot_tal, RtalnewP
  endif
  close (2)
  return
end subroutine mainout
! Copyright A.J. Koning 2025
