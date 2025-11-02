subroutine statistics
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Statistics of EXFOR analysis and TALYS+library comparison
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
!              flagunc        ! flag for uncertainty analysis
!
! *** Declaration of local data
!
  implicit none
!
! **************** Initialization of constants and arrays **************
!
! totalstat  : subroutine for total statistics of all goodness-of-fit estimators
! fout       : subroutine for output of F values
! readquality: subroutine to initialize quality information
! compare    : subroutine for comparison of experimental data with TALYS and libraries
! comparesun : subroutine to process statistics of all goodness-of-fit estimators
! quality    : subroutine for quality information
! uncout     : subroutine for uncertainty analysis
!
  call statinitial
  call readquality
  call compare
  call totalstat
  call fout
  call entries
  if (flagunc) call uncout
  return
end subroutine statistics
! Copyright A.J. Koning 2019
