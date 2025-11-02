subroutine openfiles
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: open files
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
  character*20 specialfile
!
! ********** Open files for statistics and thermal cross sections etc. **********
!
  open (unit = 4, status = 'unknown', file = 'stat/total/xc5toexfortables.no')
  open (unit = 9, status = 'unknown', file = 'stat/total/xc5toexfortables.yes')
  open (unit = 14, status = 'unknown', file = 'stat/total/compare.no')
  open (unit = 19, status = 'unknown', file = 'stat/total/compare.yes')
  open (unit = 17, status = 'unknown', file = 'stat/total/warnings')
  open (unit = 18, status = 'unknown', file = 'stat/total/subentries')
  if (parinclude(1)) then
    specialfile = 'special/exfor_'
    open (unit = 41, status = 'unknown', file = trim(specialfile)//'thermal_tot.txt')
    open (unit = 42, status = 'unknown', file = trim(specialfile)//'thermal_el.txt')
    open (unit = 43, status = 'unknown', file = trim(specialfile)//'thermal_nf.txt')
    open (unit = 44, status = 'unknown', file = trim(specialfile)//'thermal_ng.txt')
    open (unit = 45, status = 'unknown', file = trim(specialfile)//'thermal_np.txt')
    open (unit = 46, status = 'unknown', file = trim(specialfile)//'thermal_na.txt')
    open (unit = 47, status = 'unknown', file = trim(specialfile)//'thermal_nu.txt')
    open (unit = 48, status = 'unknown', file = trim(specialfile)//'thermal_nud.txt')
    open (unit = 49, status = 'unknown', file = trim(specialfile)//'thermal_nup.txt')
    open (unit = 50, status = 'unknown', file = trim(specialfile)//'30keV.txt')
    open (unit = 51, status = 'unknown', file = trim(specialfile)//'thermal_av_tot.txt')
    open (unit = 52, status = 'unknown', file = trim(specialfile)//'thermal_av_el.txt')
    open (unit = 53, status = 'unknown', file = trim(specialfile)//'thermal_av_nf.txt')
    open (unit = 54, status = 'unknown', file = trim(specialfile)//'thermal_av_ng.txt')
    open (unit = 55, status = 'unknown', file = trim(specialfile)//'thermal_av_np.txt')
    open (unit = 56, status = 'unknown', file = trim(specialfile)//'thermal_av_na.txt')
    open (unit = 57, status = 'unknown', file = trim(specialfile)//'thermal_av_nu.txt')
    open (unit = 58, status = 'unknown', file = trim(specialfile)//'thermal_av_nud.txt')
    open (unit = 59, status = 'unknown', file = trim(specialfile)//'thermal_av_nup.txt')
    open (unit = 60, status = 'unknown', file = trim(specialfile)//'30keV_av.txt')
    open (unit = 61, status = 'unknown', file = trim(specialfile)//'Ig.txt')
    open (unit = 62, status = 'unknown', file = trim(specialfile)//'If.txt')
    open (unit = 63, status = 'unknown', file = trim(specialfile)//'gamgam.txt')
    open (unit = 64, status = 'unknown', file = trim(specialfile)//'D0.txt')
  endif
  return
end subroutine openfiles
! Copyright A.J. Koning 2021
