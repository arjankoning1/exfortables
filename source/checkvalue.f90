subroutine checkvalue
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Check for errors in values
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
  use A1_error_handling_mod
!
! use A1_exfortables_mod, only: & ! All global variables
!              allemax, &     ! maximum energy (MeV) for TALYS or library comparison
!              allemin, &     ! minimum energy (MeV) for TALYS or library comparison
!              Amax, &        ! maximal A value to process
!              Amin, &        ! minimal A value to process
!              Fmax, &        ! maximum F value
!              maxentry, &    ! maximum number of X4 entries to be processed
!              numlib, &      ! maximum number of data libraries
!              numlib, &      ! maximum number of data libraries
!              parinclude, &  ! flag to include particle
!              pointcomp, &   ! reference for pointwise comparison
!              pointref, &    ! reference for pointwise comparison
!              qualitycomp, & ! reference for quality assignment
!              qualityref, &  ! reference for quality assignment
!              talysemax, &   ! maximum energy (MeV) for TALYS comparison
!              talysemin, &   ! minimum energy (MeV) for TALYS comparison
!              xseps, &       ! minimum cross section (mb) for TALYS + library comparison
!              Zmax, &        ! maximal Z value to process
!              Zmin           ! minimal Z value to process
! use A1_error_handling_mod, only: & ! Error handling
!          range_integer_error, & ! Test if integer variable is out of range
!          range_real_error    ! Test if real variable is out of range
!
! *** Declaration of local data
!
  implicit none
  character(len=5) :: reflib(0:numlib)     ! reference library
  integer          :: i                    ! counter
  integer          :: ipo                  ! counter
!
! All parameters need to fall within certain ranges.
! These ranges are specified in this subroutine and in the manual.
!
! ******************* Check for wrong input variables ******************
!
  call range_integer_error('maxentry', maxentry, 1, 1000000000)
  call range_integer_error('Zmin', Zmin, 0, 150)
  call range_integer_error('Zmax', Zmax, 0, 150)
  call range_integer_error('Zmin', Zmin, 0, Zmax)
  call range_integer_error('Amin', Amin, -1, 400)
  call range_integer_error('Amax', Amax, -1, 400)
  call range_integer_error('Amin', Amin, -1, Amax)
  ipo = 0
  do i = 0, 7
    if (parinclude(i)) ipo = ipo + 1
  enddo
  if (ipo == 0) then
    write(*, '(" EXFORTABLES-error: incorrect particle keyword")')
    stop
  endif
  call range_real_error('xseps', xseps, 1.e-10, 1000.)
  call range_real_error('Fmax', Fmax, 1., 1.e38)
  call range_real_error('talysemin', talysemin, 0., 1000.)
  call range_real_error('talysemax', talysemax, 0., 1000.)
  call range_real_error('talysemin', talysemin, 0., talysemax)
  call range_real_error('allemin', allemin, 0., 1000.)
  call range_real_error('allemax', allemax, 0., 1000.)
  call range_real_error('allemin', allemin, 0., allemax)
  reflib(0) = 'all'
  reflib(1) = 'talys'
  reflib(2) = 'tendl'
  reflib(3) = 'endfb'
  reflib(4) = 'jeff'
  reflib(5) = 'jendl'
  reflib(6) = 'eaf'
  reflib(7) = 'cendl'
  reflib(8) = 'irdff'
  pointref = -1
  do i = 0, numlib
    if (trim(pointcomp) == trim(reflib(i))) pointref = i
  enddo
  if (pointref == -1) then
    write(*, '(" EXFORTABLES-error: pointcomp should be equal to one of", 9(1x, a5))') (reflib(i), i = 0, 8)
    stop
  endif
  qualityref = -1
  do i = 0, numlib
    if (qualitycomp == reflib(i)) qualityref = i
  enddo
  if (qualityref == -1) then
    write(*, '(" EXFORTABLES-error: qualitycomp should be equal to one of", 9(1x, a5))') (reflib(i), i = 0, 8)
    stop
  endif
  return
end subroutine checkvalue
! Copyright A.J. Koning 2019
