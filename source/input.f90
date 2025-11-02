subroutine input
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read input for keywords
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
!              filespath, &   ! directory containing X4 and structure files to be read
!              flagcendl, &   ! flag to include or exclude CENDL from library average
!              flagdexp, &    ! flag to use experimental uncertainty in F factor
!              flageaf, &     ! flag to include or exclude EAF from library average
!              flagerf, &     ! flag to use error function for Frms
!              flagendfb, &   ! flag to include or exclude ENDFB from library average
!              flageview, &   ! flag to make ECISVIEW file
!              flagexpo, &    ! flag to use exponential RMS instead of power of 10
!              flaggroup, &   ! flag to group resonance data
!              flagirdff, &   ! flag to include or exclude IRDFF from library average
!              flagjeff, &    ! flag to include or exclude JEFF from library average
!              flagjendl, &   ! flag to include or exclude JENDL from library average
!              flaglib, &     ! flag for library comparison
!              flagout, &     ! flag for main output
!              flagtables, &  ! flag for new database
!              flagremove, &  ! flag for removing existing database
!              flagstat, &    ! flag for statistics
!              flagtalys, &   ! flag for TALYS comparison
!              flagtendl, &   ! flag to include or exclude TENDL from library average
!              flagunc, &     ! flag for uncertainty analysis
!              flagx4, &      ! flag to use original EXFOR information
!              flagxc5, &     ! flag to use XC5 instead of XC4
!              flagxsonly, &  ! flag to process only cross sections
!              fmax, &        ! maximal F value per point taken into account
!              inline, &      ! input line
!              libspath, &    ! directory containing data libraries
!              maxentry, &    ! maximum number of X4 entries to be processed
!              mtinclude, &   ! flag to include MT number
!              Nlibs, &       ! number of data libraries for comparison
!              nlines, &      ! number of input lines
!              numlib, &      ! maximum number of data libraries
!              nummt, &       ! maximum number of MT numbers
!              parinclude, &  ! flag to include particle
!              partype, &     ! symbol of particle
!              pointcomp, &   ! reference for pointwise comparison
!              qualitycomp, & ! reference for quality assignment
!              talysemax, &   ! maximum energy (MeV) for TALYS comparison
!              talysemin, &   ! minimum energy (MeV) for TALYS comparison
!              talyspath, &   ! directory containing TALYS results
!              xseps, &       ! minimum cross section (mb) for TALYS + library comparison
!              Zmax, &        ! maximal Z value to process
!              Zmin           ! minimal Z value to process
! use A1_error_handling_mod, only: & ! Error handling
!                      read_error ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  logical            :: flagmtinput       ! flag to include MT number
  logical            :: mtinput(nummt)    ! flag to include MT number
  character(len=1)   :: ch                ! character
  character(len=132) :: key               ! keyword
  character(len=132) :: val               ! value or string
  character(len=132) :: word(40)          ! words on input line
  character(len=132) :: line              ! input line
  integer            :: i                 ! counter
  integer            :: i2                ! counter
  integer            :: ix                ! index
  integer            :: istat             ! logical for file access
  integer            :: lenfile           ! length of string
  integer            :: lenlib            ! length of string
  integer            :: lental            ! length of string
  integer            :: mt                ! mt number
  integer            :: type              ! particle type
!
! ********************************* Defaults ***************************
!
  flagx4 = .true.
  flagxc5 = .true.
  flagtalys = .true.
  flaglib = .false.
  flagendfb = .true.
  flagjendl = .true.
  flagjeff = .true.
  flagcendl = .true.
  flageaf = .true.
  flagirdff = .true.
  flagtendl = .true.
  flagunc = .true.
  flagremove = .true.
  flagtables = .true.
  flagstat = .true.
  flageview = .false.
  flagexpo = .true.
  flagxsonly = .false.
  flaggroup = .true.
  flagout = .false.
  flagdexp = .true.
  flagerf = .true.
  flagoutliers = .true.
  flagreacstyle = .false.
  Zmin = 0
  Zmax = 150
  Amin = -1
  Amax = 400
  pointcomp = 'talys'
  qualitycomp = 'all'
  parinclude = .true.
  flagmtinput = .false.
  mtinclude = .true.
  mtinput = .false.
  maxentry = 1000000000
  xseps = 0.1
  Fmax = 1.e38
  talysemin = 0.001
  talysemax = 1000.
  allemin = 0.
  allemax = 1000.
  source = 'EXFOR'
  oformat = 'YANDF-0.4'
!
! ************************ Read input variables ************************
!
! getkeywords: subroutine to retrieve keywords and values from input line
!
! The keyword is identified and the corresponding values are read.
! Erroneous input is immediately checked.
! The keywords and number of values on each line are retrieved from the input.
!
  do i = 1, nlines
    line = inline(i)
    call getkeywords(inline(i), word)
    key = word(1)
    val = word(2)
    ch = word(2)(1:1)
!
! Test for keywords
!
    if (key == 'x4') then
      if (ch == 'n') flagx4 = .false.
      if (ch == 'y') flagx4 = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'xc5') then
      if (ch == 'n') flagxc5 = .false.
      if (ch == 'y') flagxc5 = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'talys') then
      if (ch == 'n') flagtalys = .false.
      if (ch == 'y') flagtalys = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'lib') then
      if (ch == 'n') flaglib = .false.
      if (ch == 'y') flaglib = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'endfb') then
      if (ch == 'n') flagendfb = .false.
      if (ch == 'y') flagendfb = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'jendl') then
      if (ch == 'n') flagjendl = .false.
      if (ch == 'y') flagjendl = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'jeff') then
      if (ch == 'n') flagjeff = .false.
      if (ch == 'y') flagjeff = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'cendl') then
      if (ch == 'n') flagcendl = .false.
      if (ch == 'y') flagcendl = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'eaf') then
      if (ch == 'n') flageaf = .false.
      if (ch == 'y') flageaf = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'irdff') then
      if (ch == 'n') flagirdff = .false.
      if (ch == 'y') flagirdff = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'tendl') then
      if (ch == 'n') flagtendl = .false.
      if (ch == 'y') flagtendl = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'uncertainty') then
      if (ch == 'n') flagunc = .false.
      if (ch == 'y') flagunc = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'remove') then
      if (ch == 'n') flagremove = .false.
      if (ch == 'y') flagremove = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'tables') then
      if (ch == 'n') flagtables = .false.
      if (ch == 'y') flagtables = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'statistics') then
      if (ch == 'n') flagstat = .false.
      if (ch == 'y') flagstat = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'eview') then
      if (ch == 'n') flageview = .false.
      if (ch == 'y') flageview = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'expo') then
      if (ch == 'n') flagexpo = .false.
      if (ch == 'y') flagexpo = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'xsonly') then
      if (ch == 'n') flagxsonly = .false.
      if (ch == 'y') flagxsonly = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'group') then
      if (ch == 'n') flaggroup = .false.
      if (ch == 'y') flaggroup = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'outprocess') then
      if (ch == 'n') flagout = .false.
      if (ch == 'y') flagout = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'dexp') then
      if (ch == 'n') flagdexp = .false.
      if (ch == 'y') flagdexp = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'erf') then
      if (ch == 'n') flagerf = .false.
      if (ch == 'y') flagerf = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'outliers') then
      if (ch == 'n') flagoutliers = .false.
      if (ch == 'y') flagoutliers = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'reacstyle') then
      if (ch == 'n') flagreacstyle = .false.
      if (ch == 'y') flagreacstyle = .true.
      if (ch /= 'y' .and. ch /= 'n') call read_error(line, istat)
      cycle
    endif
    if (key == 'zmin') then
      read(val, * , iostat = istat) Zmin
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'zmax') then
      read(val, * , iostat = istat) Zmax
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'amin') then
      read(val, * , iostat = istat) Amin
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'amax') then
      read(val, * , iostat = istat) Amax
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'pointcomp') then
      read(val, * , iostat = istat) pointcomp
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'qualitycomp') then
      read(val, * , iostat = istat) qualitycomp
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'particle') then
      parinclude = .false.
Loop1:  do i2 = 2, 40
        ch = word(i2)(1:1)
        do type = 0, 7
          if (ch == partype(type)) then
            parinclude(type) = .true.
            cycle Loop1
          endif
        enddo
      enddo Loop1
      cycle
    endif
    if (key == 'mt') then
      flagmtinput = .true.
      read(val, * , iostat = istat) mt
      if (istat /= 0) call read_error(line, istat)
      mtinput(mt) = .true.
      cycle
    endif
    if (key == 'maxentry') then
      read(val, * , iostat = istat) maxentry
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'talyspath') then
      talyspath = trim(val)
      lental = len_trim(talyspath)
      if (talyspath(lental:lental) /= '/') talyspath = trim(talyspath)//'/'
    endif
    if (key == 'filespath') then
      filespath = trim(val)
      lenfile = len_trim(filespath)
      if (filespath(lenfile:lenfile) /= '/') filespath = trim(filespath)//'/'
    endif
    if (key == 'libspath') then
      libspath = trim(val)
      lenlib = len_trim(libspath)
      if (libspath(lenlib:lenlib) /= '/') libspath = trim(libspath)//'/'
    endif
    if (key == 'xseps') then
      read(val, * , iostat = istat) xseps
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'fmax') then
      read(val, * , iostat = istat) fmax
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'talysemin') then
      read(val, * , iostat = istat) talysemin
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'talysemax') then
      read(val, * , iostat = istat) talysemax
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'emin') then
      read(val, * , iostat = istat) allemin
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'emax') then
      read(val, * , iostat = istat) allemax
      if (istat /= 0) call read_error(line, istat)
      cycle
    endif
    if (key == 'source') then
      ix=index(line,'source')+7
      source=trim(adjustl(line(ix:132)))
      cycle
    endif
    if (key == 'user') then
      ix=index(line,'user')+5
      user=trim(adjustl(line(ix:132)))
      cycle
    endif
    if (key == 'format') then
      ix=index(line,'format')+7
      oformat=trim(adjustl(line(ix:132)))
      cycle
    endif
  enddo
!
! Set flags derived from input
!
  if (flaglib) then
    Nlibs = numlib
  else
    if (flagtalys) then
      Nlibs = 1
    else
      Nlibs = 0
    endif
  endif
  if (flagmtinput) mtinclude = mtinput
  return
  stop
end subroutine input
! Copyright A.J. Koning 2019
