subroutine filesub
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Determine file name
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
!              A_xc5, &        ! mass number
!              angmax, &       ! maximum angle of subentry
!              angmin, &       ! minimum angle of subentry
!              authorcode, &   ! author
!              dEinc, &        ! uncertainty of incident energy
!              Einc, &         ! incident energy
!              Emax, &         ! maximum energy (MeV) of subentry
!              Emin, &         ! minimum energy (MeV) of subentry
!              flagddx, &      ! flag for DDX emission spectrum
!              flagdiscang, &  ! flag for discrete angular distribution
!              flagelang, &    ! flag for elastic angular distribution
!              flagfy, &       ! flag for fission yields
!              flaginelang, &  ! flag for inelastic angular distribution
!              flagMT, &       ! flag for MT cross section
!              flagnubar, &    ! flag for nubar
!              flagres, &      ! flag for residual production cross section
!              flagrespar, &   ! flag for resonance parameters
!              flagri, &       ! flag for resonance integral
!              flagspec, &     ! flag for emission spectrum
!              flagspecav, &   ! flag for spectrum average
!              isochar, &      ! character for isomer
!              isom_xc5, &     ! product isomer
!              iso_xc5, &      ! target isomer
!              k0_xc5, &       !
!              MTrp, &         ! MT number
!              MT_xc5, &       ! MT number
!              Npoints_xc5, &  ! number of points per subentry
!              nuclide_xc5, &  ! nuclide
!              path, &         ! path
!              proj, &         ! projectile
!              reacid, &       ! reaction identifier
!              subentry_xc5, & ! XC5 subentry number
!              valstring, &    ! XC5 string with values
!              xsfile, &       ! cross section file
!              xspath, &       ! path of cross section
!              year_xc5, &     ! year
!              Z_xc5, &        ! charge number
!              ZAexist, &
!              ZAresexist, &   ! logical for Z, A, rp existence
!              ZAres           ! ZA of residual nucleus
!
! *** Declaration of local data
!
  implicit none
  logical           :: lexist       !
  character(len=3)  :: MTtype       !
  character(len=4)  :: MTstr        !
  character(len=4)  :: extension    !
  character(len=4)  :: ystr         !
  character(len=7)  :: ZAstr        !
  character(len=7)  :: strtype      !
  character(len=10) :: Estring      !
  character(len=10) :: Rstring      !
  character(len=10) :: datatype     !
  character(len=29) :: code         !
  character(len=80) :: filename     !
  character(len=80) :: nucpath      !
  character(len=80) :: exfile       !
  character(len=80) :: setlist      !
  character(len=80) :: xslist       !
  character(len=80) :: ZAlist       !
  character(len=80) :: ZAMTlist     !
  integer           :: i            ! counter
  integer           :: k            ! counter
  integer           :: kend         !
  integer           :: istat        !
  real              :: xmin
  real              :: xmax
!
! ************************* Create file name for reaction **************
!
! Create entire filename
!
  code = authorcode
  ystr = '    '
  write(ystr(1:4), '(i4.4)') year_xc5
  MTstr = '    '
  ZAstr = '       '
  if (MT_xc5 <= 999) then
    write(MTstr(1:3), '(i3.3)') MT_xc5
  else
    write(MTstr(1:4), '(i4.4)') MT_xc5
  endif
  if (isom_xc5 >= 0) MTstr(4:4) = isochar(isom_xc5)
  xsfile = '                                                    '
  exfile = '                                                    '
  filename = '                                                                                '
  if (flagres) then
    write(ZAstr(1:6),'(i6.6)') ZAres
    if (isom_xc5 >= 0) ZAstr(7:7) = isochar(isom_xc5)
    filename = proj//'-'//trim(nuclide_xc5)//'-rp'// trim(ZAstr)
  else
    filename = proj//'-'//trim(nuclide_xc5)//'-MT'// trim(MTstr)
  endif
  filename = trim(filename)//'-'//trim(code)//'-'// trim(subentry_xc5)
  if (flagelang .or. flaginelang) filename = trim(filename)//'-ang'
  if (flagspec) filename = trim(filename)//'-spec'
  if (flagddx) filename = trim(filename)//'-ddx'
  if (flagspec .or. flagddx .or. flagelang .or. flaginelang .or. flagdiscang .or. flagfy) then
    read(valstring(1), '(e9.2)') Einc
    Einc = Einc * 1.e-6
    Estring = '-E0000.000'
    if (Einc >= 0.001) then
      write(Estring(3:10), '(f8.3)') Einc
      write(Estring(3:6), '(i4.4)') int(Einc)
    else
      write(Estring(3:10), '(es8.2)') Einc
    endif
    filename = trim(filename)// Estring
  endif
  if (flagri .or. flagspecav .or. flagnubar .or. flagfy .or. flagrespar) xsfile = trim(path)//trim(filename)
  if (flagfy) then
    read(valstring(2), '(e9.2)') dEinc
    dEinc = dEinc * 1.e-6
  endif
  filename = trim(filename)//'.'//ystr
  xsfile = trim(path)// trim(filename)
!
! If a filename already exist, use the 'alt' extension
!
  inquire (file = xsfile, exist = lexist)
  if (lexist) then
    do i = 1, 999
      extension = 'A000'
      write(extension(2:4), '(i3.3)') i
      kend = len_trim(xsfile)
      do k = kend, 1, - 1
        if (xsfile(k:k) == '.') then
          exfile = xsfile(1:k - 1) //extension //trim(xsfile(k:kend))
          exit
        endif
      enddo
      inquire (file = exfile, exist = lexist)
      if (lexist) cycle
      xsfile = exfile
      kend = len_trim(filename)
      do k = kend, 1, - 1
        if (filename(k:k) == '.') then
          filename = filename(1:k - 1) //extension // trim(filename(k:kend))
          exit
        endif
      enddo
      exit
    enddo
  endif
  write(18, '(a9, 2(1x, a))') subentry_xc5, trim(filename), trim(xsfile)
!
! Make dictionaries
!
  if (k0_xc5 > 6) return
  xmin = Emin
  xmax = Emax
  Rstring = ''
  datatype = ''
  if (flagres) then
    ZAresexist(k0_xc5, Z_xc5, A_xc5, iso_xc5) = .true.
    k = ZAres
    Rstring = reacid(MTrp, -1)
    datatype = 'residual/'
    MTtype = '-rp'
    strtype = ZAstr
  else
    k = MT_xc5
    if (k <= nummt) Rstring = reacid(MT_xc5, isom_xc5)
    if (flagMT) datatype = 'xs/'
    if (flagnubar) datatype = 'fission/'
    if (flagratio) datatype = 'ratio/'
    if (flagri) datatype = 'resint/'
    if (flagrespar) datatype = 'resonance/'
    if (flagspec) datatype = 'spectrum/'
    if (flagddx) datatype = 'ddx/'
    if (flagfy .or. flagtke) datatype = ''
    if (flagelang .or. flagdiscang) then
      datatype = 'angle/'
      xmin = angmin
      xmax = angmax
    endif
    MTtype = '-MT'
    strtype = MTstr
  endif
  Rstring(2:2) = proj
  if (flagfy .or. flagtke) then
    nucpath = 'FY/' // proj // '/' // trim(nuclide_xc5) // '/'
  else
    nucpath = proj // '/' // trim(nuclide_xc5) // '/'
  endif
  do
    xslist = trim(xspath)//'xslist'
    open (unit = 79, status = 'unknown', file = xslist, position = 'append', iostat = istat)
    if (istat /= 0) exit
    write(79, '(i6, 1x, a, 1x, a9)') k, trim(filename), subentry_xc5
    close (79)
    if (.not. ZAexist(k0_xc5, Z_xc5, A_xc5, iso_xc5)) then
      ZAexist(k0_xc5, Z_xc5, A_xc5, iso_xc5) = .true.
      ZAlist = proj // '/' // proj // '.list'
      inquire (file = ZAlist, exist = lexist)
      if (.not.lexist) then
        open (unit = 71, status = 'unknown', file = ZAlist, iostat = istat)
        if (istat /= 0) exit
        write(71, '("# Z   A  iso Nuclide")') 
        close (71)
      endif
      open (unit = 71, status = 'unknown', file = ZAlist, position = 'append', iostat = istat)
      if (istat /= 0) exit
      write(71, '(3i4,1x,a6)') Z_xc5, A_xc5, iso_xc5, nuclide_xc5
      close (71)
    endif
    ZAMTlist = trim(nucpath) // trim(datatype) // proj // '-' // trim(nuclide_xc5) // '.list'
    inquire (file = ZAMTlist, exist = lexist)
    if (.not.lexist) then
      open (unit = 72, status = 'unknown', file = ZAMTlist, iostat = istat)
      if (istat /= 0) exit
      write(72, '("#   MT   iso Reaction")') 
      close (72)
    endif
    open (unit = 72, status = 'unknown', file = ZAMTlist, position = 'append', iostat = istat)
    if (istat /= 0) exit
    write(72, '(2i6,1x,a10)') k, isom_xc5, Rstring
    close (72)
    setlist = trim(nucpath) // trim(datatype) // trim(strtype) // '/' // proj // '-' // trim(nuclide_xc5) // MTtype // &
 &    trim(strtype) // '.list'
    inquire (file = setlist, exist = lexist)
    if (.not.lexist) then
      open (unit = 73, status = 'unknown', file = setlist, iostat = istat)
      if (istat /= 0) exit
      write(73, '("#              Filename                                         N    min         max     ")') 
      close (73)
    endif
    open (unit = 73, status = 'unknown', file = setlist, position = 'append', iostat = istat)
      if (istat /= 0) exit
    write(73, '(a60,i6,2es12.5)') filename, Npoints_new, xmin, xmax
    close (73)
    exit
  enddo
  return
end subroutine filesub
! Copyright A.J. Koning 2019
