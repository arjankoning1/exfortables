subroutine compare
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Compare experimental data with TALYS and libraries
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
! use A1_error_handling_mod, only: & ! Error handling
!                      read_error ! Message for file reading error
! use A1_exfortables_mod, only: & ! All global variables
!              A_xc5, &   ! mass number
!              Amax, &         ! maximal A value to process
!              Amin, &         ! minimal A value to process
!              compdir, &      ! directory with statistics
!              expfile, & ! experimental data file
!              Ffile, &      ! file with F values
!              flageview, &   ! flag to make ECISVIEW file
!              flaglib, &     ! flag for data from libraries
!              flagtalys, &   ! flag for data from TALYS
!              flagMT, &      !
!              flagout, &     ! flag for main output
!              flagres, &      !
!              flagunc, &     ! flag for uncertainty analysis
!              isochar, &      ! character for isomer
!              iso_xc5, &   ! number of target isomer
!              isom_xc5, &   ! number of final isomer
!              k0_xc5, &   ! incident particle type
!              MT_xc5, &   ! MT number
!              MTrp, &         ! MT number
!              Nset, &      ! number of experimental data sets
!              numsets, &      ! maximum number of experimental data sets
!              nuc, &     ! symbol of nucleus
!              nuclide_xc5, &   ! nuclide
!              numA, &    ! maximum number of masses
!              numisom, & ! maximum number of isomers
!              numpar, &  ! number of particles
!              nummt, &   ! maximum number of MT numbers
!              numZ, &    ! maximum number of charge
!              partype, & ! symbol of particle
!              proj, &    ! symbol of projectile
!              protar_xc5, & ! projectile-nuclide
!              uncdir, &              ! directory with uncertainties
!              Z_xc5, &   ! charge number
!              ZAexist, &   ! logical for Z, A existence
!              ZAres, &   ! ZA of residual nucleus
!              Zmax, &         ! maximal Z value to process
!              Zmin            ! minimal Z value to process
!
! *** Declaration of local data
!
  implicit none
  logical           :: lexist  ! logical for file existence
  character(len=2)  :: projpath ! projectile path
  character(len=4)  :: Astr     ! mass string
  character(len=3)  :: fileext  ! file extension
  character(len=4)  :: MTstr    ! MT string
  character(len=7)  :: resstr    ! MT string
  character(len=80) :: projlist ! file name
  character(len=80) :: typedir  ! type of directory
  character(len=80) :: allfile  ! file name
  character(len=80) :: MTfile   ! MT file name
  character(len=80) :: MTlist   ! file name
  character(len=80) :: setlist  ! file name
  character(len=80) :: nucpath  ! nuclide path
  character(len=80) :: expfile0 ! experimental data file
  character(len=80) :: MTpath   ! MT path
  integer           :: k0       ! incident particle type
  integer           :: MT       ! MT number
  integer           :: istat    ! logical for existence
  integer           :: iset     ! counter
  integer           :: A        ! mass number
  integer           :: Z        ! charge number
  integer           :: L        ! length of string
  integer           :: iso      ! counter for isomer
  integer           :: isor     ! counter for isomer
  integer           :: type     ! counter
!
! ***************************** Comparison *****************************
!
  write(*, *) "Comparison with libraries....."
  do k0 = 0, numpar
    if (.not. parinclude(k0)) cycle
    k0_xc5 = k0
    proj = partype(k0)
    projpath = proj // '/'
    projlist = trim(projpath) // proj // '.list'
    inquire (file = projlist, exist = lexist)
    if (lexist) then
      open(unit=2, file = projlist, status = 'unknown', iostat = istat)
      if (istat /= 0) call read_error(projlist, istat)
      read(2, '()', iostat = istat)
      if (istat == -1) exit
      do
        read(2, '(3i4)', iostat = istat) Z, A, iso
        if (istat == -1) exit
        ZAexist(k0, Z, A, iso) = .true.
      enddo
      close(2)
      do Z = 1, numZ
        if (Z < Zmin .or. Z > Zmax) cycle
        do A = 0, numA
          if (A > 0 .and. A < Z ) cycle
          if (A < Amin .or. A > Amax) cycle
          do iso = 0, numisom
            if (ZAexist(k0, Z, A, iso)) then
              Z_xc5 = Z
              A_xc5 = A
              iso_xc5 = iso
              Astr = '    '
              write(Astr(1:3), '(i3.3)') A
              if (iso == 1) Astr(4:4) = 'm'
              if (iso == 2) Astr(4:4) = 'n'
              nuclide_xc5 = trim(nuc(Z)) // Astr
              protar_xc5 = proj // '-' // nuclide_xc5
              write(*, '("Processing ",a1," + ",a6)') proj, nuclide_xc5
              nucpath = projpath // trim(nuclide_xc5) // '/'
!
! Loop over MT cross sections and residual production cross sections
!
              do type = 1, 2
                if (type == 1) then
                  flagMT = .true.
                  flagres = .false.
                  typedir = 'xs/'
                  fileext = '-MT'
                else
                  flagMT = .false.
                  flagres = .true.
                  typedir = 'residual/'
                  fileext = '-rp'
                endif
                MTlist = trim(nucpath) // trim(typedir) // trim(protar_xc5) // '.list'
                inquire (file = MTlist, exist = lexist)
                if (lexist) then
                  open(unit=33, file = MTlist, status = 'unknown', iostat = istat)
                  if (istat /= 0) call read_error(MTlist, istat)
                  read(33, '()', iostat = istat)
                  if (istat == -1) exit
                  do
                    read(33, '(2i6)', iostat = istat) MT,isor
                    if (istat == -1) exit
                    isom_xc5 = isor
                    if (flagMT) then
                      MT_xc5 = MT
                    else
                      MT_xc5 = MTrp
                      resstr = '       '
                      ZAres = MT
                      write(resstr(1:6), '(i6.6)') ZAres
                      if (isor >= 0) resstr = trim(resstr) // isochar(isor)
                    endif
                    MTstr = '    '
                    write(MTstr(1:3), '(i3.3)') MT_xc5
                    if (isor >= 0) MTstr = trim(MTstr) // isochar(isor)
                    if (flagMT) then
                      MTpath = trim(nucpath) // trim(typedir) // trim(MTstr) // '/'
                      setlist = trim(MTpath) // trim(protar_xc5) // fileext // trim(MTstr) // '.list'
                    else
                      MTpath = trim(nucpath) // trim(typedir) // trim(resstr) // '/'
                      setlist = trim(MTpath) // trim(protar_xc5) // fileext // trim(resstr) // '.list'
                    endif
                    inquire (file = setlist, exist = lexist)
                    if (lexist) then
                      open(unit=2, file = setlist, status = 'unknown', iostat = istat)
                      if (istat /= 0) call read_error(setlist, istat)
                      Ffile = trim(compdir) // 'reaction/' // trim(protar_xc5) // '-MT' // trim(MTstr) // '.F'
                      if (flagtalys) call readtalys
                      if (flaglib) call readlib
                      Nset = 0
                      expfile = ''
                      read(2, '()', iostat = istat)
                      if (istat == -1) exit
                      do
                        if (Nset == numsets) then
                          write(*, '(" EXFORTABLES-Warning: Nset larger than ",i4, " Data set skipped")') numsets
                          Nset = Nset - 1
                          exit
                        endif
                        read(2, '(a55)', iostat = istat) expfile0
                        if (istat == -1) exit
                        if (flagout) write(*, '(2x,a)') trim(expfile0)
                        Nset = Nset + 1
                        expfile(Nset) = 'stat/'// trim(MTpath) // trim(expfile0)
                        call readexp
                      enddo
                      close(2)
                      call xsinterpol(Nset)
                      call pvalues(Nset)
                      do iset= 1, Nset
                        call comparesub(iset)
                        call comparesum(iset)
                        call quality(iset)
                        call reacstat(iset)
                        call writeF(iset)
                        call writequality(iset)
                        if (flagunc) call uncstat(iset)
                        if (flageview) call eview(iset)
                      enddo
                      L =len_trim(Ffile)
                      call sort(Ffile, L, 'R', 83, 92, 1, 0)
                    endif
                  enddo
                  close(33)
                endif
              enddo
            endif
          enddo
        enddo
      enddo
      write(*, *) "Comparison with libraries done"
      write(*, *) "Sorting output by F value....."
!
! Sorting F values and uncertainties per MT number
!
      do MT = 1, nummt
        do isor = -1, numisom
          MTstr = '    '
          write(MTstr(1:3), '(i3.3)') MT
          if (isor >= 0) MTstr(4:4) = isochar(isor)
          MTfile = trim(compdir) //'MT/'// proj // '-MT' // trim(MTstr) // '.F'
          L =len_trim(MTfile)
          inquire (file = MTfile, exist = lexist)
          if (lexist) call sort(MTfile, L, 'R', 83, 92, 1, 0)
          if (flagunc) then
            MTfile = trim(uncdir) //'MT/'// proj // '-MT' // trim(MTstr) // '.unc'
            L =len_trim(MTfile)
            inquire (file = MTfile, exist = lexist)
            if (lexist) call sort(MTfile, L, 'R', 77, 86, 1, 0)
          endif
        enddo
      enddo
      allfile = trim(compdir) // 'total/' // proj // '-allreac'
      L =len_trim(allfile)
      call sort(allfile, L, 'R', 83, 92, 1, 0)
      if (flagunc) then
        allfile = trim(uncdir) // 'total/' // proj // '-unc'
        L =len_trim(allfile)
        call sort(allfile, L, 'R', 77, 86, 1, 0)
      endif
    endif
  enddo
  write(*, *) "Sorting done"
  return
end subroutine compare
