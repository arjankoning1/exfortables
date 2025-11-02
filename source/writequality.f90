subroutine writequality(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write quality information
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
!              Asset, &         ! average As value per subentry
!              chi2set, &       ! average Chi - 2 per subentry
!              authors_E, &   ! authors of paper
!              Emax_E, &          ! maximum energy (MeV) of subentry
!              Emin_E, &          ! minimum energy (MeV) of subentry
!              expfile, &       ! experimental data file
!              flagMT, &        ! flag for MT cross section
!              Fset, &          ! average F value per subentry
!              k0_xc5, &        ! incident particle
!              library, &       ! nuclear data library
!              MT_xc5, &        ! MT number
!              nuclide_xc5, &   ! nuclide
!              Nlibs, &         ! number of data libraries for comparison
!              Npoints_E, &   ! number of points per subentry
!              Npointsset, &    ! number of points per subentry
!              proj, &          ! projectile
!              pset, &          ! average p value per subentry
!              Qaction, &       ! recommended action
!              Qdate, &         ! quality date
!              Qscore, &        ! quality score
!              reaction_E, &  ! XC5 reaction
!              reference_E, & ! reference of paper
!              subentry_E, &  ! XC5 subentry number
!              title_E          ! title of paper
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist     !
  logical            :: fill     !
  character(len=4)   :: MTstr      !
  character(len=9)   :: subentry   !
  character(len=10)  :: liblast
  character(len=20)  :: qf         !
  character(len=132) :: qfile      !
  character(len=132) :: qMTfile    !
  character(len=132) :: qallfile   !
  character(len=132) :: jsonfile   !
  character(len=132) :: string
  integer            :: ff         !
  integer            :: i          ! counter
  integer            :: ix
  integer            :: istat
  integer            :: k
  integer            :: iset       ! counter
  integer            :: lib        !
  integer            :: MT         !
  integer            :: N 
!
! Write quality information
!
  MTstr = '    '
  subentry = subentry_E(iset)
  MT = MT_xc5
  write(MTstr(1:3), '(i3.3)') MT
  MTstr(4:4) = isochar(isom_xc5)
  liblast =''
  do i = 1, 2
    ff = 2 + i
    if (i == 1) then
      qfile = expfile(iset)
      open (unit = ff, status = 'unknown', position = 'append', file = qfile)
    else
      if ( .not. (flagMT .and. k0_xc5 == 1)) cycle
      qf = proj//'-'//trim(nuclide_xc5)//'-MT'//trim(MTstr)//'.q'
      qfile = 'quality/'//trim(qf)
      inquire (file = qfile, exist = lexist)
      if (lexist) then
        open (unit = ff, status = 'unknown', position = 'append', file = qfile)
      else
        open (unit = ff, status = 'unknown', file = qfile)
      endif
    endif
    N = 0
    do k=1,Nlibs
      if (libexist(k)) N=N+1
    enddo
    write(ff, '("# statistics:")')
    write(ff, '("#   NEA score: ", a2)') Qscore
    write(ff, '("#   IAEA score: ", i2)') weight
    write(ff, '("#   E-min [MeV]: ", es13.6)') Emin_E(iset)
    write(ff, '("#   E-max [MeV]: ", es13.6)') Emax_E(iset)
    write(ff, '("#   F-value: ", es13.6)') Fset(0)
    write(ff, '("#   A-value: ", es13.6)') Asset(0)
    write(ff, '("#   chi-2: ", es13.6)') chi2set(0)
    write(ff, '("#   p-value: ", es13.6)') pset(iset)
    write(ff, '("#   exp. data sets for p-value: ", i6)') Nexp(iset,1)
    write(ff, '("#   libraries: ", i6)') N
    write(ff, '("##  Library          F              A            chi-2")')
!   write(ff, '("#   quality: ", a9, ": ", a2)') subentry, Qscore
!   write(ff, '("# Date       ", a9, ": ", a10)') subentry, Qdate
!   write(ff, '("# Reaction   ", a9, ": NP:", i5, " E-range:", 1p, g11.3, "-", g11.3, " MeV ", a35)') subentry, &
!&    Npoints_E(iset), Emin_E(iset), Emax_E(iset), reaction_E(iset)
!   write(ff, '("# Action     ", a9, ": ", a)') subentry, trim(Qaction)
!   write(ff, '("# GOF estimators             N     F          A       chi-2")')
    do lib = 1, Nlibs
!     write(ff, '("# ",a10," ", a9, ": ",i6,1p,3g11.3)') library(lib),subentry,Npointsset(lib), Fset(lib), Asset(lib), chi2set(lib)
      if (libexist(lib)) then
        write(ff, '("# ",a10," ",3es15.6)') library(lib), Fset(lib), Asset(lib), chi2set(lib)
        liblast = library(lib)
      endif
    enddo
!   write(ff, '("# p-value    ", a9, ": ", 1p, g12.4)') subentry, pset(iset)
    if (i == 2) then
      write(ff, '("# Authors  ", a9, ": ", a)') subentry, trim(authors_E(iset))
      write(ff, '("# Title    ", a9, ": ", a)') subentry, trim(title_E(iset))
      write(ff, '("# Journal  ", a9, ": ", a)') subentry, trim(reference_E(iset))
    endif
    close (ff)
  enddo
  if (liblast /= '') then
    jsonfile = 'stat/json/' // trim(subentry) // '.json'
    open (unit = 21, status = 'unknown', file = jsonfile)
    fill = .true.
    do 
      read(21,'(a)', iostat = istat) string
      if (istat == -1) exit
      ix = index(string,'F values')
      if (ix > 0) fill = .false.
    enddo
    close(21)
    if (fill) then
      open (unit = 21, status = 'unknown', file = jsonfile, position = 'append')
      backspace 21
      write(21,'(4x,"""F values"": ")')
      write(21,'(4x,"[")')
      write(21,'(8x,"{")')
!     write(21,'(8x,"""p-value""        : ",es15.6,",")') pset(iset)
      do lib = 1, Nlibs
        if (libexist(lib)) then
          if (trim(library(lib)) == trim(liblast)) then
            write(21, '(8x,"""",a,"""",t26,": ",es15.6)') trim(library(lib)), Fset(lib)
          else
            write(21, '(8x,"""",a,"""",t26,": ",es15.6,",")') trim(library(lib)), Fset(lib)
          endif
        endif
      enddo
      write(21,'(8x,"}")')
      write(21,'(4x,"]")')
      write(21,'("}")')
      close(21)
    endif
  endif
  qMTfile = 'quality/'//proj//'-MT'//trim(MTstr)//'.q'
  open (unit = 71, status = 'unknown', position = 'append', file = qMTfile)
  write(71, '(a9, i4,2es15.6,2i4 )') subentry, weight, Fset(0), pset(iset),Nexp(iset,1),N
  close (71)
  qallfile = 'quality/'//proj//'-all'//'.q'
  open (unit = 71, status = 'unknown', position = 'append', file = qallfile)
  write(71, '(a9,2i4,2es15.6,2i4 )') subentry, MT, weight, Fset(0), pset(iset),Nexp(iset,1),N
  close (71)
  return
end subroutine writequality
! Copyright A.J. Koning 2019
