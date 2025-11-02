subroutine fout
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Output of F values
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     01-01-2019   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A0_kinds_mod, only: & ! Definition of single and double precision variables
!              sgl              ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              Aexist_xc5, &    ! flag for existence of A
!              Aix, &           ! index for A
!              author1_xc5, &   ! author
!              authorcode, &    ! author
!              ebin, &          ! lower bound of energy bin
!              ecbin, &         ! energy bin
!              Emax, &          ! maximum energy (MeV) of subentry
!              Emin, &          ! minimum energy (MeV) of subentry
!              entry_sub, &     ! entry of subentry
!              entry_xc5, &     ! entry
!              Fall, &          ! average F value for all reactions
!              FallE, &         ! total F per energy bin
!              FAMT, &          ! average F for all MT numbers
!              fbin, &          ! boundaries for F bins
!              Fentry, &        ! average F value per entry
!              flaglib, &       ! flag for library comparison
!              Fmax, &          ! maximum F value
!              FMT, &           ! average F value per MT number
!              FMTE, &          ! F per MT number and energy bin
!              Fnuc, &          ! average F value per nucleus
!              FnucE, &         ! F per nucleus and energy bin
!              FnucMT, &        ! average log F value per nucleus and MT
!              Fpar, &          ! average F value per particle
!              FparE, &         ! F per particle and energy bin
!              Fset, &          ! average F value per subentry
!              isochar, &       ! character for isomer
!              library, &       ! nuclear data library
!              libstring, &     ! string with library names
!              libstring2, &    ! string with library names
!              MTbin, &         ! number of F values in MT bin
!              MTbinav, &       ! average F value per bin
!              MTbincum, &      ! cumulative F value per MT number
!              MTbinsigma, &    ! average F deviation per bin
!              MTbintot, &      ! total number of F values in MT bin
!              MTexist, &       ! flag for existence of MT
!              MTexist_xc5, &   ! flag for existence of MT
!              mtinclude, &     ! flag to include MT number
!              MTix, &          ! index for MT
!              MTsum, &         ! number of reactions per MT number
!              MTsumtot, &      ! total F value per particle per MT
!              Nentry_xc5, &    ! number of entries according to NRDC file
!              Nlibs, &         ! number of data libraries for comparison
!              Npoints, &       ! number of points compared per library
!              Npointsall, &    ! total number of points
!              NpointsallE, &   ! number of points per energy bin
!              NpointsAMT, &    ! total number of points per MT number
!              NpointsMT, &     ! number of points per MT number
!              NpointsMTE, &    ! number of points per MT number and ene
!              Npointsnuc, &    ! number of points per nucleus
!              NpointsnucE, &   ! number of points per nucleus and energ
!              NpointsnucMT, &  ! number of points per nucleus and MT nu
!              Npointspar, &    ! number of points per particle
!              NpointsparE, &   ! number of points per particle and ener
!              Nsetsall, &      ! total number of data sets
!              NsetsallE, &     ! total number of data sets for all ener
!              NsetsallMT, &    ! all MT sets
!              NsetsallMTtot, & ! all MT sets
!              NsetsAMT, &      ! all MT sets
!              NsetsMT, &       ! number of data sets per MT number
!              NsetsMTE, &      ! number of data sets per MT number and
!              NsetsMTtot, &    ! number of sets per MT number
!              Nsetsnuc, &      ! number of data sets per nucleus
!              NsetsnucE, &     ! number of data sets per nuclide and en
!              NsetsnucMT, &    ! number of data sets per nucleus and MT
!              Nsetspar, &      ! number of data sets per particle
!              NsetsparE, &     ! number of data sets per particle and e
!              Nsub_xc5, &      ! counter for XC5 subentries
!              nuc, &           ! symbol of nucleus
!              numA, &          ! maximum number of masses
!              numbin, &        ! maximum number of bins
!              numEbin, &       ! maximum number of energy bins
!              numisom, &       ! maximum number of isomers
!              nummt, &         ! maximum number of MT numbers
!              numpar, &        ! maximum number of particles
!              numZ, &          ! maximum number of elements
!              parinclude, &    ! flag to include particle
!              parname, &       ! name of particle
!              partype, &       ! symbol of particle
!              pointref, &      ! reference for pointwise comparison
!              processed, &     ! flag for processed subentry
!              Qentry, &        ! entry with quality information
!              QMT, &           ! quality per MT number
!              QMTall, &        ! total quality per MT number
!              Qscore, &        ! quality score
!              reacid, &        ! reaction string
!              reaction_xc5, &  ! XC5 reaction
!              subentry_xc5, &  ! XC5 subentry number
!              uncertainty, &   ! average uncertainty of subentry
!              xsexpav, &       ! average experimental cross section
!              xsthset, &       ! average theoretical cross section per
!              year_xc5         ! year
!
! *** Declaration of local data
!
  implicit none
  character(len=15) :: col(15)    ! header
  character(len=15) :: un(15)    ! header
  character(len=80) :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  character(len=3)   :: Astr                    !
  character(len=3)   :: amp                     !
  character(len=4)   :: ystr                    !
  character(len=7)   :: Fbase                   !
  character(len=10)  :: subdir                  !
  character(len=11)  :: statfile                !
  character(len=14)  :: mtfile                  !
  character(len=20)  :: FallEfile               !
  character(len=22)  :: FparEfile               !
  character(len=22)  :: FnucEfile               !
  character(len=23)  :: Fnucfile                !
  character(len=28)  :: FMTEfile                !
  character(len=28)  :: FMTAfile                !
  character(len=27)  :: Fstatfile               !
  character(len=35)  :: entryfile               !
  character(len=300) :: fstring                 !
  character(len=300) :: headstring1(0:numpar)   !
  character(len=300) :: headstring2(0:numpar)   !
  character(len=300) :: headstring3(0:numpar)   !
  character(len=300) :: headstring4(0:numpar)   !
  character(len=300) :: headstring5(0:numpar)   !
  character(len=300) :: secstring(0:numpar)     !
  character(len=300) :: setstring               !
  character(len=300) :: tailstring              !
  integer            :: A                       !
  integer            :: bin                     !
  integer            :: i                       ! counter
  integer            :: ia                      !
  integer            :: ie                      !
  integer            :: imt                     !
  integer            :: Ncol
  integer            :: Np
  integer            :: is                      !
  integer            :: iz                      !
  integer            :: j                       ! counter
  integer            :: k                       ! counter
  integer            :: k0                      !
  integer            :: lib                     !
  integer            :: m                       ! counter
  integer            :: MT                      !
  integer            :: N                       !
  integer            :: Z                       !
  integer            :: indent
  integer            :: id2
  integer            :: id4
  real(sgl)          :: eb                      !
!
! ************** Output of individual F values *************************
!
  indent = 0
  id2 = indent + 2
  id4 = indent + 4
  amp = ' & '
  subdir = 'stat/comp/'
  write( * , * ) "Writing F values....."
  headstring1 = '# Z    A  MT Iso         World  '
  headstring2 = '# energy              World  '
  headstring3 = '# Z    A              World  '
  headstring4 = '# MT Iso              World  '
  headstring5 = '# particle            World  '
  secstring = '#             F     #points #sets'
  if (flaglib) then
    headstring1(1) = trim(headstring1(1))//'    '//libstring2
    headstring2(1) = trim(headstring2(1))//'    '//libstring2
    headstring3(1) = trim(headstring3(1))//'    '//libstring2
    headstring4(1) = trim(headstring4(1))//'    '//libstring2
    headstring5(1) = trim(headstring5(1))//'    '//libstring2
    secstring(1) = '#'
    write(secstring(1)(13:219), '(9(a23))') (' |  F     #points #sets', i = 1, 9)
  endif
  write(fstring, '(300(" "))')
  do k0 = 0, numpar
    if ( .not. parinclude(k0)) cycle
    do Z = 1, numZ
      Fbase = '       '
      Fnucfile = '                       '
      do A = 0, numA
        if ( .not. Aexist_xc5(k0, Z, A)) cycle
        ia = Aix(k0, Z, A)
        write(Astr(1:3), '(i3.3)') A
        Fbase = partype(k0)//'-'//trim(nuc(Z))//Astr
        Fnucfile = subdir//'nucMT/'//Fbase
        open (unit = 1, status = 'unknown', file = Fnucfile)
        topline = 'F per MT number for '//parname(k0)//' + '//trim(nuc(Z))//Astr
        quantity = 'Fs'
        call write_header(indent,topline,source,user,date,oformat)
        Ztarget = Z
        Atarget = A
        targetnuclide = trim(nuc(Z))//Astr
        call write_target(indent)
        call write_reaction(indent,'('//partype(k0)//',x)',0.D0,0.D0,0,0)
        call write_char(id2,'parameters','')
        call write_integer(id4,'Number of sets',Nsetsnuc(2, k0, Z, A))
        call write_integer(id4,'Number of points',Npointsnuc(2, k0, Z, A))
        do lib= 1, Nlibs
          call write_real(id4,'F for '//trim(library(lib)),Fnuc(lib, k0, Z, A))
        enddo
        un = ''
        col = ''
        col(1) = 'MT'
        col(2) = 'iso'
        do lib = 1, Nlibs
          col(2+lib) = trim(library(lib))
        enddo
        col(2+Nlibs+1) = 'sets'
        col(2+Nlibs+2) = 'points'
        Ncol=2+Nlibs+2
        Np = 0
        call write_quantity(id2,quantity)
        call write_datablock(id2,Ncol,Np,col,un)
        do MT = 1, nummt
          if ( .not. MTexist_xc5(MT)) cycle
          imt = MTix(MT)
          do is = - 1, numisom
            if ( .not. MTexist(k0, MT, is)) cycle
            N = NpointsnucMT(2, k0, Z, ia, imt, is)
            if (N > 0) then
              write(1, '(2(6x,i6,3x), 8es15.6, 2(6x,i6,3x))') MT, is, (FnucMT(lib, k0, Z, ia, imt, is), lib = 1, Nlibs), &
 &              NsetsnucMT(2, k0, Z, ia, imt, is), N
            endif
          enddo
        enddo
        close(unit = 1)
!       open (unit = 8, status = 'unknown', file = Fnucfile)
!       write(8, '("# Average F values per MT number for ", a8, " + ", a2, i3, " for all ", i6, " subentries with F < ", &
!&        1p, g10.3)') parname(k0), nuc(Z), A, Nsetsnuc(pointref, k0, Z, A), Fmax
!       write(8, '(a)') trim(headstring1(k0))
!       write(8, '(a)') trim(secstring(k0))
!       do MT = 1, nummt
!         if ( .not. MTexist_xc5(MT)) cycle
!         imt = MTix(MT)
!         do is = - 1, numisom
!           if ( .not. MTexist(k0, MT, is)) cycle
!           N = NpointsnucMT(0, k0, Z, ia, imt, is)
!           if (N > 0) then
!             write(fstring, '(5x, i4, i3, 1p, g10.3, 0p, i7, i6)') MT, is, FnucMT(0, k0, Z, ia, imt, is), N, &
!&              NsetsnucMT(0, k0, Z, ia, imt, is)
!             if (flaglib) then
!               write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FnucMT(lib, k0, Z, ia, imt, is), &
!&                NpointsnucMT(lib, k0, Z, ia, imt, is), NsetsnucMT(lib, k0, Z, ia, imt, is), lib = 1, Nlibs)
!               fstring = trim(fstring) //setstring
!             endif
!             write(8, '(a)') trim(fstring)
!           endif
!         enddo
!       enddo
!       write(8, '(a)') trim(headstring1(k0))
!       write(8, '(a)') trim(secstring(k0))
!       write(fstring, '(300(" "))')
!       write(fstring, '("        Total: ", 1p, g10.3, 0p, i7, i6)') &
!&          Fnuc(pointref, k0, Z, A), Npointsnuc(pointref, k0, Z, A), Nsetsnuc(pointref, k0, Z, A)
!       if (flaglib) then
!         write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (Fnuc(lib, k0, Z, A), Npointsnuc(lib, k0, Z, A), &
!&          Nsetsnuc(lib, k0, Z, A), lib = 1, Nlibs)
!         fstring = trim(fstring) //setstring
!       endif
!       write(8, '(a)') trim(fstring)
!       close(unit = 8)
        FnucEfile = subdir//'nucE/'//Fbase
        open (unit = 1, status = 'unknown', file = FnucEfile)
        topline = 'F per energy bin for '//parname(k0)//' + '//trim(nuc(Z))//Astr
        quantity = 'F'
        call write_header(indent,topline,source,user,date,oformat)
        call write_target(indent)
        call write_reaction(indent,'('//partype(k0)//',x)',0.D0,0.D0,0,0)
        un = ''
        col = ''
        col(1) = 'E'
        un(1) = 'MeV'
        do lib = 1, Nlibs
          col(1+lib) = trim(library(lib))
        enddo
        col(1+Nlibs+1) = 'sets'
        col(1+Nlibs+2) = 'points'
        Ncol=1+Nlibs+2
        call write_quantity(id2,quantity)
        call write_datablock(id2,Ncol,numEbin,col,un)
        do bin = 1, numEbin
          write(1, '(9es15.6,2(6x,i6,6x))') ebin(bin), (FnucE(lib, k0, Z, ia, bin), lib=1, Nlibs), &
 &          NsetsnucE(2, k0, Z, ia, bin), NpointsnucE(2, k0, Z, ia, bin)
        enddo
        close(unit = 1)
!       open (unit = 9, status = 'unknown', file = FnucEfile)
!       write(9, '("# Average F values per energy bin for ", a8, " + ", a3, i2, " for all subentries with F < ", 1p, g10.3)')  &
!&        parname(k0), nuc(Z), A, Fmax
!       write(9, '(a)') trim(headstring2(k0))
!       write(9, '(a)') trim(secstring(k0))
!       write(fstring, '(300(" "))')
!       do bin = 1, numEbin
!         write(fstring, '(1p, g12.4, g10.3, 0p, i7, i6)') ebin(bin), FnucE(0, k0, Z, ia, bin), &
!&          NpointsnucE(0, k0, Z, ia, bin), NsetsnucE(0, k0, Z, ia, bin)
!         if (flaglib) then
!           write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FnucE(lib, k0, Z, ia, bin), &
!&            NpointsnucE(lib, k0, Z, ia, bin), NsetsnucE(lib, k0, Z, ia, bin), lib = 1, Nlibs)
!           fstring = trim(fstring) //setstring
!         endif
!         write(9, '(a)') trim(fstring)
!       enddo
!       write(9, '(a)') trim(headstring2(k0))
!       write(9, '(a)') trim(secstring(k0))
!       close(unit = 9)
      enddo
    enddo
    write(fstring, '(300(" "))')
    do MT = 1, nummt
      if ( .not. MTexist_xc5(MT)) cycle
      imt = MTix(MT)
      Np = 0
      do is = -1, numisom
        if ( .not. MTexist(k0, MT, is)) cycle
        write(reacid(MT, is)(2:2), '(a1)') partype(k0)
        mtfile = partype(k0)//'-MT000      '
        write(mtfile(5:7), '(i3.3)') MT
        if (is >= 0) mtfile = trim(mtfile)//isochar(is)
        mtfile = trim(mtfile)//'.F'
        FMTAfile = subdir//'MTA/'//mtfile
!       open (unit = 8, status = 'unknown', file = FMTAfile)
!       write(8, '("# Average F values per mass unit for ", a8, " and MT = ", i3, 1x, a10, " for all ", i6, &
!&        " subentries with F < ", 1p, g10.3)') parname(k0), MT, reacid(MT, is), NsetsMT(pointref, k0, imt, is), Fmax
!       write(8, '(a)') trim(headstring3(k0))
!       write(8, '(a)') trim(secstring(k0))
        do A = 1, numA
!         N = NpointsAMT(0, k0, A, imt, is)
!         if (N > 0) then
!           write(fstring, '(4x, i4, 4x, 1p, g10.3, 0p, i7, i6)') A, FAMT(0, k0, A, imt, is), N, NsetsAMT(0, k0, A, imt, is)
!           if (flaglib) then
!             write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FAMT(lib, k0, A, imt, is), &
!&              NpointsAMT(lib, k0, A, imt, is), NsetsAMT(lib, k0, A, imt, is), lib = 1, Nlibs)
!             fstring = trim(fstring) //setstring
!           endif
!           write(8, '(a)') trim(fstring)
!         endif
          do Z = 1, numZ
            ia = Aix(k0, Z, A)
            N = NpointsnucMT(2, k0, Z, ia, imt, is)
            if (N > 0) Np = Np + 1
          enddo
        enddo
!       write(8, '(a)') trim(headstring3(k0))
!       write(8, '(a)') trim(secstring(k0))
!       write(fstring, '(300(" "))')
!       write(fstring, '("     Total: ", 1p, g10.3, 0p, i7, i6)') &
!&          FMT(pointref, k0, imt, is), NpointsMT(pointref, k0, imt, is), NsetsMT(pointref, k0, imt, is)
!       if (flaglib) then
!         write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FMT(lib, k0, imt, is), NpointsMT(lib, k0, imt, is), &
!&          NsetsMT(lib, k0, imt, is), lib = 1, Nlibs)
!         fstring = trim(fstring) //setstring
!       endif
!       write(8, '(a)') trim(fstring)
!       close(unit = 8)
        open (unit = 1, status = 'unknown', file = FMTAfile)
        topline = 'F per nuclide for '//reacid(MT, is)
        quantity = 'F'
        call write_header(indent,topline,source,user,date,oformat)
        call write_reaction(indent,reacid(MT, is),0.D0,0.D0,3,MT)
        call write_char(id2,'parameters','')
        call write_integer(id4,'Number of sets',NsetsMT(2, k0, imt, is))
        call write_integer(id4,'Number of points',NpointsMT(2, k0, imt, is))
        do lib= 1, Nlibs
          call write_real(id4,'F for '//trim(library(lib)),FMT(lib, k0, imt, is))
        enddo
        un = ''
        col = ''
        col(1) = 'Z'
        col(2) = 'A'
        col(3) = 'iso'
        do lib = 1, Nlibs
          col(3+lib) = trim(library(lib))
        enddo
        col(3+Nlibs+1) = 'sets'
        col(3+Nlibs+2) = 'points'
        Ncol=3+Nlibs+2
        call write_quantity(id2,quantity)
        call write_datablock(id2,Ncol,Np,col,un)
        do Z = 1, numZ
          do A = 1, numA
            ia = Aix(k0, Z, A)
            N = NpointsnucMT(2, k0, Z, ia, imt, is)
            if (N > 0) then
              write(1, '(3(6x,i3,6x),8f15.6,2(6x,i6,3x))') Z, A, is, (FnucMT(lib, k0, Z, ia, imt, is), lib = 1, Nlibs) , &
 &              NsetsnucMT(2, k0, Z, ia, imt, is), NpointsnucMT(2, k0, Z, ia, imt, is)
            endif
          enddo
        enddo
        close(1)
      enddo
    enddo
  enddo
!
! ***************** Output of F values per bin *************************
!
! Binning
!
  do k0 = 0, numpar
    if ( .not. parinclude(k0)) cycle
    do MT = 1, nummt
      if ( .not. mtinclude(MT)) cycle
      imt = MTix(MT)
      do is = - 1, numisom
        if ( .not. MTexist(k0, MT, is)) cycle
        write(reacid(MT, is)(2:2), '(a1)') partype(k0)
        mtfile = partype(k0)//'-MT000      '
        write(mtfile(5:7), '(i3.3)') MT
        if (is >= 0) mtfile = trim(mtfile)//isochar(is)
        mtfile = trim(mtfile)//'.histo'
        open (unit = 1, status = 'unknown',  file = subdir//'histo/'//mtfile)
        topline = 'Histogram of F per energy bin for '//reacid(MT, is)
        quantity = 'Sets'
        call write_header(indent,topline,source,user,date,oformat)
        call write_reaction(indent,reacid(MT, is),0.D0,0.D0,3,MT)
        call write_real(id2,'F(1-sigma)',MTbinsigma(k0, imt, is, 1))
        call write_real(id2,'F(2-sigma)',MTbinsigma(k0, imt, is, 2))
        un = ''
        col = ''
        col(1) = 'Fbin'
        col(2) = 'Sets'
        col(3) = 'Cum. fraction'
        col(4) = 'Average'
        Ncol = 4
!       do lib = 1, Nlibs
!         col(1+lib) = trim(library(lib))
!       enddo
!       col(1+Nlibs+1) = 'points'
!       col(1+Nlibs+2) = 'sets'
!       Ncol=1+Nlibs+2
        call write_quantity(id2,quantity)
        call write_datablock(id2,Ncol,numbin+1,col,un)
        do j = 0, numbin
          write(1, '(f15.6, 6x,i6, 3x, 2es15.6)') fbin(j), MTbin(k0, imt, is, j), MTbincum(k0, imt, is, j), &
 &          MTbinav(k0, imt, is, j)
        enddo
        close (1)
!       open (unit = 13, status = 'unknown', file = subdir//'histo/'//mtfile)
!       write(13, '("#MT = ", i3, " ", a10, " #Sets:", i6, " Reference: ", a10)') MT, reacid(MT, is), &
!&        MTbintot(k0, imt, is), library(pointref)
!       write(13, '("#  Fbin  #Sets  Cum. fraction   Average", " F(1-sigma): ", f12.5, " F(2-sigma): ", f12.5)') &
!&        (MTbinsigma(k0, imt, is, j), j = 1, 2)
!       do j = 0, numbin
!         write(13, '(f8.3, i6, 2f12.3)') fbin(j), MTbin(k0, imt, is, j), MTbincum(k0, imt, is, j), MTbinav(k0, imt, is, j)
!       enddo
!       close (13)
      enddo
    enddo
  enddo
!
! ******************** Output of total F values ************************
!
! Totals per MT number
!
  open (unit = 11, status = 'unknown', file = subdir//'total/MT.sum')
  write(11, '("# Total number of reactions compared")')
  do k0 = 0, numpar
    if ( .not. parinclude(k0)) cycle
    do MT = 1, nummt
      if ( .not. mtinclude(MT)) cycle
      imt = MTix(MT)
      do is = - 1, numisom
        if ( .not. MTexist(k0, MT, is)) cycle
        write(reacid(MT, is)(2:2), '(a1)') partype(k0)
        write(11, '(1x, i3, 1x, a10, 1x, i6)') MT, reacid(MT, is), MTsum(k0, imt, is)
      enddo
    enddo
  enddo
  close (11)
!
! Total F values summed over all reactions, per Z,A  and per MT number
!
! Per particle and MT number
!
  do k0 = 0, numpar
    if ( .not. parinclude(k0)) cycle
    if (flaglib) open (unit = 19, status = 'unknown', &
      file = subdir//'total/'//partype(k0)//'-F.tex')
    open (unit = 11, status = 'unknown', file = subdir//'total/'//partype(k0)//'-MT.F')
!   write(11, '("# Average F values per reaction summed over all", &
!&    " nuclides for ", a8, " for all ", i6, " subentries with F < ", 1p, g10.3)') parname(k0), Nsetspar(pointref, k0), Fmax
!   write(11, '(a)') trim(headstring4(k0))
!   write(11, '(a)') trim(secstring(k0))
    write(fstring, '(300(" "))')
    do MT = 1, nummt
      if ( .not. MTexist(k0, MT, - 1)) cycle
      imt = MTix(MT)
      do is = - 1, numisom
        if ( .not. MTexist(k0, MT, is)) cycle
        if (NpointsMT(pointref, k0, imt, is) == 0) cycle
        write(reacid(MT, is)(2:2), '(a1)') partype(k0)
!       write(fstring, '(i4, i3, 5x, 1p, g10.3, 0p, i7, i6)') MT, is, FMT(0, k0, imt, is), &
!&        NpointsMT(pointref, k0, imt, is), NsetsMT(pointref, k0, imt, is)
        if (flaglib) then
!         write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FMT(lib, k0, imt, is), NpointsMT(lib, k0, imt, is), &
!&          NsetsMT(lib, k0, imt, is), lib = 1, Nlibs)
!         fstring = trim(fstring) //setstring
          write(19, '(a10, 7(a3, 1p, g10.3, 0p, a3, i6),"\\")') reacid(MT, is), (amp, FMT(lib, k0, imt, is), amp, &
 &          NsetsMT(lib, k0, imt, is), lib = 2, Nlibs)
        endif
!       write(11, '(a)') trim(fstring)
        mtfile = partype(k0)//'-MT000      '
        write(mtfile(5:7), '(i3.3)') MT
        if (is >= 0) mtfile = trim(mtfile)//isochar(is)
        mtfile = trim(mtfile)//'.F'
        FMTEfile = subdir//'MTE/'//mtfile
        open (unit = 1, status = 'unknown',  file = FMTEfile)
        topline = 'F per energy bin for '//reacid(MT, is)
        quantity = 'F'
        call write_header(indent,topline,source,user,date,oformat)
        call write_reaction(indent,reacid(MT, is),0.D0,0.D0,3,MT)
        call write_real(id2,'Maximum F',Fmax)
        un = ''
        col = ''
        col(1) = 'E'
        un(1) = 'MeV'
        do lib = 1, Nlibs
          col(1+lib) = trim(library(lib))
        enddo
        col(1+Nlibs+1) = 'sets'
        col(1+Nlibs+2) = 'points'
        Ncol=1+Nlibs+2
        call write_quantity(id2,quantity)
        call write_datablock(id2,Ncol,numEbin,col,un)
        do bin = 1, numEbin
          if (MT > 3 .and. MT /= 102) then
            eb = ecbin(bin)
          else
            eb = ebin(bin)
          endif
          if (eb < 0.) cycle
          write(1, '(9es15.6,2(6x,i6,3x))') eb, (FMTE(lib, k0, imt, is, bin), lib = 1, Nlibs), &
 &           NsetsMTE(2, k0, imt, is, bin), NpointsMTE(2, k0, imt, is, bin)
        enddo
        close(unit = 1)
!       open (unit = 9, status = 'unknown', file = FMTEfile)
!       write(9, '("# Average F values per energy bin for ", a8, " and MT = ", i3, 1x, a10, &
!&        " for all subentries with F < ", 1p, g10.3)') parname(k0), MT, reacid(MT, is), Fmax
!       write(9, '(a)') trim(headstring2(k0))
!       write(9, '(a)') trim(secstring(k0))
!       write(fstring, '(300(" "))')
!       do bin = 1, numEbin
!         if (MT > 3 .and. MT /= 102) then
!           eb = ecbin(bin)
!         else
!           eb = ebin(bin)
!         endif
!         if (eb < 0.) cycle
!         write(fstring, '(1p, g12.3, g10.3, 0p, i7, i6)') eb, FMTE(0, k0, imt, is, bin), &
!&          NpointsMTE(0, k0, imt, is, bin), NsetsMTE(0, k0, imt, is, bin)
!         if (flaglib) then
!           write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FMTE(lib, k0, imt, is, bin), &
!&            NpointsMTE(lib, k0, imt, is, bin), NsetsMTE(lib, k0, imt, is, bin), lib = 1, Nlibs)
!           fstring = trim(fstring) //setstring
!         endif
!         write(9, '(a)') trim(fstring)
!       enddo
!       write(9, '(a)') trim(headstring2(k0))
!       write(9, '(a)') trim(secstring(k0))
!       close(unit = 9)
      enddo
    enddo
    open (unit = 1, status = 'unknown', file = subdir//'total/'//partype(k0)//'-MT.F')
    topline = 'F per reaction summed over all nuclides for '//parname(k0)
    quantity = 'F'
    call write_header(indent,topline,source,user,date,oformat)
    call write_reaction(indent,'('//partype(k0)//',x)',0.D0,0.D0,0,0)
    call write_real(id2,'Maximum F',Fmax)
    call write_integer(id2,'Subentries',Nsetspar(2,k0))
    call write_char(id2,'parameters','')
    call write_integer(id4,'Number of sets',Nsetspar(2, k0))
    call write_integer(id4,'Number of points',Npointspar(2, k0))
    do lib= 1, Nlibs
      call write_real(id4,'F for '//trim(library(lib)),Fpar(lib, k0))
    enddo
    Np = 0
    un = ''
    col = ''
    col(1) = 'MT'
    col(2) = 'iso'
    do lib = 1, Nlibs
      col(2+lib) = trim(library(lib))
    enddo
    col(2+Nlibs+1) = 'sets'
    col(2+Nlibs+2) = 'points'
    Ncol=2+Nlibs+2
    call write_quantity(id2,quantity)
    call write_datablock(id2,Ncol,Np,col,un)
    do MT = 1, nummt
      if ( .not. MTexist(k0, MT, - 1)) cycle
      imt = MTix(MT)
      do is = - 1, numisom
        if ( .not. MTexist(k0, MT, is)) cycle
        if (NpointsMT(2, k0, imt, is) == 0) cycle
        write(1, '(2(6x,i6, 3x), 8es15.6, 2(6x,i6, 3x))') MT, is, (FMT(lib, k0, imt, is), lib= 1, Nlibs), &
 &        NpointsMT(2, k0, imt, is), NsetsMT(2, k0, imt, is)
      enddo
    enddo
    close (unit = 1)
!   write(fstring, '(300(" "))')
!   write(fstring, '("        Total: ", 1p, g10.3, 0p, i7, i6)') Fpar(pointref, k0), Npointspar(pointref, k0), &
!&    Nsetspar(pointref, k0)
!   if ( flaglib) then
!     write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (Fpar(lib, k0), Npointspar(lib, k0), Nsetspar(lib, k0), lib = 1, Nlibs)
!     fstring = trim(fstring) //setstring
!   endif
!   write(11, '(a)') trim(headstring4(k0))
!   write(11, '(a)') trim(secstring(k0))
!   write(11, '(a)') trim(fstring)
!   close (unit = 11)
    close (unit = 19)
!
! Per particle, Z and A
!
    open (unit = 1, status = 'unknown',  file = subdir//'total/'//partype(k0)//'-nuc.F')
    topline = 'F per nucleus summed over all reactions for '//parname(k0)
    quantity = 'F'
    call write_header(indent,topline,source,user,date,oformat)
    un = ''
    col = ''
    col(1) = 'Z'
    col(2) = 'A'
    col(3) = 'iso'
    do lib = 1, Nlibs
      col(3+lib) = trim(library(lib))
    enddo
    col(3+Nlibs+1) = 'sets'
    col(3+Nlibs+2) = 'points'
    Ncol=3+Nlibs+2
    Np = 0
    call write_quantity(id2,quantity)
    call write_datablock(id2,Ncol,Np,col,un)
    do iz = 1, numZ
      do ia = 0, numA
        if (Npointsnuc(2, k0, iz, ia) == 0) cycle
        write(1, '(2(6x,i6,3x), 8es15.6, 2(6x,i6,3x))') iz, ia, &
 &        (Fnuc(lib, k0, iz, ia), lib=1, Nlibs), Nsetsnuc(2, k0, iz, ia), Npointsnuc(2, k0, iz, ia)
      enddo
    enddo
    close(unit = 1)
!   open (unit = 12, status = 'unknown', file = subdir//'total/'//partype(k0)//'-nuc.F')
!   write(12, '("# Average F values per nucleus summed over all reactions for ", a8, " for all ", i6, " subentries with F < ", &
!&    1p, g10.3)') parname(k0), Nsetspar(pointref, k0), Fmax
!   write(12, '(a)') trim(headstring3(k0))
!   write(12, '(a)') trim(secstring(k0))
!   write(fstring, '(300(" "))')
!   do iz = 1, numZ
!     do ia = 0, numA
!       if (Npointsnuc(pointref, k0, iz, ia) == 0) cycle
!       write(fstring, '(2i4, 4x, 1p, g10.3, 0p, i7, i6)') iz, ia, &
!&        Fnuc(0, k0, iz, ia), Npointsnuc(pointref, k0, iz, ia), Nsetsnuc(pointref, k0, iz, ia)
!       if (flaglib) then
!         write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (Fnuc(lib, k0, iz, ia), Npointsnuc(lib, k0, iz, ia), &
!&          Nsetsnuc(lib, k0, iz, ia), lib = 1, Nlibs)
!         fstring = trim(fstring) //setstring
!       endif
!       write(12, '(a)') trim(fstring)
!     enddo
!   enddo
!   write(fstring, '(300(" "))')
!   write(fstring, '("     Total: ", 1p, g10.3, 0p, i7, i6)') Fpar(pointref, k0), Npointspar(pointref, k0), Nsetspar(pointref, k0)
!   if (flaglib) then
!     write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (Fpar(lib, k0), Npointspar(lib, k0), Nsetspar(lib, k0), lib = 1, Nlibs)
!     fstring = trim(fstring) //setstring
!   endif
!   write(12, '(a)') trim(headstring3(k0))
!   write(12, '(a)') trim(secstring(k0))
!   write(12, '(a)') trim(fstring)
!   close(unit = 12)
    FparEfile = subdir//'parE/'//partype(k0)//'-par.F'
    open (unit = 1, status = 'unknown',  file = FparEfile)
    topline = 'F per energy bin for '//parname(k0)//' and all nuclides for all subentries'
    quantity = 'F'
    call write_header(indent,topline,source,user,date,oformat)
    call write_reaction(indent,'('//partype(k0)//',x)',0.D0,0.D0,0,0)
    call write_real(id2,'Maximum F',Fmax)
    un = ''
    col = ''
    col(1) = 'E'
    un(1) = 'MeV'
    do lib = 1, Nlibs
      col(1+lib) = trim(library(lib))
    enddo
    col(1+Nlibs+1) = 'sets'
    col(1+Nlibs+2) = 'points'
    Ncol=1+Nlibs+2
    call write_quantity(id2,quantity)
    call write_datablock(id2,Ncol,numEbin,col,un)
    do bin = 1, numEbin
      write(1, '(9es15.6, 2(6x,i6,3x))') ebin(bin), (FparE(lib, k0, bin), lib = 1, Nlibs), NsetsparE(2, k0, bin), &
 &      NpointsparE(2, k0, bin)
    enddo
    close(unit = 1)
!   open (unit = 9, status = 'unknown', file = FparEfile)
!   write(9, '("# Average F values per energy bin for ", a8, " and all nuclides for all subentries with F < ", 1p, g10.3)') &
!&    parname(k0), Fmax
!   write(9, '(a)') trim(headstring2(k0))
!   write(9, '(a)') trim(secstring(k0))
!   write(fstring, '(300(" "))')
!   do bin = 1, numEbin
!     write(fstring, '(1p, g12.4, g10.3, 0p, i7, i6)') ebin(bin), FparE(0, k0, bin), NpointsparE(0, k0, bin), NsetsparE(0, k0, bin)
!     if (flaglib) then
!       write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FparE(lib, k0, bin), NpointsparE(lib, k0, bin), &
!&        NsetsparE(lib, k0, bin), lib = 1, Nlibs)
!       fstring = trim(fstring) //setstring
!     endif
!     write(9, '(a)') trim(fstring)
!   enddo
!   write(9, '(a)') trim(headstring2(k0))
!   write(9, '(a)') trim(secstring(k0))
!   close(unit = 9)
  enddo
!
! Total
!
  FallEfile = subdir//'parE/all.F'
  open (unit = 1, status = 'unknown',  file = FallEfile)
  topline = 'F per energy bin for all nuclides for all subentries'
  quantity = 'F'
  call write_header(indent,topline,source,user,date,oformat)
  call write_quantity(indent,quantity)
  call write_real(id2,'Maximum F',Fmax)
  call write_datablock(id2,Ncol,numEbin,col,un)
  do bin = 1, numEbin
    write(1, '(9es15.6, 2(6x,i6,3x))') ebin(bin), (FallE(lib, bin), lib = 1, Nlibs), NsetsallE(2, bin), NpointsallE(2, bin)
  enddo
  close(unit = 1)
! write(fstring, '(300(" "))')
! open (unit = 9, status = 'unknown', file = FallEfile)
! write(9, '("# Average F values per energy bin for all nuclides", " for all subentries with F < ", 1p, g10.3)') Fmax
! write(9, '(a)') trim(headstring2(1))
! write(9, '(a)') trim(secstring(1))
! write(fstring, '(300(" "))')
! do bin = 1, numEbin
!   write(fstring, '(1p, g12.4, g10.3, 0p, i7, i6)') ebin(bin), FallE(0, bin), NpointsallE(0, bin), NsetsallE(0, bin)
!   if (flaglib) then
!     write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (FallE(lib, bin), NpointsallE(lib, bin), NsetsallE(lib, bin), lib = 1, Nlibs)
!     fstring = trim(fstring) //setstring
!   endif
!   write(9, '(a)') trim(fstring)
! enddo
! close(unit = 9)
  open (unit = 1, status = 'unknown',  file = subdir//'total/all.F')
  topline = 'F per projectile summed over all reactions and nuclides'
  quantity = 'F'
  call write_header(indent,topline,source,user,date,oformat)
  call write_char(indent,'parameters','')
  call write_real(id2,'Maximum F',Fmax)
  call write_integer(id2,'Subentries',Nsetsall(pointref))
  call write_integer(id2,'Number of sets',Nsetsall(2))
  call write_integer(id2,'Number of points',Npointsall(2))
  do lib= 1, Nlibs
    call write_real(id2,'F for '//trim(library(lib)),Fall(lib))
  enddo
  col(1) = 'projectile'
  un(1) = ''
  call write_quantity(id2,quantity)
  call write_datablock(id2,Ncol,numpar+1,col,un)
  do k0 = 0, numpar
    write(1, '(a8,7x, 8es15.6, 2(6x,i6,3x))') parname(k0), (Fpar(lib, k0), lib = 1, Nlibs), Nsetspar(2, k0), Npointspar(2, k0)
  enddo
  close(unit = 1)
! open (unit = 10, status = 'unknown', file = subdir//'total/all.F')
! write(10, '("# Average F values per projectile summed over all reactions and nuclides for all ", i6" subentries with F < ", &
!&  1p, g10.3)') Nsetsall(pointref), Fmax
! write(10, '(a)') trim(headstring5(1))
! write(10, '(a)') trim(secstring(1))
! write(fstring, '(300(" "))')
! do k0 = 0, numpar
!   if ( .not. parinclude(k0)) cycle
!   write(fstring, '(a8, 4x, 1p, g10.3, 0p, i7, i6)') parname(k0), Fpar(0, k0), Npointspar(pointref, k0), Nsetspar(pointref, k0)
!   if (flaglib) then
!     write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (Fpar(lib, k0), Npointspar(lib, k0), Nsetspar(lib, k0), lib = 1, Nlibs)
!     fstring = trim(fstring) //setstring
!   endif
!   write(10, '(a)') trim(fstring)
! enddo
! write(fstring, '(300(" "))')
! write(fstring, '("     Total: ", 1p, g10.3, 0p, i7, i6)') Fall(0), Npointsall(pointref), Nsetsall(pointref)
! if (flaglib) then
!   write(setstring, '(8(1p, g10.3, 0p, i7, i6))') (Fall(lib), Npointsall(lib), Nsetsall(lib), lib = 1, Nlibs)
!   fstring = trim(fstring) //setstring
! endif
! write(10, '(a)') trim(fstring)
! close (10)
!
! Entries
!
  headstring1(1) = '#ENTRY     AUTHOR    YEAR    World  '// '                          Quality'
  write(fstring, '(300(" "))')
  write(setstring, '(300(" "))')
  do ie = 1, Nentry_xc5
    ystr = '    '
    write(ystr(1:4), '(i4.4)') year_xc5
    entryfile = trim(authorcode)//'-'//trim(entry_xc5)//'.' //ystr
    open (unit = 7, status = 'unknown', file = subdir//'entry/'//entryfile)
    write(7, '(a)') trim(headstring1(1))
    write(fstring, '(a5, 1x, a14, 1x, i4, " F = ", 1p, g10.3, " T1:", i2, " T2:", i2, " T3:", i2, " N1:", i2, " N2:", &
 &    i2, " N3:", i2, " R1:", i2, " R2:", i2, " R3:", i2)') entry_xc5, author1_xc5, year_xc5, Fentry(0, ie), &
 &      ((Qentry(ie, k, m), m = 1, 3), k = 1, 3)
    write(7, '(a)') trim(fstring)
    headstring2(1) = "#SUBENT     N         " // "    Reaction            F    Quality" // &
 &    "    World   Exp     Dexp(%)    E-min     E-max"
    if (flaglib) headstring2(1) = trim(headstring2(1))//'     '//libstring
    write(7, '(a)') trim(headstring2(1))
    do i = 1, Nsub_xc5
      if (entry_sub(ie)(1:5) == subentry_xc5(1:5)) then
        write(fstring, '(a9, i4, 1x, a30, 1p, g10.3, " ", a2, 5g10.3, 0p)') subentry_xc5, Npoints(0), reaction_xc5, &
 &        Fset(0), Qscore, xsthset(0), xsexpav(1), uncertainty(1), Emin, Emax
        if (flaglib) then
          write(setstring, '(4x, 1p, 8g10.3)') (Fset(lib), lib = 1, Nlibs)
          fstring = trim(fstring) //setstring
        endif
        write(7, '(a)') trim(fstring)
      endif
    enddo
    write(7, '(a)') trim(headstring2(1))
    write(tailstring, '(34x, "Average:  ", 1p, g10.3, 54x)') Fentry(0, ie)
    if (flaglib) then
      write(setstring, '(3x, 1p, 8g10.3)') (Fentry(lib, ie), lib = 1, Nlibs)
      tailstring = tailstring(1:104) //setstring
    endif
    write(7, '(a)') trim(tailstring)
    close (7)
  enddo
!
! Final statistics table in Latex format
!
  do k0 = 0, numpar
    if ( .not. parinclude(k0)) cycle
    statfile = 'n-table.tex'
    write(statfile(1:1), '(a1)') partype(k0)
    Fstatfile = subdir//'total/'//statfile
    open (unit = 8, status = 'unknown', file = Fstatfile)
    write(8, '(" Reaction  & All    &Compared& F $<$ 5&", " T1     & T2     & T3     & N1     & N2     & N3     & ", &
 &    "R1     & R2     & R3     & E1     & E2     & E3     & ", "Reviewed \\")')
    do MT = 1, nummt
      if ( .not. MTexist_xc5(MT)) cycle
      imt = MTix(MT)
      do is = - 1, numisom
        if ( .not. MTexist(k0, MT, is)) cycle
        write(reacid(MT, is)(2:2), '(a1)') partype(k0)
        write(8, '(a10, 15(a3, i6), a3, a1,"\\")') reacid(MT, is), amp, NsetsallMT(k0, MT, is), amp, MTsum(k0, imt, is), amp, &
 &        NsetsMT(pointref, k0, imt, is), ((amp, QMT(k0, MT, is, i, j), j = 1, 3), i = 1, 4), amp, processed(k0, MT, is)
      enddo
    enddo
    write(8, '(" Total    ", 3(a3, i6), 12(a3, i6), a3,"\\")') amp, NsetsallMTtot(k0), amp, MTsumtot(k0), amp, &
 &    NsetsMTtot(k0), ((amp, QMTall(k0, i, j), j = 1, 3), i = 1, 4), amp
    close (8)
  enddo
  write( * , * ) "Writing F values done"
  return
end subroutine fout
! Copyright A.J. Koning 2019
