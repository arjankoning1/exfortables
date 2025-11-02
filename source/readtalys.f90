subroutine readtalys
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read data from TALYS output files
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
! use A0_kinds_mod, only: & ! Definition of single and double precision variables
!              sgl            ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              A_xc5, &       ! mass number
!              E1mb, &        ! energy at 1 mb threshold
!              Enon, &        ! energy for non - elastic cross section
!              etal, &        ! energy of TALYS
!              flagMT, &      ! flag for MT cross section
!              flagres, &     ! flag for residual production cross section
!              flagtalys, &   ! flag for TALYS comparison
!              k0_xc5, &      ! incident particle
!              kres, &        ! type of outgoing particle
!              Liso, &        ! number of isomer
!              MT_xc5, &      ! MT number
!              Nnon, &        ! number of non - elastic energies
!              Ntal, &        ! number of TALYS points
!              numpoint, &    ! maximum number of data points in subentry
!              parA, &        ! mass number of particle
!              partype, &     ! symbol of particle
!              parZ, &        ! charge number of particle
!              reac, &        ! reaction identifier
!              talfile, &     ! name of TALYS output file
!              talyspath, &   ! directory containing TALYS results
!              xsnon, &       ! non - elastic cross section
!              xstal, &       ! cross section of TALYS
!              Z_xc5, &       ! charge number
!              ZAres          ! ZA of residual nucleus
!
! *** Declaration of local data
!
  implicit none
  logical            :: lexist            !
  character(len=6)   :: resstring         !
  character(len=16)  :: isofile           !
  character(len=132) :: isomfile          !
  character(len=132) :: line
  character(len=132) :: key
  character(len=132) :: reacpath          !
  character(len=132) :: talysfile         !
  integer            :: A                 !
  integer            :: i                 ! counter
  integer            :: istat             ! error code
  integer            :: j                 ! counter
  integer            :: jbeg              !
  integer            :: jend              !
  integer            :: jinc              !
  integer            :: k0                !
  integer            :: k1                !
  integer            :: keyix
  integer            :: lenreac           !
  integer            :: LL                !
  integer            :: MT                !
  integer            :: N1MeV             !
  integer            :: na                !
  integer            :: nd                !
  integer            :: nh                !
  integer            :: nn                !
  integer            :: Nout              !
  integer            :: np                !
  integer            :: nt                !
  integer            :: Z                 !
  integer            :: ZAr               !
  integer            :: Zout              !
  real(sgl)          :: denom             !
  real(sgl)          :: frac              !
  real(sgl)          :: xst(0:numpoint)   !
  real(sgl)          :: xsthresh          !
!
! ************************* Determine TALYS file name ******************
!
  Z = Z_xc5
  A = A_xc5
  MT = MT_xc5
  k0 = k0_xc5
  ZAr = ZAres
  E1mb = 0.
  etal = 0.
  xstal = 0.
  Nnon = 0
  Enon = 0.
  xsnon = 0.
  Ntal = 0
  talysfile = '                                                                            '
  reacpath = trim(talyspath)//proj//'/'//trim(nuclide_xc5)//'/tables/'
  lenreac = len_trim(reacpath)
  if (flagtalys) then
    if (flagMT) then
!
! Residual product for charged-particle reactions
!
      do
        if (k0 /= 1) then
          if (reac(MT) ==  -1) return
          na = mod(reac(MT), 10)
          nh = mod(reac(MT), 100) / 10
          nt = mod(reac(MT), 1000) / 100
          nd = mod(reac(MT), 10000) / 1000
          np = mod(reac(MT), 100000) / 10000
          nn = reac(MT) / 100000
          Zout = np + nd + nt + 2 * nh + 2 * na
          Nout = nn + nd + 2 * nt + nh + 2 * na
          ZAr = 1000 * (Z + parZ(k0) - Zout) + A + parA(k0) - (Zout + Nout)
        endif
        if (talfile(MT) == '                ') return
!
! Search for isomeric file
!
        if ((MT > 3 .and. MT < 50) .or. MT > 91) then
          LL = Liso
          if (LL == 0) then
            if (k0 == 1) then
              isofile = talfile(MT)
              write(isofile(10:12), '("L00")')
              isomfile = trim(reacpath)//'xs/'//isofile
            else
              isofile = 'rp000000.000'
              write(isofile(3:8), '(i6.6)') ZAr
              write(isofile(10:12), '("L00")')
              isomfile = trim(reacpath)//'residual/'//isofile
            endif
            inquire (file = isomfile, exist = lexist)
            if (lexist) talysfile = isomfile
            exit
          endif
          if (LL == 1) then
            jbeg = 1
            jend = 20
            jinc = 1
          endif
          if (LL == 2) then
            jbeg = 20
            jend = 1
            jinc = - 1
          endif
          if (LL == 1 .or. LL == 2) then
            do j = jbeg, jend, jinc
              if (k0 == 1) then
                isofile = talfile(MT)
                write(isofile(10:12), '("L", i2.2)') j
                isomfile = trim(reacpath)//'xs/'//isofile
              else
                isofile = 'rp000000.000'
                write(isofile(3:8), '(i6.6)') ZAr
                write(isofile(10:12), '("L", i2.2)') j
                isomfile = trim(reacpath)//'residual/'//isofile
              endif
              inquire (file = isomfile, exist = lexist)
              if (lexist) then
                talysfile = isomfile
                exit
              endif
            enddo
            exit
          endif
          if (k0 == 1) then
            talysfile = trim(reacpath)//'xs/'//talfile(MT)
          else
            isofile = 'rp000000.tot'
            write(isofile(3:8), '(i6.6)') ZAr
            talysfile = trim(reacpath)//'residual/'//isofile
          endif
        endif
        if (MT <= 3) talysfile = trim(reacpath)//'xs/'//talfile(MT)
        if (MT > 50 .and. MT <= 90) talysfile = trim(reacpath)//'xs/'//talfile(MT)
        if (MT >= 201 .and. MT <= 207) then
          k1 = kres
          talysfile = trim(reacpath)//'xs/'//partype(k1)// 'prod.tot'
        endif
        exit
      enddo
    endif
    if (flagres) then
      write(resstring(1:6), '(i6.6)') ZAr
      talysfile = trim(reacpath)//'residual/rp'//resstring(1:6)//'.tot'
    endif
!
! Read data
!
    Ntal = 0
    if (talysfile(1:1) == ' ') return
    inquire (file = talysfile, exist = lexist)
    if ( .not. lexist) return
    open (unit = 10, status = 'unknown', file = talysfile)
    do
      read(10,'(a)',iostat = istat) line
      if (istat == -1) exit
      key='entries'
      keyix=index(line,trim(key)) 
      if (keyix > 0) then
        read(line(keyix+len_trim(key)+2:80),*, iostat = istat) Ntal
        if (istat /= 0) call read_error(talysfile, istat)
        read(10,'(/)')
        xstal(0) = 0.
        xst(0) = 0.
        N1MeV = Ntal + 1
        do i = 1, Ntal
          xst(i) = xst(i - 1)
          xstal(i) = xst(i)
          etal(i) = 0.
          read(10, * , iostat = istat) etal(i), xst(i)
          if (istat /= 0) cycle
          xstal(i) = xst(i)
          if (etal(i) > 1. .and. N1MeV == Ntal + 1) N1MeV = i
        enddo
        exit
      endif
    enddo
    etal(0) = etal(1)
    xst(0) = xst(1)
    xstal(0) = xst(0)
    close (10)
!
! Search for energy where cross section crosses the threshold value of 1 mb.
!
    E1mb = 0.
    if (MT > 3 .and. MT /= 102) then
      xsthresh = 1.
      do i = N1MeV, Ntal - 1
        if (xst(i) <= xsthresh .and. xst(i + 1) > xsthresh) then
          frac = (xsthresh - xst(i)) / (xst(i + 1) - xst(i))
          denom = etal(i + 1) - etal(i)
          if (denom > 0.) E1mb = etal(i) + frac * denom
        endif
      enddo
    endif
  endif
!
! Read non-elastic cross section
!
  talysfile = trim(reacpath)//'total/nonelastic.tot'
  inquire (file = talysfile, exist = lexist)
  if ( .not. lexist) return
  open (unit = 10, status = 'unknown', file = talysfile)
  do
    read(10,'(a)',iostat = istat) line
    if (istat == -1) exit
    key='entries'
    keyix=index(line,trim(key)) 
    if (keyix > 0) then
      read(line(keyix+len_trim(key)+2:80),*, iostat = istat) Nnon
      if (istat /= 0) call read_error(talysfile, istat)
      read(10,'(/)')
      do i = 1, Nnon
        read(10, * , iostat = istat) Enon(i), xsnon(i)
        if (istat /= 0) cycle
      enddo
      exit
    endif
  enddo
  Enon(0) = 0.
  xsnon(0) = xsnon(1)
  close (10)
  return
end subroutine readtalys
! Copyright A.J. Koning 2019
