subroutine reacinitial
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Initialization of nuclear reaction info
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
!              base, &        ! base number for RMS
!              ebin, &        ! lower bound of energy bin
!              ecbin, &       ! energy bin
!              flagcendl, &   ! flag to include or exclude CENDL from library average
!              flageaf, &     ! flag to include or exclude EAF from library average
!              flagendfb, &   ! flag to include or exclude ENDFB from library average
!              flagexpo, &    ! flag to use exponential RMS instead of power of 10
!              flagirdff, &   ! flag to include or exclude IRDFF from library average
!              flagjeff, &    ! flag to include or exclude JEFF from library average
!              flagjendl, &   ! flag to include or exclude JENDL from library average
!              flagtendl, &   ! flag to include or exclude TENDL from library average
!              flagunc, &     ! flag for uncertainty analysis
!              Fsigma, &      ! deviation for F factor
!              isochar, &     ! character for isomer
!              libinclude, &  ! flag to include library
!              library, &     ! nuclear data library
!              libspath, &    ! directory containing data libraries
!              libstring, &   ! string with library names
!              libstring2, &  ! string with library names
!              libweight, &   ! weight per nuclear data library
!              limit, &       ! limit for exponent in RMS
!              MTrp, &        ! MT number for residual production
!              Nmod, &        ! number for output of progress of processing
!              numbin, &      ! maximum number of bins
!              numisom, &     ! maximum number of isomers
!              numlib, &      ! maximum number of data libraries
!              nummt, &       ! maximum number of MT numbers
!              processed, &   ! flag for processed subentry
!              reac, &        ! reaction identifier
!              reacid, &      ! reaction string
!              talfile, &     ! name of TALYS output file
!              uncbin         ! uncertainty bin
!
! *** Declaration of local data
!
  implicit none
  character(len=10) :: rtmp    !
  integer           :: i       ! counter
  integer           :: is      !
  integer           :: j       ! counter
  integer           :: lib     !
  integer           :: MT      !
!
! ************************ Initialization ******************************
!
! Energy grid for binning
!
  ebin(0) = 0.
  ebin(1) = 2.53e-8
  ebin(2) = 1.e-6
  ebin(3) = 1.e-5
  ebin(4) = 1.e-4
  ebin(5) = 0.001
  ebin(6) = 0.002
  ebin(7) = 0.005
  ebin(8) = 0.01
  ebin(9) = 0.02
  ebin(10) = 0.05
  ebin(11) = 0.1
  ebin(12) = 0.2
  ebin(13) = 0.5
  ebin(14) = 1.0
  ebin(15) = 1.5
  ebin(16) = 2.0
  ebin(17) = 3.0
  ebin(18) = 4.0
  ebin(19) = 5.0
  ebin(20) = 6.0
  ebin(21) = 7.0
  ebin(22) = 8.0
  ebin(23) = 9.0
  ebin(24) = 10.0
  ebin(25) = 11.0
  ebin(26) = 12.0
  ebin(27) = 13.0
  ebin(28) = 14.0
  ebin(29) = 15.0
  ebin(30) = 16.0
  ebin(31) = 18.0
  ebin(32) = 20.0
  ebin(33) = 25.0
  ebin(34) = 30.0
  ebin(35) = 40.0
  ebin(36) = 60.0
  ebin(37) = 100.0
  ebin(38) = 200.0
  ebin(39) = 500.0
  ebin(40) = 1000.0
  ecbin = ebin
  do i = 1, 11
    ecbin(i) = - 11. + i
  enddo
!
! Set TALYS output filenames and reaction identifiers
!
  talfile(1) = 'totalxs.tot'
  talfile(2) = 'elastic.tot'
  talfile(3) = 'nonelastic.tot'
  talfile(4) = 'xs100000.tot'
  talfile(11) = 'xs201000.tot'
  talfile(16) = 'xs200000.tot'
  talfile(17) = 'xs300000.tot'
  talfile(18) = 'fission.tot'
  talfile(22) = 'xs100001.tot'
  talfile(23) = 'xs100003.tot'
  talfile(24) = 'xs200001.tot'
  talfile(25) = 'xs300001.tot'
  talfile(28) = 'xs110000.tot'
  talfile(29) = 'xs100002.tot'
  talfile(30) = 'xs200002.tot'
  talfile(32) = 'xs101000.tot'
  talfile(33) = 'xs100100.tot'
  talfile(34) = 'xs100010.tot'
  talfile(35) = 'xs101002.tot'
  talfile(36) = 'xs100102.tot'
  talfile(37) = 'xs400000.tot'
  talfile(41) = 'xs210000.tot'
  talfile(42) = 'xs310000.tot'
  talfile(44) = 'xs120000.tot'
  talfile(45) = 'xs110001.tot'
  talfile(51) = 'nn.L01'
  talfile(52) = 'nn.L02'
  talfile(53) = 'nn.L03'
  talfile(54) = 'nn.L04'
  talfile(55) = 'nn.L05'
  talfile(56) = 'nn.L06'
  talfile(57) = 'nn.L07'
  talfile(58) = 'nn.L08'
  talfile(59) = 'nn.L09'
  talfile(60) = 'nn.L10'
  talfile(61) = 'nn.L11'
  talfile(62) = 'nn.L12'
  talfile(63) = 'nn.L13'
  talfile(64) = 'nn.L14'
  talfile(65) = 'nn.L15'
  talfile(66) = 'nn.L16'
  talfile(67) = 'nn.L17'
  talfile(68) = 'nn.L18'
  talfile(69) = 'nn.L19'
  talfile(70) = 'nn.L20'
  talfile(71) = 'nn.L21'
  talfile(72) = 'nn.L22'
  talfile(73) = 'nn.L23'
  talfile(74) = 'nn.L24'
  talfile(75) = 'nn.L25'
  talfile(76) = 'nn.L26'
  talfile(77) = 'nn.L27'
  talfile(78) = 'nn.L28'
  talfile(79) = 'nn.L29'
  talfile(80) = 'nn.L30'
  talfile(81) = 'nn.L31'
  talfile(82) = 'nn.L32'
  talfile(83) = 'nn.L33'
  talfile(84) = 'nn.L34'
  talfile(85) = 'nn.L35'
  talfile(86) = 'nn.L36'
  talfile(87) = 'nn.L37'
  talfile(88) = 'nn.L38'
  talfile(89) = 'nn.L39'
  talfile(80) = 'nn.L40'
  talfile(101) = 'xs000000.tot'
  talfile(102) = 'xs000000.tot'
  talfile(103) = 'xs010000.tot'
  talfile(104) = 'xs001000.tot'
  talfile(105) = 'xs000100.tot'
  talfile(106) = 'xs000010.tot'
  talfile(107) = 'xs000001.tot'
  talfile(108) = 'xs000002.tot'
  talfile(109) = 'xs000003.tot'
  talfile(111) = 'xs020000.tot'
  talfile(112) = 'xs010001.tot'
  talfile(113) = 'xs000102.tot'
  talfile(114) = 'xs001002.tot'
  talfile(115) = 'xs011000.tot'
  talfile(116) = 'xs010100.tot'
  talfile(117) = 'xs001001.tot'
  talfile(201) = 'nprod.tot'
  talfile(202) = 'gprod.tot'
  talfile(203) = 'pprod.tot'
  talfile(204) = 'dprod.tot'
  talfile(205) = 'tprod.tot'
  talfile(206) = 'hprod.tot'
  talfile(207) = 'aprod.tot'
!
! reaction numbers
!
  reac(4) = 100000
  reac(11) = 201000
  reac(16) = 200000
  reac(17) = 300000
  reac(22) = 100001
  reac(23) = 100003
  reac(24) = 200001
  reac(25) = 300001
  reac(28) = 110000
  reac(29) = 100002
  reac(30) = 200002
  reac(32) = 101000
  reac(33) = 100100
  reac(34) = 100010
  reac(35) = 101002
  reac(36) = 100102
  reac(37) = 400000
  reac(41) = 210000
  reac(42) = 310000
  reac(44) = 120000
  reac(45) = 110001
  reac(101) = 000000
  reac(102) = 000000
  reac(103) = 010000
  reac(104) = 001000
  reac(105) = 000100
  reac(106) = 000010
  reac(107) = 000001
  reac(108) = 000002
  reac(109) = 000003
  reac(111) = 020000
  reac(112) = 010001
  reac(113) = 000102
  reac(114) = 001002
  reac(115) = 011000
  reac(116) = 010100
  reac(117) = 001001
!
! Ejectiles from CN per MT number
!
  zejec = 0
  nejec = 0
  zejec(4) = 0
  nejec(4) = 1
  zejec(11) = 1
  nejec(11) = 3
  zejec(16) = 0
  nejec(16) = 2
  zejec(17) = 0
  nejec(17) = 3
  zejec(22) = 2
  nejec(22) = 3
  zejec(23) = 6
  nejec(23) = 7
  zejec(24) = 2
  nejec(24) = 4
  zejec(25) = 2
  nejec(25) = 5
  zejec(28) = 1
  nejec(28) = 1
  zejec(29) = 4
  nejec(29) = 5
  zejec(30) = 4
  zejec(30) = 6
  zejec(32) = 1
  nejec(32) = 2
  zejec(33) = 1
  nejec(33) = 3
  zejec(34) = 2
  nejec(34) = 2
  zejec(35) = 5
  nejec(35) = 6
  zejec(36) = 5
  nejec(36) = 7
  zejec(37) = 0
  nejec(37) = 4
  zejec(41) = 1
  nejec(41) = 2
  zejec(42) = 1
  nejec(42) = 3
  zejec(44) = 2
  nejec(44) = 1
  zejec(45) = 3
  nejec(45) = 3
  zejec(101) = 0
  nejec(101) = 0
  zejec(102) = 0
  nejec(102) = 0
  zejec(103) = 1
  nejec(103) = 0
  zejec(104) = 1
  nejec(104) = 1
  zejec(105) = 1
  nejec(105) = 2
  zejec(106) = 2
  nejec(106) = 1
  zejec(107) = 2
  nejec(107) = 2
  zejec(108) = 4
  nejec(108) = 4
  zejec(109) = 6
  nejec(109) = 6
  zejec(111) = 2
  nejec(111) = 0
  zejec(112) = 3
  nejec(112) = 2
  zejec(113) = 6
  nejec(113) = 5
  zejec(114) = 5
  nejec(114) = 5
  zejec(115) = 2
  nejec(115) = 1
  zejec(116) = 2
  nejec(116) = 2
  zejec(117) = 3
  nejec(117) = 3
!
! reaction strings
!
  reacstring = ' '
  reacstring(1) = 'n-tot    '
  reacstring(2) = 'n-el     '
  reacstring(3) = 'n-non    '
  reacstring(4) = 'n-inl    '
  reacstring(11) = 'n-2nd    '
  reacstring(16) = 'n-2n     '
  reacstring(17) = 'n-3n     '
  reacstring(18) = 'n-f      '
  reacstring(22) = 'n-na     '
  reacstring(23) = 'n-n3a    '
  reacstring(24) = 'n-2na    '
  reacstring(25) = 'n-3na    '
  reacstring(28) = 'n-np     '
  reacstring(29) = 'n-n2a    '
  reacstring(30) = 'n-2n2a   '
  reacstring(32) = 'n-nd     '
  reacstring(33) = 'n-nt     '
  reacstring(34) = 'n-nh     '
  reacstring(35) = 'n-nd2a   '
  reacstring(36) = 'n-nt2a   '
  reacstring(37) = 'n-4n)    '
  reacstring(41) = 'n-2np    '
  reacstring(42) = 'n-3np    '
  reacstring(44) = 'n-n2p    '
  reacstring(45) = 'n-npa    '
  reacstring(50) = 'n-n00    '
  reacid = ' '
  reacid(1, -1) = '(n,tot)   '
  reacid(2, -1) = '(n,el)    '
  reacid(3, -1) = '(n,non)   '
  reacid(4, -1) = "(n,n')    "
  reacid(11, -1) = '(n,2nd)   '
  reacid(16, -1) = '(n,2n)    '
  reacid(17, -1) = '(n,3n)    '
  reacid(18, -1) = '(n,f)     '
  reacid(22, -1) = '(n,na)    '
  reacid(23, -1) = '(n,n3a)   '
  reacid(24, -1) = '(n,2na)   '
  reacid(25, -1) = '(n,3na)   '
  reacid(28, -1) = '(n,np)    '
  reacid(29, -1) = '(n,n2a)   '
  reacid(30, -1) = '(n,2n2a)  '
  reacid(32, -1) = '(n,nd)    '
  reacid(33, -1) = '(n,nt)    '
  reacid(34, -1) = '(n,nh)    '
  reacid(35, -1) = '(n,nd2a)  '
  reacid(36, -1) = '(n,nt2a)  '
  reacid(37, -1) = '(n,4n)    '
  reacid(41, -1) = '(n,2np)   '
  reacid(42, -1) = '(n,3np)   '
  reacid(44, -1) = '(n,n2p)   '
  reacid(45, -1) = '(n,npa)   '
  reacid(50, -1) = "(n,n'_00) "
  do i = 1, 40
    reacid(50+i, -1) = "(n,n'_01) "
    write(reacid(50+i, -1)(7:8), '(i2.2)') i
    write(reacstring(50+i)(3:4), '(i2.2)') i
  enddo
  reacstring(101) = 'n-abs    '
  reacstring(102) = 'n-g      '
  reacstring(103) = 'n-p      '
  reacstring(104) = 'n-d      '
  reacstring(105) = 'n-t      '
  reacstring(106) = 'n-h      '
  reacstring(107) = 'n-a      '
  reacstring(108) = 'n-2a     '
  reacstring(109) = 'n-3a     '
  reacstring(111) = 'n-2p     '
  reacstring(112) = 'n-pa     '
  reacstring(113) = 'n-t2a    '
  reacstring(114) = 'n-d2a    '
  reacstring(115) = 'n-pd     '
  reacstring(116) = 'n-pt     '
  reacstring(117) = 'n-da     '
  reacstring(201) = 'n-xn     '
  reacstring(202) = 'n-xg     '
  reacstring(203) = 'n-xp     '
  reacstring(204) = 'n-xd     '
  reacstring(205) = 'n-xt     '
  reacstring(206) = 'n-xh     '
  reacstring(207) = 'n-xa     '
  reacid(101, -1) = '(n,abs)   '
  reacid(102, -1) = '(n,g)     '
  reacid(103, -1) = '(n,p)     '
  reacid(104, -1) = '(n,d)     '
  reacid(105, -1) = '(n,t)     '
  reacid(106, -1) = '(n,h)     '
  reacid(107, -1) = '(n,a)     '
  reacid(108, -1) = '(n,2a)    '
  reacid(109, -1) = '(n,3a)    '
  reacid(111, -1) = '(n,2p)    '
  reacid(112, -1) = '(n,pa)    '
  reacid(113, -1) = '(n,t2a)   '
  reacid(114, -1) = '(n,d2a)   '
  reacid(115, -1) = '(n,pd)    '
  reacid(116, -1) = '(n,pt)    '
  reacid(117, -1) = '(n,da)    '
  reacid(201, -1) = '(n,xn)    '
  reacid(202, -1) = '(n,xg)    '
  reacid(203, -1) = '(n,xp)    '
  reacid(204, -1) = '(n,xd)    '
  reacid(205, -1) = '(n,xt)    '
  reacid(206, -1) = '(n,xh)    '
  reacid(207, -1) = '(n,xa)    '
  reacid(151, -1) = '(n,res)   '
  reacid(452, -1) = '(n,nutot) '
  reacid(455, -1) = '(n,nudel) '
  reacid(456, -1) = '(n,nuprm) '
  do i = 0, 40
    do j = 600, 800, 50
      reacid(j + i, - 1) = "(n,p_00)  "
      write(reacid(j+i, -1)(6:7), '(i2.2)') i
      write(reacstring(j+i)(5:6), '(i2.2)') i
    enddo
  enddo
  MTrp=min(nummt,998)
  MTrp=5
  reacid(MTrp, -1) = '(n,x)     '
  reacstring(MTrp) = 'n-x      '
  isochar(-1) = ' '
  isochar(0) = 'g'
  isochar(1) = 'm'
  isochar(2) = 'n'
  do MT = 1, nummt
    rtmp = reacid(MT, - 1)
    do is = 0, numisom
      reacid(MT, is) = trim(rtmp) //isochar(is)
      reacstring(MT) = trim(reacstring(MT)) //'-'//isochar(is)
    enddo
  enddo
!
! Make bins to store experimental uncertainties
!
  if (flagunc) then
    do j = 0, 20
      uncbin(j) = j * 5.
    enddo
    do j = 21, 40
      uncbin(j) = 100. + (j - 20) * 10.
    enddo
    do j = 41, numbin
      uncbin(j) = 300. + 100. * (10 **(0.1 * (j - 40)))
    enddo
  endif
!
! Set names of nuclear data libraries
!
  library(0) = 'World'
  library(1) = 'TALYS'
  library(2) = 'tendl.2025'
  library(3) = 'endfb8.1'
  library(4) = 'jeff4.0'
  library(5) = 'jendl5.0'
  library(6) = 'cendl3.2'
  library(7) = 'eaf.2010'
  library(8) = 'irdff2.0'
  write(libstring, '(8a10)') (library(lib), lib = 1, 8)
  write(libstring2, '(8a23)') (library(lib), lib = 1, 8)
  if (flagtendl) libinclude(2) = .true.
  if (flagendfb) libinclude(3) = .true.
  if (flagjeff) libinclude(4) = .true.
  if (flagjendl) libinclude(5) = .true.
  if (flagcendl) libinclude(6) = .true.
  if (flageaf) libinclude(7) = .true.
  if (flagirdff) libinclude(8) = .true.
  libweight(1) = 3.
  libweight(2) = 3.
  libweight(3) = 6.
  libweight(4) = 3.
  libweight(5) = 5.
  libweight(6) = 5.
  libweight(7) = 2.
  libweight(8) = 10.
!
! Other initializations
!
  Nmod = 1000
  if (flagexpo) then
    base = exp(1.)
    limit = 80.
  else
    base = 10.
    limit = 35.
  endif
!
! 1-sigma and 2-sigma boundaries for F values per MT number, used
! for quality assignment
!
  Fsigma = 0.
  Fsigma(1, -1, 1) = 1.38
  Fsigma(1, -1, 2) = 4.03
  Fsigma(2, -1, 1) = 1.22
  Fsigma(2, -1, 2) = 2.26
  Fsigma(3, -1, 1) = 1.27
  Fsigma(3, -1, 2) = 11.2
  Fsigma(4, -1, 1) = 1.49
  Fsigma(4, -1, 2) = 6.17
  Fsigma(4, 1, 1) = 1.51
  Fsigma(4, 1, 2) = 3.53
  Fsigma(16, -1, 1) = 1.26
  Fsigma(16, -1, 2) = 2.52
  Fsigma(16, 0, 1) = 1.40
  Fsigma(16, 0, 2) = 2.42
  Fsigma(16, 1, 1) = 1.38
  Fsigma(16, 1, 2) = 3.59
  Fsigma(17, -1, 1) = 2.27
  Fsigma(17, -1, 2) = 23.6
  Fsigma(18, -1, 1) = 1.52
  Fsigma(18, -1, 2) = 14.8
  Fsigma(22, -1, 1) = 6.75
  Fsigma(22, -1, 2) = 80.7
  Fsigma(28, -1, 1) = 7.63
  Fsigma(28, -1, 2) = 121.5
  Fsigma(51, -1, 1) = 2.58
  Fsigma(51, -1, 2) = 21.1
  do MT = 52, 90
    Fsigma(MT, - 1, 1) = 3.34
    Fsigma(MT, - 1, 2) = 101.4
  enddo
  Fsigma(102, -1, 1) = 2.03
  Fsigma(102, -1, 2) = 39.5
  Fsigma(102, 0, 1) = 1.96
  Fsigma(102, 0, 2) = 37.8
  Fsigma(102, 1, 1) = 3.71
  Fsigma(102, 1, 2) = 70.2
  Fsigma(103, -1, 1) = 1.39
  Fsigma(103, -1, 2) = 3.56
  Fsigma(103, 0, 1) = 1.86
  Fsigma(103, 0, 2) = 8.90
  Fsigma(103, 1, 1) = 1.68
  Fsigma(103, 1, 2) = 7.62
  Fsigma(104, -1, 1) = 2.98
  Fsigma(104, -1, 2) = 14.9
  Fsigma(105, -1, 1) = 2.18
  Fsigma(105, -1, 2) = 29.8
  Fsigma(106, -1, 1) = 16.9
  Fsigma(106, -1, 2) = 999.0
  Fsigma(107, -1, 1) = 1.98
  Fsigma(107, -1, 2) = 12.1
  Fsigma(107, 0, 1) = 2.58
  Fsigma(107, 0, 2) = 15.0
  Fsigma(107, 1, 1) = 2.75
  Fsigma(107, 1, 2) = 12.5
  Fsigma(201, -1, 1) = 1.79
  Fsigma(201, -1, 2) = 15.6
  Fsigma(203, -1, 1) = 1.43
  Fsigma(203, -1, 2) = 2.99
  Fsigma(204, -1, 1) = 1.83
  Fsigma(204, -1, 2) = 343.
  Fsigma(205, -1, 1) = 2.00
  Fsigma(205, -1, 2) = 159.
  Fsigma(207, -1, 1) = 2.07
  Fsigma(207, -1, 2) = 9.0
  Fsigma(MTrp, -1, 1) = 2.70
  Fsigma(MTrp, -1, 2) = 22.8
  do MT = 1, nummt
    if (Fsigma(MT, -1, 1) == 0.) Fsigma(MT, -1, 1) = 2.
    if (Fsigma(MT, -1, 2) == 0.) Fsigma(MT, -1, 2) = 5.
    do j = 1, 2
      if (Fsigma(MT, 1, j) == 0.) Fsigma(MT, 1, j) = Fsigma(MT, -1, j)
      if (Fsigma(MT, 0, j) == 0.) Fsigma(MT, 0, j) = Fsigma(MT, 1, j)
      if (Fsigma(MT, 2, j) == 0.) Fsigma(MT, 2, j) = Fsigma(MT, 1, j)
    enddo
  enddo
!
! Record which reactions have been processed
!
  processed = ' '
  do MT = 4, nummt
    if (MT /= 18 .and. MT /= 102) then
      do is = -1, numisom
        processed(1, MT, is) = 'y'
      enddo
    endif
  enddo
  return
end subroutine reacinitial
! Copyright A.J. Koning 2019
