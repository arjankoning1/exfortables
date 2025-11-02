subroutine reactionsub
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Check XC5 subentry and determine reaction type
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
!              sgl             ! single precision kind
! use A1_exfortables_mod, only: & ! All global variables
!              Amax, &         ! maximal A value to process
!              Amin, &         ! minimal A value to process
!              Ares, &         ! residual A
!              flagconv, &     ! flag for conversion to new database
!              flagddx, &      ! flag for DDX emission spectrum
!              flagdisc, &     ! flag for discrete reaction
!              flagdiscang, &  ! flag for discrete angular distribution
!              flagelang, &    ! flag for elastic angular distribution
!              flagfy, &       ! flag for fission yields
!              flagMT, &       ! flag for MT cross section
!              flagnubar, &    ! flag for nubar
!              flagratio, &    ! flag for cross section ratio
!              flagres, &      ! flag for residual production cross section
!              flagrespar, &   ! flag for resonance parameters
!              flagri, &       ! flag for resonance integral
!              flagspec, &     ! flag for emission spectrum
!              flagspecav, &   ! flag for spectrum average
!              flagxsonly, &   ! flag to process only cross sections
!              frame, &        ! reference fram
!              isom_xc5, &     ! number of isomer
!              k0_xc5, &       ! incident particle
!              kres, &         ! type of outgoing particle
!              Liso, &         ! number of isomer
!              Lrat, &         ! isomer for ratio
!              MT_xc5, &       ! MT number
!              MTrp, &         ! MT number
!              mtinclude, &    ! flag to include MT number
!              MTrat, &        ! MT number for ratio
!              Nobs_xc5, &     ! number of obsolete cases
!              Npoints_xc5, &  ! number of points per subentry
!              NsetsallMT, &   ! all MT sets
!              Nsub_xc5, &     ! counter for XC5 subentries
!              nummt, &        ! maximum number of MT numbers
!              parinclude, &   ! flag to include particle
!              partype, &      ! symbol of particle
!              proj, &         ! projectile
!              reaction_xc5, & ! XC5 reaction
!              resiso, &       ! isomer of residual nuclide
!              string_xc5, &   ! date according to NDRC file
!              valstring, &    ! XC5 string with values
!              ZAP_xc5, &      ! projectile
!              ZAres, &        ! ZA of residual nucleus
!              Zmax, &         ! maximal Z value to process
!              Zmin, &         ! minimal Z value to process
!              Zres            ! residual Z
! use A1_error_handling_mod, only: & ! Error handling
!          read_error ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  character(len=1) :: riso        !
  character(len=1) :: xstatus     !
  character(len=1) :: frame0      !
  character(len=3) :: ident       !
  integer          :: A           !
  integer          :: Ar          !
  integer          :: i           ! counter
  integer          :: is          !
  integer          :: istat       ! error code
  integer          :: isubxc5     !
  integer          :: ix          !
  integer          :: k           ! counter
  integer          :: k0          !
  integer          :: k1          !
  integer          :: LL          !
  integer          :: MF          !
  integer          :: MT          !
  integer          :: Np          !
  integer          :: nx          !
  integer          :: imt         !
  integer          :: parsig      !
  integer          :: Z           !
  integer          :: ZAP         !
  integer          :: Zr          !
  real(sgl)        :: ex1         !
  real(sgl)        :: ex2         !
  real(sgl)        :: ratiso      !
  real(sgl)        :: ris         !
  real(sgl)        :: RMTrat      !
  real(sgl)        :: ZA          !
  real(sgl)        :: Zrr         !
  real(sgl)        :: Arr         !
!
! ***** Read projectile, Z, A, MF, MT, etc from first data line ********
!
  isubxc5 = Nsub_xc5
  Eindex = Eindexnew
  read(string_xc5(Eindex), '(i5, 2i3, 1x, i3, i4, 3a1, 8a9)') ZAP, Z, A,  MF, MT, riso, xstatus, frame0, (valstring(k), k = 1, 8)
  ident = string_xc5(Eindex)(95:97)
  resiso = riso
  if (riso == 'G') resiso='g'
  if (riso == 'M') resiso='m'
  if (riso == 'N') resiso='n'
  ZAP_xc5 = ZAP
!
! **** Test for info that we can and can not yet process ***************
!
! Amax    aximal A value to process
!
  if (Z == 0) return
  if (Z < Zmin .or. Z > Zmax) return
  if (A < Amin .or. A > Amax) return
  if (xstatus == 'S' .or. xstatus == 'O') then
    Nobs_xc5 = Nobs_xc5 + 1
    return
  endif
  if (Ndatasets == 0) return
!
! Special extensions of the reaction string
!
  LL = len_trim(reaction_xc5)
  if (reaction_xc5(LL-1:LL) == 'RA') return
  if (reaction_xc5(LL-2:LL) == 'RAW') return
  if (reaction_xc5(LL-2:LL) == 'MSC') return
  if (reaction_xc5(LL-3:LL) == 'CALC') return
  if (reaction_xc5(LL-3:LL) == 'EVAL') return
  if (reaction_xc5(LL-4:LL) == 'DERIV') return
  if (reaction_xc5(LL-4:LL) == 'RECOM') return
  ix = index(reaction_xc5, 'CN,SIG')
  if (ix > 0) return
  ix = index(reaction_xc5, 'CMP')
  if (ix > 0) return
  ix = index(reaction_xc5, '(AP,')
  if (ix > 0) return
  ix = index(reaction_xc5, '(E,')
  if (ix > 0) return
  ix = index(reaction_xc5, '(PIP,')
  if (ix > 0) return
  ix = index(reaction_xc5, '(PIN,')
  if (ix > 0) return
  ix = index(reaction_xc5, '0-ET-0')
  if (ix > 0) return
  ix = index(reaction_xc5, 'M1+M2')
  if (ix > 0) then
    riso='M'
    resiso='m'
  endif
!
! **** Determine ejectile, isomeric information and reference frame ****
!
  k0 = - 1
  if (parinclude(0) .and. ZAP == 0) k0 = 0
  if (parinclude(1) .and. ZAP == 1) k0 = 1
  if (parinclude(2) .and. ZAP == 1001) k0 = 2
  if (parinclude(3) .and. ZAP == 1002) k0 = 3
  if (parinclude(4) .and. ZAP == 1003) k0 = 4
  if (parinclude(5) .and. ZAP == 2003) k0 = 5
  if (parinclude(6) .and. ZAP == 2004) k0 = 6
  if (parinclude(7) .and. ZAP > 2004) k0 = 7
  if (k0 >= 0) proj = partype(k0)
  if (k0 ==  -1) return
  k0_xc5 = k0
  LL = 99
  if (riso == ' ') LL = 99
  if (riso == 'T') LL = 99
  if (riso == 'G') LL = 0
  if (riso == 'M') LL = 1
  if (riso == 'N') LL = 2
  if (riso == 'LL') LL = 1
  if (riso == '1') LL = 1
  if (riso == '2') LL = 2
  if (riso == '3') LL = 2
  if (riso == '4') LL = 2
  if (riso == '5') LL = 2
  if (riso == '?') LL = 1
  if (riso == '+') LL = 99
  if (frame0 == ' ') frame0 = 'L'
  frame = frame0
  Liso = LL
  if (LL == 0) isom_xc5 = 0
  if (LL == 1) isom_xc5 = 1
  if (LL == 2) isom_xc5 = 2
!
! ************************ Determine reaction type *********************
!
! Determine reaction type
!
  do
    parsig = index(reaction_xc5, 'PAR,SIG')
    if (MF == 203) then
      flagratio = .true.
      RMTrat = 0
      if (valstring(5) /= '         ') then
        read(valstring(5), * , iostat = istat) RMTrat
        if (istat /= 0) call read_error(valstring(5), istat, eor = 'continue', eof = 'continue')
      endif
      MTrat = int(RMTrat)
      ratiso = RMTrat - MTrat
      Lrat = 99
      if (ratiso == 0.) Lrat = 0
      if (ratiso == 0.1) Lrat = 1
      if (ratiso >= 0.2 .and. ratiso <= 0.8) Lrat = 2
      flagconv = .true.
      goto 100
    endif
    if (MF == 213) then
      flagri = .true.
      flagconv = .true.
      exit
    endif
    if (MF == 223) then
      flagspecav = .true.
      flagconv = .true.
      exit
    endif
    if (MF >= 401 .and. MF <= 405) then
      flagrespar = .true.
      flagconv = .true.
      imt = 151
      ix = 0
      ix = index(reaction_xc5, '(N,G),,WID,,AV')
      if (ix > 0 .and. MF == 405 .and. MT == 6001) imt = 102
      ix = index(reaction_xc5, '(N,0),,D')
      if (ix > 0 .and. MF == 402 .and. MT == 6004) imt = 101
      MT = imt
      MT_xc5 = MT
      exit
    endif
    if (MF == 801) then
      flagfy = .true.
      isom_xc5 = -1
      MT = 464
      ix = index(reaction_xc5, 'IND,FY')
      if (ix > 0) then
        MT = 454
      endif
      ix = index(reaction_xc5, ',CHG,FY')
      if (ix > 0) then
        MT = 454
      endif
      ix = index(reaction_xc5, 'SEC,FY')
      if (ix > 0) then
        MT = 454
      endif
      ix = index(reaction_xc5, ',CHN,FY')
      if (ix > 0) then
        MT = 459
      endif
      ix = index(reaction_xc5, 'CUM,FY')
      if (ix > 0) then
        MT = 459
      endif
      ix = index(reaction_xc5, 'PRE,FY')
      if (ix > 0) then
        MT = 460
      endif
      ix = index(reaction_xc5, 'TER,FY')
      if (ix > 0) then
        MT = 460
      endif
      ix = index(reaction_xc5, 'PRV,FY')
      if (ix > 0) then
        MT = 460
      endif
      ix = index(reaction_xc5, 'QTR,FY')
      if (ix > 0) then
        MT = 460
      endif
      ix = index(reaction_xc5, 'PRE,AKE,FF')
      if (ix > 0) then
        flagfy = .false.
        flagtke = .true.
        MT = 461
      endif
      ix = index(reaction_xc5, 'SEC,AKE,FF')
      if (ix > 0) then
        flagfy = .false.
        flagtke = .true.
        MT = 462
      endif
      ix = index(reaction_xc5, 'REL')
      if (ix > 0) then
        MT = 464
      endif
      flagconv = .true.
      MT_xc5 = MT
      exit
    endif
    if (MF == 1 .and. (MT == 452 .or. MT == 455 .or. MT == 456)) then
      flagnubar = .true.
      flagconv = .true.
      exit
    endif
    if (MT > 120 .and. MT < 9000 .and. .not. MT == 1003 .and. .not. (MT >= 600 .and. MT <= 848)) exit
!
! Cross sections
!
! inelastic  : subroutine to process inelastic scattering
!
    if (MF == 3) then
      ix = index(reaction_xc5, 'PAR,FY')
      if (ix > 0) exit
      if (MT <= 120 .or. (MT >= 600 .and. MT <= 848)) then
        flagMT = .true.
        flagconv = .true.
      endif
      if ((MT >= 51 .and. MT <=90) .or. (MT >= 600 .and. MT <= 848)) then
        if (ident == 'LVL' .or. ident == 'EXC' .or. ident(2:3) == 'E2') then
          flagdisc = .true.
          call inelastic(MT)
          MT_xc5 = MT
          Np = Npoints_xc5
          if (Np > 1) then
            read(string_xc5(1), '(76x, e9.2)') ex1
            do i = 2, Np
              read(string_xc5(i), '(76x, e9.2)') ex2
              if (ex1 /= ex2) then
                Npoints_xc5 = i - 1
                Npoints_new = i - 1
                exit
              endif
            enddo
          endif
          flagconv = .true.
        else
          exit
        endif
      endif
      if (MT == 4) then
        if (parsig > 0) flagconv = .false.
      endif
      if (MT == 50) then
        flagdisc = .true.
        call inelastic(MT)
        flagconv = .true.
      endif
      if (MT == 1003) then
        flagres = .true.
        flagconv = .true.
      endif
      if (MT >= 9000) then
        MT = 5
        ZA = 0.
        ZAres = 0
        if (valstring(6) /= '         ') then
          if (valstring(5) /= '         ') then
            read(valstring(5), * , iostat = istat) Zrr
            if (istat /= 0) call read_error(valstring(5), istat, eor = 'continue', eof = 'continue')
            read(valstring(6), * , iostat = istat) Arr
            if (istat /= 0) call read_error(valstring(6), istat, eor = 'continue', eof = 'continue')
            ZAres = int(1000.*Zrr+Arr)
            Zres = int(Zrr)
            Ares = int(Arr)
          else
            read(valstring(6), * , iostat = istat) ZA
            if (istat /= 0) call read_error(valstring(6), istat, eor = 'continue', eof = 'continue')
            ZAres = int(ZA)
          endif
        endif
        k1 = - 1
        if (ZAres == 0) k1 = 0
        if (ZAres == 1) k1 = 1
        if (ZAres == 1001) k1 = 2
        if (ZAres == 1002) k1 = 3
        if (ZAres == 1003) k1 = 4
        if (ZAres == 2003) k1 = 5
        if (ZAres == 2004) k1 = 6
        if (k1 >= 0 .and. k1 <= 6) then
          flagMT = .true.
          MT = 201 + k1
          if (k1 == 0) MT = 202
          if (k1 == 1) MT = 201
          MT_xc5 = MT
        else
          flagres = .true.
        endif
        kres = k1
        flagconv = .true.
        nx = index(reaction_xc5, 'N, X')
        if (parsig > 0 .and. nx > 0) flagconv = .false.
        nx = index(reaction_xc5, 'SCT')
        if (nx > 0) flagconv = .false.
        nx = index(reaction_xc5, 'PAR,SIG')
        if (nx > 0 .and. MT >=201 .and. MT <= 207) flagconv = .false.
      endif
      if (MT == 18) then
        ZA = 0.
        ZAres = 0
        if (valstring(6) /= '         ') then
          if (valstring(5) /= '         ') then
            read(valstring(5), * , iostat = istat) Zrr
            if (istat /= 0) call read_error(valstring(5), istat, eor = 'continue', eof = 'continue')
            read(valstring(6), * , iostat = istat) Arr
            if (istat /= 0) call read_error(valstring(6), istat, eor = 'continue', eof = 'continue')
            ZAres = int(1000.*Zrr+Arr)
            Zres = int(Zrr)
            Ares = int(Arr)
          else
            read(valstring(6), * , iostat = istat) ZA
            if (istat /= 0) call read_error(valstring(6), istat, eor = 'continue', eof = 'continue')
            ZAres = int(ZA)
          endif
          flagMT = .false.
          flagres = .true.
          flagconv = .true.
          MF = 6
          MT = 5
          MT_xc5 = MT
          MF_xc5 = MF
        endif
      endif
      ix = index(reaction_xc5, 'SIG,,')
      if (ix > 0) then
        ix = index(reaction_xc5, 'SPA')
        if (ix > 0) then
          flagspecav = .true.
          flagMT = .false.
          flagres = .false.
          flagconv = .true.
          exit
        endif
        ix = index(reaction_xc5, 'FST')
        if (ix > 0) then
          flagspecav = .true.
          flagMT = .false.
          flagres = .false.
          flagconv = .true.
          exit
        endif
        ix = index(reaction_xc5, 'MXW')
        if (ix > 0) then
          flagspecav = .true.
          flagMT = .false.
                flagres = .false.
          flagconv = .true.
          exit
        endif
        ix = index(reaction_xc5, 'FIS')
            if (ix > 0) then
          flagspecav = .true.
          flagMT = .false.
          flagres = .false.
          flagconv = .true.
          exit
        endif
        exit
      endif
      ix = index(reaction_xc5, 'PAR,SIG,G')
      if (ix > 0) then
        flagMT = .false.
        flagres = .false.
        flagconv = .false.
        exit
      endif
      goto 100
    endif
!
! Angular distributions
!
    if (MF >= 4 .and. MF <= 6 .and. MT >= 9000) then
      ix = index(reaction_xc5, 'DA,,')
      if (ix > 0) return
      ZA = 0.
      if (valstring(6) /= '         ') then
        read(valstring(6), * , iostat = istat) ZA
        if (istat /= 0) call read_error(valstring(6), istat, eor = 'continue', eof = 'continue')
      endif
      ZAres = int(ZA)
      k1 = -1
      if (ZAres == 0) k1 = 0
      if (ZAres == 1) k1 = 1
      if (ZAres == 1001) k1 = 2
      if (ZAres == 1002) k1 = 3
      if (ZAres == 1003) k1 = 4
      if (ZAres == 2003) k1 = 5
      if (ZAres == 2004) k1 = 6
      if (k1 >= 0 .and. k1 <= 6) then
        MT = 201 + k1
        if (k1 == 0) MT = 202
        if (k1 == 1) MT = 201
        MT_xc5 = MT
      endif
      kres = k1
    endif
    if (MF == 4) then
      ix = index(reaction_xc5, 'DA,,')
      if (ix > 0) return
      if (MT == 2) then
        flagelang = .true.
        flagconv = .true.
        exit
      endif
      if ((MT >= 51 .and. MT <=90) .or. (MT >= 600 .and. MT <= 848) &
 &      .and. (ident == 'LVL' .or. ident == 'EXC' .or. ident(2:3) == 'E2')) then
        flagdiscang = .true.
        call inelastic(MT)
        MT_xc5 = MT
        flagconv = .true.
        exit
      endif
      if (MT == 4 .or. MT == 50 .or. (MT >= 103 .and. MT <= 107)) then
        flagdiscang = .true.
        call inelastic(MT)
        flagconv = .true.
        exit
      endif
      if (MT == 9000) then
        flagddx = .true.
        flagconv = .true.
        goto 100
      endif
      exit
    endif
!
! (Double-differential) spectra
!
    if (MF == 5 .or. MF == 6) then
      if (MT >= 9000 .or. (MT >= 102 .and. MT <= 107) .or. MT == 4 .or. (MT >= 201 .and. MT <= 207)) then
        if (MF == 5) then
          flagspec = .true.
          flagconv = .true.
        endif
        if (MF == 6) then
          flagddx = .true.
          flagconv = .true.
        endif
      else
        exit
      endif
      goto 100
    endif
!
! Extra test for conversion
!
100 if (flagres .or. flagspec .or. flagddx .or. flagratio) then
      if (ZAres == 0) then
        ZA = 0.
        if (valstring(6) /= '         ') then
          read(valstring(6), * , iostat = istat) ZA
          if (istat /= 0) call read_error(valstring(6), istat, eor = 'continue', eof = 'continue')
        endif
        ZAres = int(ZA)
      endif
      if (MT == 1003) then
        Zr = Z - 1
        Ar = A - 1
        ZAres = 1000 * Zr + Ar
!
! Put this in (n,np) though not exactly true
!
        if (k0 == 1) then
          flagres = .false.
          flagMT = .true.
          flagconv = .true.
          MT_xc5 = 28
          exit
        endif
      endif
      if (MT == 4 .or. MT == 201) ZAres = 1
      if (MT == 102 .or. MT == 202) ZAres = 0
      if (MT == 103 .or. MT == 203) ZAres = 1001
      if (MT == 104 .or. MT == 204) ZAres = 1002
      if (MT == 105 .or. MT == 205) ZAres = 1003
      if (MT == 106 .or. MT == 206) ZAres = 2003
      if (MT == 107 .or. MT == 207) ZAres = 2004
      if (ZAres == 0 .and. ZA /= 0.9) then
        flagres = .false.
        flagratio = .false.
        flagconv = .false.
      endif
      Zres = ZAres / 1000
      Ares = ZAres - Zres * 1000
      if (flagres .or. flagratio) then
        ris = ZA - ZAres
        if (ris == 0.) Liso = 0
        if (ris == 0.1) Liso = 1
        if (ris == 0.2) Liso = 2
        if (ris > 0.2 .and. ris <= 0.8) Liso = 1
      endif
      if (flagspec .or. flagddx) then
        k1 = kres
        if (k1 ==  -1) flagconv = .false.
      endif
      if (flagres) then
        MT_xc5 = MTrp
        MT = MTrp
      endif
      if (k0 == 1 .and. Zres == Z - 1 .and. Ares == A - 1) then
        flagres = .false.
        flagMT = .true.
        flagconv = .true.
        MT_xc5 = 28
        exit
      endif
    endif
    exit
  enddo
!
! Option to only cover cross sections
!
  if (flagxsonly .and. .not. (flagMT .or. flagres)) flagconv = .false.
  if (flagratio) then
    flagMT = .false.
    flagres = .false.
  endif
  if (MT <= 0) then
    flagconv = .false.
    return
  endif
  is = isom_xc5
  if (MT <= nummt .and. (flagMT .or. flagres)) NsetsallMT(k0, MT, is) = NsetsallMT(k0, MT, is) + 1
  MT = MT_xc5
  if (MT <= nummt) then
    if ( .not. mtinclude(MT)) flagconv = .false.
  endif
  ix = index(reaction_xc5, 'EVAL')
  if (ix > 0) flagconv = .false.
  ix = index(reaction_xc5, '-L,')
  if (ix > 0) flagconv = .false.
  return
end subroutine reactionsub
! Copyright A.J. Koning 2019
