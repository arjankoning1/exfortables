subroutine quality(iset)
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Quality information
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
!              Asset, &         ! average As value per subentry
!              chi2set, &       ! average Chi - 2 per subentry
!              day, &           ! day
!              entry_sub, &     ! entry of subentry
!              Fset, &          ! average F value per subentry
!              Fsigma, &        ! deviation for F factor
!              isom_xc5, &      ! number of isomer
!              k0_xc5, &        ! incident particle
!              library, &       ! nuclear data library
!              month, &         ! month
!              MT_xc5, &        ! MT number
!              Nentry_xc5, &    ! number of entries according to NRDC file
!              Nlibs, &         ! number of data libraries for comparison
!              Npointsset, &    ! number of points per subentry
!              NQ, &            ! number of items with quality information
!              Qaction, &       ! recommended action
!              Qaction0, &      ! recommended action
!              Qcomment, &      ! comments
!              Qdate, &         ! quality date
!              Qdate0, &        ! quality date
!              Qentry, &        ! entry with quality information
!              QMT, &           ! quality per MT number
!              QMTall, &        ! total quality per MT number
!              Qscore, &        ! quality score
!              Qscore0, &       ! quality score
!              Qsub0, &         ! subentry with quality information
!              qualityref, &    ! reference for quality assignment
!              Rset, &          ! average R per subentry
!              subentry_E, &  ! XC5 subentry number
!              year             ! year
!
! *** Declaration of local data
!
  implicit none
  character(len=1)   :: class(4)   !
  character(len=2)   :: score      !
  character(len=9)   :: subentry   !
  integer            :: i          ! counter
  integer            :: iset       ! counter
  integer            :: ie         !
  integer            :: is         !
  integer            :: iscore     !
  integer            :: j          ! counter
  integer            :: k          ! counter
  integer            :: k0         !
  integer            :: lib        !
  integer            :: MT         !
  real(sgl)          :: chi2       !
  real(sgl)          :: F          !
  real(sgl)          :: R          ! fraction of non-elastic cross section
!
! Put reference on one long line for quality file
!
  class(1) = 'T'
  class(2) = 'N'
  class(3) = 'R'
  class(4) = 'E'
  MT = MT_xc5
  is = isom_xc5
  k0 = k0_xc5
!
! ************************* Quality flags ******************************
!
  subentry = subentry_E(iset)
  do i = 1, NQ
    if (subentry(1:9) == Qsub0(i)(1:9)) then
      Qscore = Qscore0(i)
      if (Qscore(1:1) /= 'T') Qdate = Qdate0(i)
      Qaction = Qaction0(i)
      exit
    endif
  enddo
  if (Qdate(1:1) == ' ') then
    Qdate(3:3) = '-'
    Qdate(6:6) = '-'
    write(Qdate(1:2), '(i2.2)') day
    write(Qdate(4:5), '(i2.2)') month
    write(Qdate(7:10), '(i4.4)') year
  endif
  weight = -1
  do i = 1, NW
    if (subentry(1:9) == subweight(i)(1:9)) then
      weight = weight0(i)
      exit
    endif
  enddo
!
! Quality scoring
!
  k = qualityref
  if (Fset(k) > 0.) then
    if (Qscore(1:1) == ' ') Qscore(1:1) = 'T'
    if (Qscore(1:1) /= 'R' .and. Qscore(1:1) /= 'E') Qscore(1:1) = 'T'
    F = Fset(k)
    R = Rset(k)
    chi2 = chi2set(k)
    if (F <= Fsigma(MT, is, 1)) then
      iscore = 1
    else
      if (F <= Fsigma(MT, is, 2)) then
        if (R > 0.10 .and. chi2 > 30.) then
          iscore = 3
        else
          iscore = 2
        endif
      else
        if (R > 0.05 .and. chi2 > 30.) then
          iscore = 3
        else
          iscore = 2
        endif
      endif
    endif
    write(Qscore(2:2), '(i1)') iscore
    do lib = 0, Nlibs
      Qcomment(lib)(1:10) = library(lib)
      write(Qcomment(lib)(19:78), '("N=", i6, "    F=", 1p, g11.3, " A=", g11.3, " chi-2=", g11.3)') &
 &      Npointsset(lib), Fset(lib), Asset(lib), chi2set(lib)
    enddo
  else
    if (Qscore(1:1) == 'T' .or. Qscore(1:1) == 'N') Qscore(1:2) = '  '
  endif
!
! Summed quality scores
!
  do i = 1, 4
    score = class(i)//' '
    do j = 1, 3
      write(score(2:2), '(i1)') j
      if (Qscore == score) then
        QMT(k0, MT, is, i, j) = QMT(k0, MT, is, i, j) + 1
        QMTall(k0, i, j) = QMTall(k0, i, j) + 1
        do ie = 1, Nentry_xc5
          if (entry_sub(ie)(1:5) == subentry_E(iset)(1:5)) then
            Qentry(ie, i, j) = Qentry(ie, i, j) + 1
            return
          endif
        enddo
      endif
    enddo
  enddo
  return
end subroutine quality
! Copyright A.J. Koning 2019
