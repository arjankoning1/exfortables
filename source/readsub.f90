subroutine readsub
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Read XC5 subentry
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
!              A_xc5, &               ! mass number
!              author1_xc5, &         ! author
!              authors_xc5, &         ! authors of paper
!              date_x4_nrdc, &        ! date according to NDRC file
!              date_xc5, &            ! date
!              date_xc5_nrdc, &       ! date according to NDRC file
!              entry_sub, &           ! entry of subentry
!              entry_xc5, &           ! entry
!              exist_xc5, &           ! flag for existence of subentry
!              flagout, &             ! flag for main output
!              flagxc5, &             ! flag to use XC5 instead of XC4
!              iso_xc5, &             ! isomer of target
!              isochar, &             ! symbol of isomer
!              isochar_xc5, &         ! isomer of target
!              MF_xc5, &              ! MF number
!              MT_xc5, &              ! MT number
!              nauthors, &            ! number of lines with authors
!              Nentry_xc5, &          ! number of entries according to NRDC file
!              Nentry_xc5_nrdc, &     ! number of entries according to NRDC file
!              Nmod, &                ! number for output of progress of processing
!              Npoints_xc5, &         ! number of points per subentry
!              Npointstot_xc5, &      ! total number of points
!              Npointstot_xc5_nrdc, & ! number of total points according to NDRC file
!              nref, &                ! number of lines with reference
!              Nsub_new, &            ! counter for new subentries
!              Nsub_x4_nrdc, &        ! number of subentries according to NRDC file
!              Nsub_xc5, &            ! counter for XC5 subentries
!              Nsub_xc5_nrdc, &       ! number of subentries according to NRDC file
!              nuclide_xc5, &         ! nuclide
!              ntitle, &              ! number of lines with title
!              nuc, &                 ! nuclide symbol
!              numA, &                ! maximum number of masses
!              numA, &                ! maximum number of masses
!              nument, &              ! maximum number of entries
!              numpoint, &            ! maximum number of data points in subentry
!              numref, &              ! maximum number of reference lines
!              numsub, &              ! maximum number of subentries
!              numZ, &                ! maximum number of elements
!              reaction_xc5, &        ! XC5 reaction
!              reference_xc5, &       ! reference of paper
!              string_xc5, &          ! date according to NDRC file
!              subentry_xc5, &        ! XC5 subentry number
!              time_xc5_nrdc, &       ! time according to NDRC file
!              title_xc5, &           ! title of paper
!              year_xc5, &            ! year
!              Z_xc5                  ! charge number
! use A1_error_handling_mod, only: & ! Error handling
!          read_error ! Message for file reading error
!
! *** Declaration of local data
!
  implicit none
  character(len=1)   :: ch         ! character for isomer
  character(len=4)   :: Astr       ! mass string
  character(len=300) :: line,line2 ! input line
  integer            :: k          ! counter
  integer            :: Np
  integer            :: ix         ! index
  integer            :: ia         ! mass number
  integer            :: iauthors   ! number of author lines
  integer            :: iref       ! number of reference lines
  integer            :: istat      ! error code
  integer            :: ititle     ! number of title lines
  integer            :: iz         ! charge number
  integer            :: Nm         ! help variable
  integer            :: pos1       ! position on line
  integer            :: targ       ! target
!
! **************** Read XC5 subentry information ***********************
!
  call entryinitial
  if (flagxc5) then
    pos1 = 17
  else
    pos1 = 13
  endif
  do
    read(11, '(a)', iostat = istat) line
    if (istat == -1) exit
    if (istat > 0) call read_error(trim(cfile), istat)
    entry_xc5=''
!
! A. ENTRY loop
!
! Also some elementary checks are performed
!
!   if (line(1:12) == '#ENTRY      ') then
!     read(line(pos1:pos1+8), '(a9)') entry_xc5
!     if (entry_xc5 /= entry_sub(Nentry_xc5)) then
!       Nentry_xc5 = Nentry_xc5 + 1
!       if (Nentry_xc5 > nument) then
!         write(*, '(" EXFORTABLES-error: Nentry_xc5 = ", i7, " > nument = ", i6)') Nentry_xc5, nument
!         stop
!       endif
!       if (flagout) write(* , * ) "Entry: ", entry_xc5
!       entry_sub(Nentry_xc5) = entry_xc5
!     endif
!     cycle
!   endif
    if (line(1:12) == '#AUTHOR1    ') then
      read(line(pos1:pos1+24), '(a25)') author1_xc5
      if (flagout) write( * , * ) "Author: ", author1_xc5
      cycle
    endif
    if (line(1:12) == '#YEAR       ') then
      read(line(pos1:pos1+3), '(i4)') year_xc5
      cycle
    endif
    if (line(1:12) == '#TITLE      ') then
      read(line(pos1:pos1+71), '(a72)') title_xc5(1)
      ititle = 1
      do
        read(11, '(a)') line
        if (line(1:2) == '#+') then
          ititle = ititle + 1
          if (ititle > numref) then
            write(*, '(" EXFORTABLES-error: ititle=", i3, " > numref = ", i3, " for Entry ", a9)') ititle, numref, entry_xc5
            stop
          endif
          title_xc5(ititle) = line(pos1:pos1+71)
        else
          exit
        endif
      enddo
      ntitle = ititle
      backspace 11
      cycle
    endif
    if (line(1:12) == '#AUTHOR(S)  ' .or. line(1:12) == '#AUTHORS    ') then
      read(line(pos1:pos1+71), '(a72)') authors_xc5(1)
      iauthors = 1
      do
        read(11, '(a)') line
        if (line(1:2) == '#+') then
          iauthors = iauthors + 1
          if (iauthors > numref) then
            write(*, '(" EXFORTABLES-error: iauthors =",i3," > numref=", i3," for Entry ", a9)') iauthors, numref, entry_xc5
            stop
          endif
          authors_xc5(iauthors) = line(pos1:pos1+71)
        else
          exit
        endif
      enddo
      nauthors = iauthors
      backspace 11
      cycle
    endif
    if (line(1:10) == '#REFERENCE') then
      read(line(pos1:pos1+71), '(a72)') reference_xc5(1)
      iref = 1
      do
        read(11, '(a)') line
        if (line(1:2) == '#+') then
          iref = iref + 1
          if (iref > numref) then
            write(*, '(" EXFORTABLES-error: iref = ", i3, " > numref = ", i3, " for Entry ", a9)') iref, numref, entry_xc5
            stop
          endif
          reference_xc5(iref) = line(pos1:pos1+71)
        else
          exit
        endif
      enddo
      nref = iref
      backspace 11
      cycle
    endif
    Ndatasets = 1
    if (line(1:12) == '#DATASETS   ') then
      read(line(pos1:pos1+3), '(i4)') Ndatasets
      cycle
    endif
!
! B. SUBENTRY loop
!
    if (line(1:12) == '#DATASET    ') then
      call subentryinitial
      Nsub_xc5 = Nsub_xc5 + 1
      Nsub_new = Nsub_new + 1
      if (Nsub_xc5 > numsub) then
        write(*, '(" EXFORTABLES-error: Nsub_xc5 = ", i6, " > numsub = ", i6)') Nsub_xc5, numsub
        stop
      endif
      Nm = min(10 **(int(log10(real(Nsub_xc5)))), Nmod)
      if (mod(Nsub_xc5, Nm) == 0) write(*, '("XC5 subentries processed:", i8)') Nsub_xc5
      read(line(pos1:pos1+8), '(a9)') subentry_xc5
      if (flagout) write( * , * ) "Subentry: ", subentry_xc5
      cycle
    endif
    if (line(1:12) == '#DATE       ') then
      read(line(pos1:pos1+7), '(i8)') date_xc5
      cycle
    endif
!
! Reaction identifiers
!
    if (line(1:12) == '#REACTION   ') then
      read(line(pos1:pos1+59), '(a60)') reaction_xc5
      cycle
    endif
    if (line(1:12) == '#TARG       ') then
      read(line(pos1:pos1+5), '(i6)') targ
      ch = line(pos1+6:pos1+6)
      ix = index(line(pos1+6:pos1+26), 'M')
      if (ix > 0) ch = 'M'
      ix = index(line(pos1+6:pos1+26), 'N')
      if (ix > 0) ch = 'N'
      iz = targ / 1000
      ia = mod(targ, 1000)
      if (iz < 0 .or. iz > numZ) then
        write(*, '(" EXFORTABLES-error: 0 < = iz = ", i6, " < = numZ = ", i6, " for Subentry: ", a9)') iz, numZ, subentry_xc5
        stop
      endif
      if (ia < 0 .or. ia > numA) then
        write(*, '(" EXFORTABLES-error: 0 < = ia = ", i6, " < = numA = ", i6, " for Subentry: ", a9)') ia, numA, subentry_xc5
        stop
      endif
      Z_xc5 = iz
      A_xc5 = ia
      Astr = '    '
      write(Astr(1:3), '(i3.3)' ) ia
      iso_xc5 = 0
      isochar_xc5 = ' '
      if (ch == 'M') then
        iso_xc5 = 1
        isochar_xc5 = isochar(iso_xc5)
        Astr(4:4) = isochar_xc5
      endif
      if (ch == 'N') then
        iso_xc5 = 2
        isochar_xc5 = isochar(iso_xc5)
        Astr(4:4) = isochar_xc5
      endif
      nuclide_xc5 = trim(nuc(iz))//Astr
      cycle
    endif
    if (line(1:12) == '#MF         ') then
      read(line(pos1:pos1+2), '(i3)') MF_xc5
      cycle
    endif
    if (line(1:12) == '#MT         ') then
      read(line(pos1:pos1+3), '(i4)') MT_xc5
      cycle
    endif
!
! C. Read dataset
!
    if (line(1:12) == '#DATA       ' .or. line(1:12) == '#C4DATA     ' .or. line(1:12) == '#C5DATA     ') then
      read(line(pos1:pos1+4), '(i5)') Np
      if (Np > numpoint) then
        write(*, '(" EXFORTABLES-error: Npoints =", i8, " > numpoint for Subentry ", a9)') Np, subentry_xc5
        stop
      endif
      flagZAProd = .false.
      k = 0
      do
        read(11, '(a)', iostat = istat) line2
        if (istat == -1) exit
        if (line2(1:8) == '#Headers') then
          ix = index(line2, 'ZAProd')
          if (ix > 0) flagZAProd = .true.
        endif
        if (line2(1:1) /= '#') then
          k = k + 1
          string_xc5(k) = trim(line2)
        endif
      enddo
      Npoints_xc5 = k
      Npointstot_xc5 = Npointstot_xc5 + Npoints_xc5
      exit
    endif
  enddo
  Eindex = 1
  Eindexnew = 1
  flagsubdone = .true.
  Npoints_new = Npoints_xc5
  Nend_new = Npoints_xc5
  return
end subroutine readsub
! Copyright A.J. Koning 2019
