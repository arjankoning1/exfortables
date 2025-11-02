subroutine filesort
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Sort files
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
!              Amin, &    ! minimal A value to process
!              Amax, &    ! maximal A value to process
!              isochar, & ! symbol of isomer
!              nuc, &     ! symbol of nucleus
!              numA, &    ! maximum number of masses
!              numisom, & ! maximum number of isomers
!              numpar, &  ! number of particles
!              nummt, &   ! maximum number of MT numbers
!              numZ, &    ! maximum number of charge
!              partype, & ! symbol of particle
!              ZAexist    ! logical for Z, A existence
!
! *** Declaration of local data
!
  implicit none
  logical           :: lexist  ! logical for file existence
  character(len=1)  :: proj0    ! projectile
  character(len=2)  :: projpath ! projectile path
  character(len=3)  :: Astr     ! mass string
  character(len=3)  :: ext
  character(len=4)  :: MTstr    ! MT string
  character(len=7)  :: ZAstr    ! ZA string
  character(len=6)  :: target   ! nuclides
  character(len=8)  :: proj0nuc ! projectile-target
  character(len=80) :: listfile !
  character(len=80) :: datatype ! data type
  character(len=80) :: spec(20) ! special file
  character(len=80) :: special  ! special file
  character(len=80) :: projlist ! file name
  character(len=80) :: MTlist   ! file name
  character(len=80) :: reslist  ! file name
  character(len=80) :: setlist  ! file name
  character(len=80) :: nucpath  ! nuclide path
  character(len=80) :: respath  ! residual product path
  character(len=80) :: MTpath   ! MT path
  integer           :: k0       ! incident particle type
  integer           :: MT       ! MT number
  integer           :: A        ! mass number
  integer           :: L        ! length of string
  integer           :: Z        ! charge number
  integer           :: ZArp     !
  integer           :: N
  integer           :: k
  integer           :: i        ! counter
  integer           :: iso      ! counter for isomer
  integer           :: istat    !
  integer           :: isor     ! counter for isomer
!
! ***************************** Sort dictionary *****************************
!
  Astr = '   '
  MTstr = '    '
  open(unit = 31, status = 'unknown', file = 'stat/comp/lists/ZAMT.tab')
  do k0 = 0, numpar
    proj0 = partype(k0)
    projpath = proj0 // '/'
    projlist = trim(projpath) // proj0 // '.list'
    inquire (file = projlist, exist = lexist)
    L = len_trim(projlist)
    if (lexist) call sort(projlist, L, 'C', 1, 12, 1, 0)
    do Z = 1, numZ
      do A = 0, numA
        if (A > 0 .and. A < Z ) cycle
        if (A < Amin .or. A > Amax) cycle
        do iso = 0, numisom
          if (ZAexist(k0, Z, A, iso)) then
            write(Astr(1:3), '(i3.3)') A
            target = trim(nuc(Z)) // Astr
            if (iso == 1) target = trim(target) // 'm'
            if (iso == 2) target = trim(target) // 'n'
            nucpath = projpath // trim(target) // '/'
            proj0nuc=proj0 // '-' // trim(target)
!
! MT numbers
!
            do i = 1, 8
              if (i == 1) datatype = 'xs'
              if (i == 2) datatype = 'angle'
              if (i == 3) datatype = 'ddx'
              if (i == 4) datatype = 'fission'
              if (i == 5) datatype = 'ratio'
              if (i == 6) datatype = 'resint'
              if (i == 7) datatype = 'resonance'
              if (i == 8) datatype = 'spectrum'
              MTlist = trim(nucpath) // trim(datatype) // '/' // trim(proj0nuc) // '.list'
              inquire (file = MTlist, exist = lexist)
              L = len_trim(MTlist)
              if (lexist) call sort(MTlist, L, 'C', 1, 12, 1, 0)
              do MT = 1, nummt
                do isor = -1, numisom
                  MTstr = '    '
                  write(MTstr(1:3), '(i3.3)') MT
                  if (isor >= 0) MTstr(4:4) = isochar(isor)
                  MTpath = trim(nucpath) // trim(datatype) // '/' // trim(MTstr) // '/'
                  setlist = trim(MTpath) // trim(proj0nuc) // '-MT' // trim(MTstr) // '.list'
                  L = len_trim(setlist)
                  inquire (file = setlist, exist = lexist)
                  if (lexist) then
                    call sort(setlist, L, 'C', 1, 80, 1, 0)
                    open(unit = 32, status = 'unknown', file = setlist)
                    k = 0
                    do
                      read(32,'()',iostat=istat)
                      if (istat == -1) exit
                      k = k + 1
                    enddo
                    close(32)
                    N = k - 1
                    if (i == 1) then
                      write(31,'(7i4)') k0,Z,A,iso,MT,isor,N
                      listfile = 'stat/comp/lists/' // proj0 // '-MT' // trim(MTstr) // '.list'
                      open (unit = 33, status = 'unknown', file = listfile, position = 'append')
                      write(33,'(a)') trim(proj0nuc)
                      close(33)
                    endif
                  endif
                enddo
              enddo
            enddo
!
! Residual production cross sections
!
            reslist = trim(nucpath) // 'residual/' // trim(proj0nuc) // '.list'
            inquire (file = reslist, exist = lexist)
            L = len_trim(reslist)
            if (lexist) then
              call sort(reslist, L, 'C', 1, 12, 1, 0)
              open(unit = 21, status = 'unknown', file = reslist)
              read(21, '()', iostat = istat)
              if (istat == -1) exit
              do
                read(21, '(2i6)', iostat = istat) ZArp, isor
                if (istat == -1) exit
                ZAstr = '       '
                write(ZAstr(1:6), '(i6.6)') ZArp
                write(ZAstr(7:7), '(a1)') isochar(isor)
                respath = trim(nucpath) // 'residual/' // trim(ZAstr) // '/'
                setlist = trim(respath) // trim(proj0nuc) // '-' // trim(ZAstr) // '.list'
                L = len_trim(setlist)
                inquire (file = setlist, exist = lexist)
                if (lexist) call sort(setlist, L, 'C', 1, 80, 1, 0)
              enddo
              close (21)
            endif
          endif
        enddo
      enddo
    enddo
  enddo
  close (21)
  close (31)
!
! Special files
!
  spec = ''
  do k = 1, 2
    if (k == 1) then
      ext = ''
    else
      ext = '_av'
    endif
    spec(1) = 'exfor_thermal'//trim(ext)//'_tot.txt'
    spec(2) = 'exfor_thermal'//trim(ext)//'_el.txt'
    spec(3) = 'exfor_thermal'//trim(ext)//'_nf.txt'
    spec(4) = 'exfor_thermal'//trim(ext)//'_ng.txt'
    spec(5) = 'exfor_thermal'//trim(ext)//'_np.txt'
    spec(6) = 'exfor_thermal'//trim(ext)//'_na.txt'
    spec(7) = 'exfor_thermal'//trim(ext)//'_nu.txt'
    spec(8) = 'exfor_thermal'//trim(ext)//'_nud.txt'
    spec(9) = 'exfor_thermal'//trim(ext)//'_nup.txt'
    spec(10) = 'exfor_30keV'//trim(ext)//'.txt'
    spec(11) = 'exfor_Ig.txt'
    spec(12) = 'exfor_If.txt'
    spec(13) = 'exfor_gamgam.txt'
    spec(14) = 'exfor_D0.txt'
    do i = 1, 14
      if (k == 2 .and. i > 10) cycle
      special = 'special/' // trim(spec(i))
      inquire (file = special, exist = lexist)
      L = len_trim(special)
      if (lexist) then
        call sort(special, L, 'I', 1, 4,  0, 0)
        call sort(special, L, 'I', 5, 8,  0, 0)
      endif
    enddo
   enddo
  return
end subroutine filesort
! Copyright A.J. Koning 2019
