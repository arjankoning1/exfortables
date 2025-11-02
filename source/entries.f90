subroutine entries
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
!              compdir, &      ! directory with statistics
!              numsub, &       ! maximum number of subentries
!              parinclude, &    ! flag to include particle
!              partype, &    ! symbol of particle
!              partype          ! symbol of particle
!
! *** Declaration of local data
!
  implicit none
  logical            :: done(numsub)   !
  character(len=300) :: string(0:numsub)   !
  character(len=4)   :: ystr                    !
  character(len=5)   :: entry                    !
  character(len=13)  :: subentry                  !
  character(len=16)  :: auth                  !
  character(len=35)  :: entryfile               !
  character(len=35)  :: FallEfile               !
  character(len=80)  :: allfile               !
  integer            :: i                       ! counter
  integer            :: istat
  integer            :: L
  integer            :: isub                    ! counter
  integer            :: N                      !
  integer            :: j                       ! counter
  integer            :: k0                      !
  real(sgl)          :: Fent                  !
  real(sgl)          :: Fsum                  !
  real(sgl)          :: Fsub                  !
!
! Entries
!
  allfile = trim(compdir)//'total/allentries'
  open (unit = 8, status = 'unknown', file = allfile)
  write(8, '("#ENTRY    AUTHOR              YEAR    F")')
  do k0 = 0, 6
    if ( .not. parinclude(k0)) cycle
    FallEfile = trim(compdir)//'total/'//partype(k0)//'-allreac'
    open (unit = 2, status = 'unknown', file = FallEfile)
    read(2,'(a)', iostat = istat) string(0)
    if (istat == -1) cycle
    i=0
    do
      i= i + 1
      read(2,'(a)', iostat = istat) string(i)
      if (istat == -1) exit
    enddo
    close(2)
    N = i - 1
    done = .false.
    do i = 1, N
      Fsum = 0.
      if (.not. done(i)) then
        done (i) = .true.
        read(string(i)(17:25),'(a9)') subentry
        read(string(i)(83:92),*) Fsub
        entry = subentry(1:5)
        auth = string(i)(26:41)
        ystr = string(i)(42:45)
        Fsum = Fsub
        if (Fsub > 0.) then
          isub = 1
        else
          isub = 0
        endif
        entryfile = trim(auth)//'-'//trim(entry)//'.' //ystr
        open (unit = 7, status = 'unknown', file = trim(compdir)//'entry/'//entryfile)
        write(7, '(a)') trim(string(0))
        write(7, '(a)') trim(string(i))
        do j = i + 1, N
          read(string(j)(17:25),'(a9)') subentry
          if (subentry(1:5) == entry) then
            done (j) = .true.
            read(string(j)(83:92),*) Fsub
            if (Fsub > 0.) then
              isub = isub + 1
              Fsum = Fsum + Fsub
            endif
            write(7, '(a)') trim(string(j))
          endif
        enddo
        if (isub > 0) then
          Fent = Fsum / isub
        else
          Fent = 0.
        endif
        write(7, '(" Average F:",1p,g10.3)') Fent
        close (unit=7)
        write(8, '(a5,5x,a16,4x,a4,1p,g10.3)') entry,auth,ystr,Fent
      endif
    enddo
  enddo
  close (unit=8)
  L = len_trim(allfile)
  call sort(allfile, L, 'R', 36, 45, 1, 0)
  return
end subroutine entries
! Copyright A.J. Koning 2019
