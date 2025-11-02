subroutine processxc5
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Process XC5 data into new database
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
!              filespath, &   ! directory containing X4 and structure files to be read
!              flagconv, &    ! flag for conversion to new database
!              flagMTequiv, & ! flag for equivalence of MT number and residual product
!              flagxs, &      ! flag for cross section
!              Nsub_new, &    ! counter for new subentries
!
! *** Declaration of local data
!
  implicit none
  integer            :: A          ! mass number
  integer            :: i          ! counter
  integer            :: iso        ! isomer number
  integer            :: istat      ! error code
  integer            :: k0         ! counter for incident particle
  integer            :: Nlist      ! counter
  integer            :: Z          ! charge number
  logical            :: nucexist   ! flag
  character(len=13)  :: isostring(0:numisom) ! isomer string
  character(len=1)   :: ptype      ! particle type
  character(len=3)   :: Astring    ! string
  character(len=6)   :: nuclide    ! nuclide
  character(len=6)   :: nuc_list(numsub)! nuclide list
  character(len=132) :: list       ! filename
!
! **************** Read data and process it into new database **********
!
! openfiles  : subroutine to open files
! closefiles : subroutine to close files
! generalinfo: subroutine to read general C4 and C5 info
! process_xs : subroutine to process cross sections
!
  call openfiles
  call generalinfo
!
! Process entire database
!
  write( * , * ) "Processing XC5 database....."
!
! Loop over particles and read nuclide list
!
  isostring(0) = ' '
  isostring(1) = 'm'
  isostring(2) = 'n'
Loop1: do k0 = 0, numpar
    if (.not. parinclude(k0)) cycle
    ptype = partype(k0)
    list = trim(filespath)//'C5reac/'//ptype//'/'//ptype//'.list'
    open (unit = 2, status = 'old', file = list)
    i = 0
    do
      i = i + 1
      read(2,'(a6)', iostat=istat) nuc_list(i)
      if (istat == -1) exit
    enddo
    close(unit = 2)
    Nlist = i - 1
!
! Loop over elements, masses and target isomers and read subentry list
!
    do Z = 1, numZ
      if (Z < Zmin .or. Z > Zmax) cycle
      do A = 0, numA
        if (A > 0 .and. A < Z ) cycle
        if (A < Amin .or. A > Amax) cycle
        Astring = '   '
        write(Astring(1:3),'(i3.3)') A
        do iso = 0, numisom
          nuclide = trim(nuc(Z))//Astring//isostring(iso)
          nucexist = .false.
          do i = 1, Nlist
            if (trim(nuclide) == trim(nuc_list(i))) then
              nucexist = .true.
              exit
            endif
          enddo
          if (.not. nucexist) exit
          write( * , * ) "Processing ",ptype," + ",nuclide
          list = trim(filespath)//'C5reac/'//ptype//'/'//trim(nuclide)//'/'//ptype//'-'//trim(nuclide)//'.list'
!
! Loop over reactions and data types
!
          call process_xs(ptype, nuclide, list)
          call process_ratios(ptype, nuclide, list)
          call process_ri(ptype, nuclide, list)
          call process_sacs(ptype, nuclide, list)
          call process_fy(ptype, nuclide, list)
          call process_ad(ptype, nuclide, list)
          call process_respar(ptype, nuclide, list)
          call process_nubar(ptype, nuclide, list)
          call process_ddx(ptype, nuclide, list)
          if (ptype == 'n' .and. iso == 0) call process_pseudo(Z,A,nuclide)
!
! Jump out early for testing purposes
!
          if (Nsub_xc5 > maxentry) then
            exit Loop1
          endif
        enddo
      enddo
    enddo
  enddo Loop1
  call closefiles
  write( * , * ) "Processing XC5 database done"
  write( * , * ) "Sorting dictionaries......"
  if (flagstat) call filesort
  write( * , * ) "Dictionaries sorting done"
  return
end subroutine processxc5
! Copyright A.J. Koning 2021
