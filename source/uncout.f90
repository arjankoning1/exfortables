subroutine uncout
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Output of uncertainty analysis
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
!              isochar, &    ! character for isomer
!              MTexist, &    ! flag for existence of MT
!              mtinclude, &  ! flag to include particle
!              MTuncbin, &   ! uncertainty per bin
!              numbin, &     ! maximum number of bins
!              numisom, &    ! maximum number of isomers
!              nummt, &       ! maximum number of MT numbers
!              numpar, &     ! maximum number of particles
!              partype, &    ! symbol of particle
!              parinclude, & ! flag to include particle
!              reacid, &     ! reaction string
!              uncbin        ! uncertainty bin
!
! *** Declaration of local data
!
  implicit none
  character(len=15) :: col(15)    ! header
  character(len=15) :: un(15)    ! header
  character(len=80) :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  integer           :: is           ! isomer
  integer           :: j            ! counter
  integer           :: Ncol
  integer           :: indent
  integer           :: id2
  integer           :: k            ! counter
  integer           :: MT           ! MT number
  character(len=80) :: mtuncfile    ! file with uncertainties
!
! ****************** Output of (sorted) uncertainties ******************
!
  write(*, *) "Writing uncertainties....."
!
! Uncertainties per MT number and bin
!
  indent = 0
  id2 = indent + 2
  do k = 0, numpar
    if (.not. parinclude(k)) cycle
    do MT = 1, nummt
      if ( .not. mtinclude(MT)) cycle
      do is = - 1, numisom
        if ( .not. MTexist(k, MT, is)) cycle
        write(reacid(MT, is)(2:2), '(a1)') partype(k)
        mtuncfile = partype(k)//'-MT000        '
        write(mtuncfile(5:7), '(i3.3)') MT
        if (is >= 0) mtuncfile = trim(mtuncfile)//isochar(is)
        mtuncfile = trim(mtuncfile)//'.histo'
        open (unit = 1, status = 'unknown',  file = 'stat/unc/histo/'//mtuncfile)
        topline = 'Histogram of uncertainties for '//reacid(MT, is)
        quantity = 'Sets'
        call write_header(indent,topline,source,user,date,oformat)
        call write_reaction(indent,reacid(MT, is),0.D0,0.D0,3,MT)
        call write_integer(id2,'number of data sets',MTuncsets(k, MT, is))
        un = ''
        col = ''
        col(1) = 'bin'
        col(2) = 'Sets'
        Ncol=2
        call write_datablock(id2,Ncol,numbin+1,col,un)
        do j = 0, numbin
          write(1, '(f15.6, 3x, i6)') uncbin(j), MTuncbin(k, MT, is, j)
        enddo
        close(unit = 1)
!       open (unit = 13, status = 'new', file = 'stat/unc/histo/'//mtuncfile)
!       write(13, '("#Uncertainties for MT = ", i3, " ", a10)') MT, reacid(MT, is)
!       write(13, '("#  % bin      #sets")')
!       do j = 0, numbin
!         write(13, '(1p, g11.3, 0p, i6)') uncbin(j), MTuncbin(k, MT, is, j)
!       enddo
!       close(unit = 13)
      enddo
    enddo
  enddo
  write(*, *) "Writing uncertainties done"
  return
end subroutine uncout
! Copyright A.J. Koning 2019
