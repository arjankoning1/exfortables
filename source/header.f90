subroutine header
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Write header for output file
!
! Revision    Date      Author      Quality  Description
! ======================================================
!    1     13-08-2023   A.J. Koning    A     Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
! *** Use data from other modules
  use A0_exfortables_mod
!
! use A1_exfortables_mod, only: & ! All global variables
!              A_xc5, &        ! mass number
!              Ares, &         ! residual A
!              author2, &      ! author name
!              dEinc, &        ! uncertainty of incident energy
!              Eexc, &         ! excitation energy
!              Eexc2, &        ! excitation energy
!              Einc, &         ! incident energy
!              Elev, &         ! energy of discrete level
!              flagddx, &      ! flag for DDX emission spectrum
!              flagdiscang, &  ! flag for discrete angular distribution
!              flagelang, &    ! flag for elastic angular distribution
!              flagfy, &       ! flag for fission yields
!              flagtke, &      ! flag for fission yields
!              flaginelang, &  ! flag for inelastic angular distribution
!              flagMT, &       ! flag for MT cross section
!              flagnubar, &    ! flag for nubar
!              flagratio, &    ! flag for cross section ratio
!              flagres, &      ! flag for residual production cross section
!              flagrespar, &   ! flag for resonance parameters
!              flagri, &       ! flag for resonance integral
!              flagspec, &     ! flag for emission spectrum
!              flagspecav, &   ! flag for spectrum average
!              flagxs, &       ! flag for cross section
!              frame, &        ! reference fram
!              isom_xc5, &     ! number of isomer
!              kres, &         ! type of outgoing particle
!              Lrat, &         ! isomer for ratio
!              MF_xc5, &       ! MF number
!              MT_xc5, &       ! MT number
!              MTrat, &        ! MT number for ratio
!              Npoints_xc5, &  ! number of points per subentry
!              partype, &      ! symbol of particle
!              proj, &         ! projectile
!              reacid, &       ! reaction string
!              reaction_xc5, & ! XC5 reaction
!              resiso, &       ! isomer of residual nuclide
!              subentry_xc5, & ! XC5 subentry number
!              isochar_xc5, &  ! isomer of target
!              year_xc5, &     ! year
!              Z_xc5, &        ! charge number
!              Zres            ! residual Z
!
! *** Declaration of local data
!
  implicit none
  character(len=1) :: pro          !
  character(len=1) :: iso          !
  character(len=3) :: Astring
  character(len=6) :: finalnuclide
  character(len=9) :: subentry     !
  character(len=13) :: Estring     !
  character(len=80) :: jsonfile     !
  character(len=18) :: reaction   ! reaction
  character(len=15) :: col(6)    ! header
  character(len=15) :: un(6)    ! header
  character(len=80) :: quantity   ! quantity
  character(len=132) :: topline    ! topline
  character(len=80) :: author
  integer          :: A            !
  integer          :: is           !
  integer          :: Ncol
  integer          :: Np
  integer          :: k0
  integer          :: k1           !
  integer          :: level
  integer          :: MF           !
  integer          :: MT           !
  integer          :: Z            !
  integer          :: ZCN          !
  integer          :: ACN          !
  integer          :: indent
  integer          :: id2
  integer          :: id4
!
! ************************* Write file header **************************
!
  indent = 0
  id2 = indent + 2
  id4 = indent + 4
  col = ''
  quantity = ''
  reaction = ''
  Z = Z_xc5
  A = A_xc5
  MF = MF_xc5
  MT = MT_xc5
  pro = proj
  k1 = kres
  is = isom_xc5
  subentry = subentry_xc5
  author = author2
! write(1, '("# Target Z    : ", i3)') Z
! write(1, '("# Target A    : ", i3)') A
! write(1, '("# Target state: ", a1)') isochar_xc5
! write(1, '("# Projectile  : ", a1)') pro
  Astring = '   '
  write(Astring(1:3),'(i3)') A
  targetnuclide = trim(nuc(Z))//trim(adjustl(Astring))//isochar_xc5
  Ztarget = Z
  Atarget = A
!
! Residual nuclides
!
  if (MT >= 4 .and. MT <= 117 .and. MT /=5) then
    k0 = k0_xc5
    if (MT == 18) then
      Ares = 0
    else
      ZCN = Z + parZ(k0)
      ACN = A  + parA(k0)
      Zres = ZCN - zejec(MT)
      Ares = ACN - zejec(MT) - nejec(MT)
      Astring = '   '
      write(Astring(1:3),'(i3)') Ares
      finalnuclide = trim(nuc(Zres))//trim(adjustl(Astring))//resiso
    endif
  endif
!
! Determine discrete level
!
  level = 0
  if (MT >= 51 .and. MT <= 90) level = MT - 50
  if (MT >= 600 .and. MT <= 649) level = MT - 600
  if (MT >= 650 .and. MT <= 699) level = MT - 650
  if (MT >= 700 .and. MT <= 749) level = MT - 700
  if (MT >= 750 .and. MT <= 799) level = MT - 750
  if (MT >= 800 .and. MT <= 849) level = MT - 800
  if (flagMT) then
    write(reacid(MT, is)(2:2), '(a1)') pro
    reaction = reacid(MT, -1)
  endif
  if (flagres) then
    Astring = '   '
    write(Astring(1:3),'(i3)') Ares
    finalnuclide = trim(nuc(Zres))//trim(adjustl(Astring))//resiso
    reaction = '('//pro//',x)'
  endif
!
! Make JSON files
!
  if (.not. flagMTequiv) then
    jsonfile = 'stat/json/' // trim(subentry) // '.json'
    open (unit = 21, status = 'unknown', file = jsonfile)
    write(21,'("{")')
    write(21,'("    ""Subentry""      : """,a,""",")') trim(subentry)
    write(21,'("    ""Author""        : """,a,""",")') trim(authorcode)
    write(21,'("    ""Year""          : ",i4,",")') year_xc5
    write(21,'("    ""Projectile""    : """,a1,""",")') pro
    write(21,'("    ""Target Z""      : ",i4,",")') Z
    write(21,'("    ""Target A""      : ",i4,",")') A
    write(21,'("    ""Target nuclide"": """,a,""",")') trim(targetnuclide)
    if (isochar_xc5 == '') then
      iso = '0'
    else
      iso = isochar_xc5
    endif
    write(21,'("    ""Target state""  : """,a1,""",")') iso
!
! Cross sections
!
    if (flagMT) then
!   write(1, '("# Reaction    : ", a10)') reacid(MT, is)
!   write(1, '("# Final state : ", a1)') resiso
!   if (MT >= 51 .and. MT <= 90 .or. (MT >= 600 .and. MT <= 849)) then
!     if (Eexc2 == 0.) then
!       write(1, '("# E-exc       :", f9.5, " MeV (EXFOR:", f9.5, ")")') Elev, Eexc
!     else
!       write(1, '("# E-exc       :", f9.5, " MeV (EXFOR:", 2f9.5, ")")') Elev, Eexc, Eexc2
!     endif
!   endif
!   write(1, '("# Quantity    : Cross section")')
      write(21,'("    ""Reaction""      : """,a,""",")') trim(reacid(MT, is))
      write(21,'("    ""Final state""   : """,a1,""",")') resiso
      write(21,'("    ""Quantity""      : """,a,""",")') 'Cross section'
    endif
!
! Other data
!
    if (flagres) then
!   write(1, '("# Reaction    : (", a1, ", x)")') pro
!   write(1, '("# Final Z     : ", i3)') Zres
!   write(1, '("# Final A     : ", i3)') Ares
!   write(1, '("# Final state : ", a1)') resiso
!   write(1, '("# Quantity    : Cross section")')
      write(21,'("    ""Reaction""      : ""(",a,",x)"",")') pro
      write(21,'("    ""Final Z""       : ",i4,",")') Zres
      write(21,'("    ""Final A""       : ",i4,",")') Ares
      write(21,'("    ""Final state""   : """,a1,""",")') resiso
      write(21,'("    ""Quantity""      : """,a,""",")') 'Cross section'
    endif
    if (flagMT .or. flagres) then
      write(21,'("    ""X4 Reaction""   : """,a,""",")') trim(reaction_xc5)
      write(21,'("    ""MF""            : ",i4,",")') MF
      write(21,'("    ""MT""            : ",i4,",")') MT
    endif
    write(21,'("}")')
    close(21)
    open (unit = 81, status = 'unknown', position = 'append', file = 'stat/json/json.list')
      write(81,'(a)') trim(subentry) // '.json'
    close(81)
  endif
  if (flagratio) then
    reaction='ratio'
!   write(1, '("# Reaction    : ratio")')
!   write(1, '("# Final state : ", a1)') resiso
!   write(1, '("# MTrat       : ", i4)') MTrat
!   write(1, '("# Ratio isomer: ", i2)') Lrat
!   write(1, '("# Quantity    : Cross section ratio")')
  endif
  if (flagnubar) then
!   write(1, '("# Reaction    : (", a1, ", f)")') pro
    reaction = '('//pro//',f)'
!   if (MT == 452) write(1, '("# Quantity    : Total nubar")')
!   if (MT == 455) write(1, '("# Quantity    : Delayed nubar")')
!   if (MT == 456) write(1, '("# Quantity    : Prompt nubar")')
  endif
  if (flagfy) then
!   write(1, '("# Reaction    : (", a1, ", f)")') pro
!   write(1, '("# E-inc       :", 1p, e12.5, " MeV +-", e12.5, " MeV")') Einc, dEinc
!   write(1, '("# Quantity    : Fission yields")')
  endif
  if (flagtke) then
!   write(1, '("# Reaction    : (", a1, ", f)")') pro
!   if (MT == 461) then
!     write(1, '("# Quantity    : Pre-neutron emission TKE")')
!   else
!     write(1, '("# Quantity    : Pre-neutron emission TKE")')
!   endif
  endif
!   if (flaginelang) then
!     write(1, '("# Reaction    : Inelastic scattering")')
!   else
!     write(1, '("# Reaction    : Exchange  scattering")')
!   endif
!   write(1, '("# E-inc       : ", f8.3, " MeV")') Einc
!   if (Eexc2 == 0.) then
!     write(1, '("# E-exc       :", f9.5, " MeV (EXFOR:", f9.5, ")")') Elev, Eexc
!   else
!     write(1, '("# E-exc       :", f9.5, " MeV (EXFOR:", 2f9.5, ")")') Elev, Eexc, Eexc2
!   endif
!   write(1, '("# Quantity    : Angular distribution")')
! endif
! write(1, '("# Frame       : ", a1)') frame
! write(1, '("# MF          : ", i4)') MF
! write(1, '("# MT          : ", i4)') MT
! write(1, '("# X4 Subentry : ", a9)') subentry
! write(1, '("# X4 Reaction : ", a)') trim(reaction_xc5)
! write(1, '("# Author      : ", a25)') author2
! write(1, '("# Year        : ", i4)') year_xc5
! write(1, '("# Data points : ", i6)') Npoints_new
!
! Normalize Levkovski data
!
  if (subentry(1:5) == 'A0510') author ='Levkovski*0.82'
!
! Write tables
!
  Ncol=4
  Np=Npoints_xc5
  if (flagelang .or. flagdiscang) then
    quantity='angular distribution'
    if (flagelang) reaction='('//pro//',el)'
    if (flagdiscang) then
      if (flaginelang) then
        reaction='('//pro//','//pro//')'
      else
        reaction='('//pro//','//partype(k1)//')'
      endif
    endif
    un = 'mb/sr'
    col(1)='Angle'
    un(1)='deg'
    col(2)='xs'
    col(3)='dxs'
    Ncol=3
    Np= Nend_new - Eindex + 1
  endif
  if (flagri) then
    quantity='resonance integral'
    reaction='('//pro//',g)'
    un = ''
    col(1)='Elow'
    un(1)='eV'
    col(2)='Ehigh'
    un(2)='eV'
    col(3)='Res._Integral'
    col(4)='dRes._Integral'
  endif
  if (flagrespar) then
    quantity='resonance parameter'
    reaction='('//pro//',res)'
    un = ''
    col(1)='E'
    un(1)='eV'
    col(2)='dE'
    un(2)='eV'
    col(3)='par'
    col(4)='dpar'
  endif
  if (flagfy) then
    quantity='fission yield'
    reaction='('//pro//',f)'
    un = ''
    col(1)='Z'
    col(2)='A'
    col(3)='Isomer'
    col(4)='Yield'
    col(5)='dYield'
    Ncol=5
    Np= Nend_new - Eindex + 1
  endif
  if (flagtke) then
    quantity='total kinetic energy'
    reaction='('//pro//',f)'
    un = 'MeV'
    col(1)='E'
    col(2)='dE'
    col(3)='TKE'
    col(4)='dTKE'
  endif
  if (flagspecav) then
    quantity='spectrum averaged cross section'
    reaction='('//pro//',sacs)'
    un = ''
    col(1)='Elow'
    un(1)='eV'
    col(2)='Ehigh'
    un(2)='eV'
    col(3)='Average'
    col(4)='dAverage'
  endif
  if (flagnubar) then
    quantity='neutron multiplicity'
    if (MT == 452) quantity=trim(quantity)//' - prompt'
    if (MT == 455) quantity=trim(quantity)//' - delayed'
    if (MT == 456) quantity=trim(quantity)//' - total'
    reaction='('//pro//',f)'
    un = ''
    col(1)='E'
    un(1)='MeV'
    col(2)='dE'
    un(2)='MeV'
    col(3)='nubar'
    col(4)='dnubar'
  endif
  if (flagratio) then
    quantity='ratio'
    reaction='('//pro//',ratio)'
    un = ''
    col(1)='E'
    un(1)='MeV'
    col(2)='dE'
    un(2)='MeV'
    col(3)='ratio'
    col(4)='dratio'
  endif
  if (flagspec) then
    quantity='emission spectrum'
    reaction='('//pro//',x'//partype(k1)//')'
    un = 'MeV'
    col(1)='E-out'
    col(2)='dE-out'
    col(3)='xs'
    un(3)='mb/MeV.sr'
    col(4)='dxs'
    un(4)='mb/MeV.sr'
    Np= Nend_new - Eindex + 1
  endif
  if (flagddx) then
    quantity='DDX'
    reaction='('//pro//',x'//partype(k1)//')'
    un = 'MeV'
    col(1)='E-out'
    col(2)='dE-out'
    col(3)='xs'
    un(3)='mb/MeV.sr'
    col(4)='dxs'
    un(4)='mb/MeV.sr'
    Np= Nend_new - Eindex + 1
  endif
  if (flagxs) then
    quantity='cross section'
    un = 'MeV'
    col(1)='E'
    col(2)='dE'
    col(3)='xs'
    un(3)='mb'
    col(4)='dxs'
    un(4)='mb'
  endif
  Ncol = Ncol + 1
  col(Ncol)='Normalization'
  un(Ncol)=''
  if (flagxs .and. Ares > 0) then
    topline=trim(targetnuclide)//trim(reaction)//trim(finalnuclide)//' '//trim(quantity)
  else
    topline=trim(targetnuclide)//trim(reaction)//' '//trim(quantity)
  endif
  if (flagelang .or. flagdiscang .or. flagspec .or. flagddx) then
    Estring=''
    write(Estring,'(es13.6)') Einc
    topline=trim(topline)//' at '//Estring//' MeV'
  endif
  call write_header(indent,topline,source,user,date,oformat)
  call write_exfor(indent,author,year_xc5,subentry,reaction_xc5)
  if ((MT >= 51 .and. MT <= 90) .or. (MT >= 600 .and. MT <= 849)) write(1,'("#   level energy [MeV]:", es13.5)') Eexc
  call write_target(indent)
  call write_reaction(indent,reaction,0.D0,0.D0,MF,MT)
  if ((MT >= 51 .and. MT <= 90) .or. (MT >= 600 .and. MT <= 849)) call write_level(id2,-1,level,Elev, Jlev,Plev,0.)
  if (flagelang .or. flagdiscang .or. flagspec .or. flagddx) call write_real(id2,'E-incident [MeV]',Einc)
  if (flagxs .and. Ares > 0) call write_residual(id2,Zres,Ares,finalnuclide)
  if (resiso /= ' ' .and. .not.flagfy) call write_level(id4,-1,level,0., 0.,0,0.)
  call write_quantity(id2,quantity)
  call write_datablock(id2,Ncol,Np,col,un)
  return
end subroutine header
! Copyright A.J. Koning 2019
