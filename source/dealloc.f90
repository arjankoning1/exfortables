subroutine dealloc
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: (De-)Allocate arrays
!
! Revision    Date      Author           Description
! ====================================================
!    1     01-01-2019   A.J. Koning      Original code
!-----------------------------------------------------------------------------------------------------------------------------------
!
use A0_exfortables_mod
deallocate(inline) ! input line
deallocate(partype) ! symbol of particle
deallocate(nuc) ! symbol of nucleus
deallocate(parname) ! name of particle
deallocate(mainis) ! main isotope
deallocate(parA) ! mass number of particle
deallocate(parZ) ! charge number of particle
deallocate(libinclude) ! flag to include library
deallocate(talfile) ! name of TALYS output file
deallocate(reacid) ! reaction string
deallocate(reacstring) ! reaction string
deallocate(library) ! nuclear data library
deallocate(isochar) ! character for isomer
deallocate(reac) ! reaction identifier
deallocate(ebin) ! lower bound of energy bin
deallocate(ecbin) ! energy bin
deallocate(uncbin) ! uncertainty bin
deallocate(Fsigma) ! deviation for F factor
deallocate(libweight) ! weight per nuclear data library
deallocate(mass) ! mass number
deallocate(title_xc5) ! title of paper
deallocate(authors_xc5) ! authors of paper
deallocate(reference_xc5) ! reference of paper
deallocate(string_xc5) ! date according to NDRC file
deallocate(valstring) ! XC5 string with values
deallocate(NPgroup) ! number of points in group
deallocate(Zfy) ! Z of fission yield
deallocate(Afy) ! A of fission yield
deallocate(Ify) ! isomer of fission yield
deallocate(E) ! incident energy
deallocate(dE) ! incident energy uncertainty
deallocate(xs) ! cross section
deallocate(dxs) ! cross section uncertainty
deallocate(Fnorm) 
deallocate(angle) ! angle
deallocate(cosang) ! cosine of angle
deallocate(xsr) ! cross section divided by Rutherford
deallocate(dxsr) ! uncertainty of cross section divided by Rutherford
deallocate(Npoints) ! number of points compared per library
deallocate(comppoint) ! designator for comparison of point
deallocate(xsthpoint) ! theoretical cross section
deallocate(chi2point) ! chi-square
deallocate(Rpoint) ! R value
deallocate(Fpoint) ! F value
deallocate(Aspoint) ! asymmetry
deallocate(Flogpoint) ! log of F value
deallocate(Aslogpoint) ! log of asymmetry
deallocate(etal) ! energy of TALYS
deallocate(xstal) ! cross section of TALYS
deallocate(Enon) ! energy for non-elastic cross section
deallocate(xsnon) ! non-elastic cross section
deallocate(xsnonel) ! non-elastic cross section
deallocate(libexist) ! flag for existence of library
deallocate(entry_sub) !
deallocate(Nlib) ! number of data libraries for comparison
deallocate(elib) ! energy of library
deallocate(xslib) ! cross section of library
deallocate(dxslib) ! cross section uncertainty of library
deallocate(subentry_E)   ! XC5 subentry number
deallocate(author1_E)    ! author
deallocate(reaction_E)   ! XC5 reaction
deallocate(title_E)      ! title of paper
deallocate(authors_E)    ! authors of paper
deallocate(reference_E)  ! reference of paper
deallocate(year_E)       ! year
deallocate(expfile)    ! experimental data file
deallocate(Npoints_E)        !
deallocate(Emax_E)         ! maximum energy (MeV) of subentry
deallocate(Emin_E)         ! minimum energy (MeV) of subentry
deallocate(Eunc)         ! average energy uncertainty of subentry
deallocate(Euncabs)      ! average absolute energy uncertainty of subentry
deallocate(Nexp)         ! number of experiments
deallocate(Eexp)         ! incident energy
deallocate(dEexp)        ! incident energy uncertainty
deallocate(xsexp)        ! cross section
deallocate(xsexplib)        ! cross section
deallocate(dxsexplib)        ! cross section uncertainty
deallocate(xsexpint)        ! cross section
deallocate(dxsexpint)        ! cross section uncertainty
deallocate(dxsexp)       ! cross section uncertainty
deallocate(xsexpav)      ! average experimental cross section
deallocate(uncertainty)  ! average uncertainty of subentry
deallocate(MTexist_xc5) ! flag for existence of MT
!deallocate(ZAMTexist)
deallocate(ZAresexist)
deallocate(ZAexist)
deallocate(processed) ! flag for processed subentry
deallocate(NAix) ! number of A's per k0,Z
deallocate(Aix) ! index for A
deallocate(MTix) ! index for MT
deallocate(NpointsnucMT) ! number of points per nucleus and MT nu
deallocate(NpointsparE) ! number of points per particle and ener
deallocate(NpointsallE) ! number of points per energy bin
deallocate(NpointsMTE) ! number of points per MT number and ene
deallocate(NpointsnucE) ! number of points per nucleus and energ
deallocate(Npointsset) ! number of points per subentry
deallocate(Npointsall) ! total number of points
deallocate(Npointsentry) ! number of points per entry
deallocate(NpointsMT) ! number of points per MT number
deallocate(Npointspar) ! number of points per particle
deallocate(Npointsnuc) ! number of points per nucleus
deallocate(Nsetsnuc) ! number of data sets per nucleus
deallocate(Nsetsentry) ! number of data sets per entry
deallocate(MTsumtot) ! total F value per particle per MT
deallocate(Nsetsall) ! total number of data sets
deallocate(NsetsMTtot) ! number of sets per MT number
deallocate(NsetsallMTtot) ! all MT sets
deallocate(NsetsallMT) ! all MT sets
deallocate(NsetsMT) ! number of data sets per MT number
deallocate(NsetsnucMT) ! number of data sets per nucleus and MT
deallocate(NsetsAMT) ! all MT sets
deallocate(NpointsAMT) ! total number of points per MT number
deallocate(Nsetspar) ! number of data sets per particle
deallocate(NsetsnucE) ! number of data sets per nuclide and en
deallocate(NsetsparE) ! number of data sets per particle and e
deallocate(NsetsMTE) ! number of data sets per MT number and
deallocate(NsetsallE) ! total number of data sets for all ener
deallocate(FMT) ! average F value per MT number
deallocate(FMTlog) ! average log F value per MT number
deallocate(Fnuc) ! average F value per nucleus
deallocate(Fnuclog) ! average log F value per nucleus
deallocate(FnucMTlog) ! average F value per nucleus and MT num
deallocate(FnucMT) ! average log F value per nucleus and MT
deallocate(FAMT) ! average F for all MT numbers
deallocate(Fpar) ! average F value per particle
deallocate(Fall) ! average F value for all reactions
deallocate(Fparlog) ! average log F value per particle
deallocate(Falllog) ! average log F value for all reactions
deallocate(FMTE) ! F per MT number and energy bin
deallocate(FMTElog) ! log of F per MT number and energy bin
deallocate(FnucE) ! F per nucleus and energy bin
deallocate(FnucElog) ! log of F per nucleus and energy bin
deallocate(FparE) ! F per particle and energy bin
deallocate(FparElog) ! log of F per particle and energy bin
deallocate(Fentrylog) ! average log F value per entry
deallocate(Fentry) ! average F value per entry
deallocate(FallE) ! total F per energy bin
deallocate(FallElog) ! log of total F per energy bin
deallocate(pvalue) ! pvalue
deallocate(xsthset) ! average theoretical cross section per
deallocate(Fset) ! average F value per subentry
deallocate(pset) ! average p value per subentry
deallocate(chi2set) ! average Chi-2 per subentry
deallocate(Rset) ! average R per subentry
deallocate(Asset) ! average As value per subentry
deallocate(Qcomment) ! quality comment
deallocate(Qentry) ! entry with quality information
deallocate(MTexist) ! flag for existence of MT
deallocate(MTsum) ! number of reactions per MT number
deallocate(MTbin) ! number of F values in MT bin
deallocate(MTbintot) ! total number of F values in MT bin
deallocate(MTuncbin) ! uncertainty per bin
deallocate(MTuncsets) !
deallocate(QMT) ! quality per MT number
deallocate(QMTall) ! total quality per MT number
deallocate(fbin) ! boundaries for F bins
deallocate(MTbinav) ! average F value per bin
deallocate(MTbinsigma) ! average F deviation per bin
deallocate(MTbincum) ! cumulative F value per MT number
deallocate(Ffinal) ! Final F value per bin
end subroutine dealloc
! Copyright A.J. Koning 2019
