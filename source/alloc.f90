  subroutine alloc
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
allocate(inline(numinlines)) ! input line
allocate(partype(0:numpar)) ! symbol of particle
allocate(nuc(0:110)) ! symbol of nucleus
allocate(parname(0:numpar)) ! name of particle
allocate(mainis(0:110)) ! main isotope
allocate(parA(0:numpar)) ! mass number of particle
allocate(parZ(0:numpar)) ! charge number of particle
allocate(libinclude(numlib)) ! flag to include library
allocate(talfile(nummt)) ! name of TALYS output file
allocate(reacid(nummt,-1:numisom)) ! reaction string
allocate(reacstring(nummt)) ! reaction string
allocate(library(0:numlib)) ! nuclear data library
allocate(isochar(-1:numisom)) ! character for isomer
allocate(reac(nummt)) ! reaction identifier
allocate(ebin(0:numEbin)) ! lower bound of energy bin
allocate(ecbin(0:numEbin)) ! energy bin
allocate(uncbin(0:numbin)) ! uncertainty bin
allocate(Fsigma(nummt,-1:numisom,2)) ! deviation for F factor
allocate(libweight(numlib)) ! weight per nuclear data library
allocate(mass(numZ,numiso)) ! mass number
allocate(Qsub0(numsub)) ! subentry with quality information
allocate(subweight(numsub)) !
allocate(Qscore0(numsub)) ! quality score
allocate(weight0(numsub)) !
allocate(Qdate0(numsub)) ! quality date
allocate(Qaction0(numsub)) ! recommended action
allocate(title_xc5(numref)) ! title of paper
allocate(authors_xc5(numref)) ! authors of paper
allocate(reference_xc5(numref)) ! reference of paper
allocate(string_xc5(numpoint)) ! date according to NDRC file
!allocate(ZAMTexist(0:numpar, numZ, 0:numA, 0:numisom, nummt, -1:numisom)) ! logical for Z, A, MT existence
allocate(ZAresexist(0:numpar, numZ, 0:numA, 0:numisom)) ! logical for Z, A, rp existence
allocate(ZAexist(0:numpar, numZ, 0:numA, 0:numisom)) ! logical for Z, A existence
allocate(valstring(8)) ! XC5 string with values
allocate(NPgroup(numsub)) ! number of points in group
allocate(Zfy(numpoint)) ! Z of fission yield
allocate(Afy(numpoint)) ! A of fission yield
allocate(Ify(numpoint)) ! isomer of fission yield
allocate(E(numpoint)) ! incident energy uncertainty
allocate(dE(numpoint)) ! incident energy uncertainty
allocate(xs(numpoint)) ! cross section
allocate(dxs(numpoint)) ! cross section uncertainty
allocate(Fnorm(numpoint))
allocate(angle(numpoint)) ! angle
allocate(cosang(numpoint)) ! cosine of angle
allocate(xsr(numpoint)) ! cross section divided by Rutherford
allocate(dxsr(numpoint)) ! uncertainty of cross section divided by Rutherford
allocate(Npoints(0:numlib)) ! number of points compared per library
allocate(comppoint(0:numlib,numpoint)) ! designator for comparison of point
allocate(xsthpoint(0:numlib,numpoint)) ! theoretical cross section
allocate(chi2point(0:numlib,numpoint)) ! chi-square
allocate(Rpoint(0:numlib,numpoint)) ! R value
allocate(Fpoint(0:numlib,numpoint)) ! F value
allocate(Aspoint(0:numlib,numpoint)) ! asymmetry
allocate(Flogpoint(0:numlib,numpoint)) ! log of F value
allocate(Aslogpoint(0:numlib,numpoint)) ! log of asymmetry
allocate(etal(0:numpoint)) ! energy of TALYS
allocate(xstal(0:numpoint)) ! cross section of TALYS
allocate(Enon(0:numpoint)) ! energy for non-elastic cross section
allocate(xsnon(0:numpoint)) ! non-elastic cross section
allocate(libexist(0:numlib)) ! flag for existence of library
allocate(entry_sub(0:nument)) !
allocate(Nlib(0:numlib)) ! number of data libraries for comparison
allocate(elib(numlib,0:numpoint)) ! energy of library
allocate(xslib(numlib,0:numpoint)) ! cross section of library
allocate(dxslib(numlib,0:numpoint)) ! cross section uncertainty of library
allocate(subentry_E(numsets))   ! XC5 subentry number
allocate(author1_E(numsets))    ! author
allocate(reaction_E(numsets))   ! XC5 reaction
allocate(title_E(numsets))      ! title of paper
allocate(authors_E(numsets))    ! authors of paper
allocate(reference_E(numsets))  ! reference of paper
allocate(year_E(numsets))       ! year
allocate(Npoints_E(numsets))    ! number of experimental data points
allocate(expfile(numsets))    ! experimental data file
allocate(Emax_E(numsets))         ! maximum energy (MeV) of subentry
allocate(Emin_E(numsets))         ! minimum energy (MeV) of subentry
allocate(Eunc(numsets))         ! average energy uncertainty of subentry
allocate(Euncabs(numsets))      ! average absolute energy uncertainty of subentry
allocate(Nexp(numsets, numpoint))         ! number of experiments
allocate(Eexp(numsets, numpoint))         ! incident energy
allocate(dEexp(numsets, numpoint))        ! incident energy uncertainty
allocate(xsexp(numsets, numpoint))        ! cross section
allocate(dxsexp(numsets, numpoint))       ! cross section uncertainty
allocate(xsexplib(numlib,numsets, numpoint))        ! cross section
allocate(dxsexplib(numlib,numsets, numpoint))        ! cross section uncertainty
allocate(xsexpint(numlib,numsets, numpoint))        ! cross section
allocate(dxsexpint(numlib,numsets, numpoint))        ! cross section uncertainty
allocate(xsnonel(numsets, numpoint))        ! cross section
allocate(xsexpav(numsets))      ! average experimental cross section
allocate(uncertainty(numsets))  ! average uncertainty of subentry
allocate(Aexist_xc5(0:numpar,numZ,0:numA)) ! flag for existence of A
allocate(MTexist_xc5(0:nummt)) ! flag for existence of MT
allocate(processed(0:numpar,nummt,-1:numisom)) ! flag for processed subentry
allocate(NAix(0:numpar,numZ)) ! number of A's per k0,Z
allocate(Aix(0:numpar,numZ,0:numA)) ! index for A
allocate(MTix(nummt)) ! index for MT
allocate(NpointsnucMT(0:numlib,0:numpar,numZ,0:numaix,nummtix,-1:numisom)) ! number of points per nucleus and MT nu
allocate(NpointsparE(0:numlib,0:numpar,0:numEbin)) ! number of points per particle and ener
allocate(NpointsallE(0:numlib,0:numEbin)) ! number of points per energy bin
allocate(NpointsMTE(0:numlib,0:numpar,nummtix,-1:numisom,0:numEbin)) ! number of points per MT number and ene
allocate(NpointsnucE(0:numlib,0:numpar,numZ,0:numaix,0:numEbin)) ! number of points per nucleus and energ
allocate(Npointsset(0:numlib)) ! number of points per subentry
allocate(Npointsall(0:numlib)) ! total number of points
allocate(Npointsentry(0:numlib,nument)) ! number of points per entry
allocate(NpointsMT(0:numlib,0:numpar,nummtix,-1:numisom)) ! number of points per MT number
allocate(Npointspar(0:numlib,0:numpar)) ! number of points per particle
allocate(Npointsnuc(0:numlib,0:numpar,numZ,0:numA)) ! number of points per nucleus
allocate(Nsetsnuc(0:numlib,0:numpar,numZ,0:numA)) ! number of data sets per nucleus
allocate(Nsetsentry(0:numlib,nument)) ! number of data sets per entry
allocate(MTsumtot(0:numpar)) ! total F value per particle per MT
allocate(Nsetsall(0:numlib)) ! total number of data sets
allocate(NsetsMTtot(0:numpar)) ! number of sets per MT number
allocate(NsetsallMTtot(0:numpar)) ! all MT sets
allocate(NsetsallMT(0:numpar,nummt,-1:numisom)) ! all MT sets
allocate(NsetsMT(0:numlib,0:numpar,nummtix,-1:numisom)) ! number of data sets per MT number
allocate(NsetsnucMT(0:numlib,0:numpar,numZ,0:numaix,nummtix,-1:numisom)) ! number of data sets per nucleus and MT
allocate(NsetsAMT(0:numlib,0:numpar,0:numA,nummtix,-1:numisom)) ! all MT sets
allocate(NpointsAMT(0:numlib,0:numpar,0:numA,nummtix,-1:numisom)) ! total number of points per MT number
allocate(Nsetspar(0:numlib,0:numpar)) ! number of data sets per particle
allocate(NsetsnucE(0:numlib,0:numpar,numZ,0:numaix,0:numEbin)) ! number of data sets per nuclide and en
allocate(NsetsparE(0:numlib,0:numpar,0:numEbin)) ! number of data sets per particle and e
allocate(NsetsMTE(0:numlib,0:numpar,nummtix,-1:numisom,0:numEbin)) ! number of data sets per MT number and
allocate(NsetsallE(0:numlib,0:numEbin)) ! total number of data sets for all ener
allocate(FMT(0:numlib,0:numpar,nummtix,-1:numisom)) ! average F value per MT number
allocate(FMTlog(0:numlib,0:numpar,nummtix,-1:numisom)) ! average log F value per MT number
allocate(Fnuc(0:numlib,0:numpar,numZ,0:numA)) ! average F value per nucleus
allocate(Fnuclog(0:numlib,0:numpar,numZ,0:numA)) ! average log F value per nucleus
allocate(FnucMTlog(0:numlib,0:numpar,numZ,0:numaix,nummtix,-1:numisom)) ! average F value per nucleus and MT num
allocate(FnucMT(0:numlib,0:numpar,numZ,0:numaix,nummtix,-1:numisom)) ! average log F value per nucleus and MT
allocate(FAMT(0:numlib,0:numpar,0:numA,nummtix,-1:numisom)) ! average F for all MT numbers
allocate(Fpar(0:numlib,0:numpar)) ! average F value per particle
allocate(Fall(0:numlib)) ! average F value for all reactions
allocate(Fparlog(0:numlib,0:numpar)) ! average log F value per particle
allocate(Falllog(0:numlib)) ! average log F value for all reactions
allocate(FMTE(0:numlib,0:numpar,nummtix,-1:numisom,0:numEbin)) ! F per MT number and energy bin
allocate(FMTElog(0:numlib,0:numpar,nummtix,-1:numisom,0:numEbin)) ! log of F per MT number and energy bin
allocate(FnucE(0:numlib,0:numpar,numZ,0:numaix,0:numEbin)) ! F per nucleus and energy bin
allocate(FnucElog(0:numlib,0:numpar,numZ,0:numaix,0:numEbin)) ! log of F per nucleus and energy bin
allocate(FparE(0:numlib,0:numpar,0:numEbin)) ! F per particle and energy bin
allocate(FparElog(0:numlib,0:numpar,0:numEbin)) ! log of F per particle and energy bin
allocate(Fentrylog(0:numlib,0:nument)) ! average log F value per entry
allocate(Fentry(0:numlib,0:nument)) ! average F value per entry
allocate(FallE(0:numlib,0:numEbin)) ! total F per energy bin
allocate(FallElog(0:numlib,0:numEbin)) ! log of total F per energy bin
allocate(pvalue(numsets,numpoint)) ! pvalue
allocate(xsthset(0:numlib)) ! average theoretical cross section per
allocate(Fset(0:numlib)) ! average F value per subentry
allocate(pset(numsets)) ! average p value per subentry
allocate(chi2set(0:numlib)) ! average Chi-2 per subentry
allocate(Rset(0:numlib)) ! average R per subentry
allocate(Asset(0:numlib)) ! average As value per subentry
allocate(Qcomment(0:numlib)) ! entry with quality information
allocate(Qentry(nument, 4, 3)) ! entry with quality information
allocate(MTexist(0:numpar,nummt,-1:numisom)) ! flag for existence of MT
allocate(MTsum(0:numpar,nummtix,-1:numisom)) ! number of reactions per MT number
allocate(MTbin(0:numpar,0:nummtix,-1:numisom,0:numbin)) ! number of F values in MT bin
allocate(MTbintot(0:numpar,nummtix,-1:numisom)) ! total number of F values in MT bin
allocate(MTuncbin(0:numpar,0:nummt,-1:numisom,0:numbin)) ! uncertainty per bin
allocate(MTuncsets(0:numpar,0:nummt,-1:numisom)) ! number of data sets with uncertainty
allocate(QMT(0:numpar,nummt,-1:numisom,4,3)) ! quality per MT number
allocate(QMTall(0:numpar,4,3)) ! total quality per MT number
allocate(fbin(0:numbin)) ! boundaries for F bins
allocate(MTbinav(0:numpar,nummtix,-1:numisom,0:numbin)) ! average F value per bin
allocate(MTbinsigma(0:numpar,nummtix,-1:numisom,2)) ! average F deviation per bin
allocate(MTbincum(0:numpar,nummtix,-1:numisom,0:numbin)) ! cumulative F value per MT number
allocate(Ffinal(0:numpar,0:nummtix,-1:numisom,0:numbin)) ! Final F value per bin
end subroutine alloc
! Copyright A.J. Koning 2019
