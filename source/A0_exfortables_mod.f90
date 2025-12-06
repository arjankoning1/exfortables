module A0_exfortables_mod
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose   : General module with all global variables
!
! Author    : Arjan Koning
!
! 2021-12-30: Original code
! 2025-12-01: Current version
!-----------------------------------------------------------------------------------------------------------------------------------
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Definition of single and double precision variables
!
!-----------------------------------------------------------------------------------------------------------------------------------
!
  integer, parameter :: sgl = selected_real_kind(6,37)   ! single precision kind
  integer, parameter :: dbl = selected_real_kind(15,307) ! double precision kind
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Definition of single and double precision variables
!
!-----------------------------------------------------------------------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: All global variables
!
!-----------------------------------------------------------------------------------------------------------------------------------
!
!
!
!
!-----------------------------------------------------------------------------------------------------------------------------------
!
! Array dimensions
!
  integer, parameter :: numpar=7        ! maximum number of particles
  integer, parameter :: numZ=110        ! maximum number of elements
  integer, parameter :: numinlines=100  ! maximum number of input lines
  integer, parameter :: numlib=8        ! maximum number of data libraries
  integer, parameter :: nummt=851       ! maximum number of MT numbers
  integer, parameter :: numEbin=40      ! maximum number of energy bins
  integer, parameter :: numbin=50       ! maximum number of bins
  integer, parameter :: numiso=10       ! maximum number of natural isotopes
  integer, parameter :: numsub=200000   ! maximum number of subentries
  integer, parameter :: numsubx4=200000 ! maximum number of subentries in X4
  integer, parameter :: nument=26000    ! maximum number of entries
  integer, parameter :: numref=100      ! maximum number of reference lines
  integer, parameter :: numsets=200     ! maximum number of experimental data sets per channel
  integer, parameter :: numpoint=100000 ! maximum number of data points in subentry
  integer, parameter :: nummtix=100     ! maximum index for MT
  integer, parameter :: numaix=20       ! maximum index for mass number
  integer, parameter :: numisom=2       ! maximum number of isomers
  integer, parameter :: numA=270        ! maximum number of masses
!
! machine
!
  character(len=10)  :: date      ! date
  character(len=132) :: libspath  ! directory containing data libraries
  character(len=132) :: filespath ! directory containing X4 and structure files to be read
  character(len=132) :: talyspath ! directory containing TALYS results
  integer            :: day       ! day
  integer            :: month     ! month
  integer            :: year      ! year
!
! readinput
!
  character(len=132), allocatable, dimension(:)         :: inline ! input line
  integer                                               :: nlines ! number of input lines
!
! constants
!
  character(len=1),  allocatable, dimension(:)        :: partype ! symbol of particle
  character(len=2),  allocatable, dimension(:)        :: nuc     ! symbol of nucleus
  character(len=8),  allocatable, dimension(:)        :: parname ! name of particle
  integer,  allocatable, dimension(:)                 :: mainis  ! main isotope
  integer,  allocatable, dimension(:)                 :: parA    ! mass number of particle
  integer,  allocatable, dimension(:)                 :: parZ    ! charge number of particle
  real(sgl)                                           :: e2c     ! square of elementary charge in MeV.fm
  real(sgl)                                           :: pi      ! pi
!
! input
!
  logical                       :: flagx4               ! flag to use original EXFOR information
  logical                       :: flagxc5              ! flag to use XC5 instead of XC5
  logical                       :: flagtables           ! flag to make new database
  logical                       :: flagstat             ! flag for statistics
  logical                       :: flagtalys            ! flag for TALYS comparison
  logical                       :: flaglib              ! flag for library comparison
  logical                       :: flagendfb            ! flag to include or exclude ENDFB from library average
  logical                       :: flagjendl            ! flag to include or exclude JENDL from library average
  logical                       :: flagjeff             ! flag to include or exclude JEFF from library average
  logical                       :: flagcendl            ! flag to include or exclude CENDL from library average
  logical                       :: flageaf              ! flag to include or exclude EAF from library average
  logical                       :: flagirdff            ! flag to include or exclude IRDFF from library average
  logical                       :: flagtendl            ! flag to include or exclude TENDL from library average
  logical                       :: flagunc              ! flag for uncertainty analysis
  logical                       :: flagremove           ! flag for removing existing database
  logical                       :: flageview            ! flag to make ECISVIEW file
  logical                       :: flagexpo             ! flag to use exponential RMS instead of power of 10
  logical                       :: flagxsonly           ! flag to process only cross sections
  logical                       :: parinclude(0:numpar) ! flag to include projectile
  logical                       :: mtinclude(nummt)     ! flag to include MT number
  logical                       :: flaggroup            ! flag to group resonance data
  logical                       :: flagout              ! flag for main output
  logical                       :: flagdexp             ! flag to use experimental uncertainty in F factor
  logical                       :: flagerf              ! flag to use error function for Frms
  logical                       :: flagoutliers         ! flag to use outliers for total Frms estimators
  logical                       :: flagreacstyle        ! flag to use reaction strings instead of MT numbers
  character(len=5)              :: pointcomp            ! reference for pointwise comparison
  character(len=5)              :: qualitycomp          ! reference for quality assignment
  integer                       :: Zmin                 ! minimal Z value to process
  integer                       :: Zmax                 ! maximal Z value to process
  integer                       :: Amin                 ! minimal A value to process
  integer                       :: Amax                 ! maximal A value to process
  integer                       :: maxentry             ! maximum number of X4 entries to be processed
  integer                       :: Nlibs                ! number of data libraries for comparison
  integer                       :: pointref             ! reference for pointwise comparison
  integer                       :: qualityref           ! reference for quality assignment
  real(sgl)                     :: xseps                ! minimum cross section (mb) for TALYS + library comparison
  real(sgl)                     :: talysemin            ! minimum energy (MeV) for TALYS comparison
  real(sgl)                     :: talysemax            ! maximum energy (MeV) for TALYS comparison
  real(sgl)                     :: allemin              ! minimum energy (MeV) for TALYS or library comparison
  real(sgl)                     :: allemax              ! maximum energy (MeV) for TALYS or library comparison
  real(sgl)                     :: fmax                 ! maximal F value per point taken into account
!
! reacinitial
!
  logical, allocatable, dimension(:)                          :: libinclude  ! flag to include library
  character(len=15), allocatable, dimension(:)                :: talfile     ! name of TALYS output file
  character(len=9), allocatable, dimension(:)                 :: reacstring  ! reaction string
  character(len=10), allocatable, dimension(:,:)              :: reacid      ! reaction string
  character(len=80)                                           :: libstring   ! string with library names
  character(len=200)                                          :: libstring2  ! string with library names
  character(len=10), allocatable, dimension(:)                :: library     ! nuclear data library
  character(len=1), allocatable, dimension(:)                 :: isochar     ! character for isomer
  character(len=132)                                          :: source      ! source of data
  character(len=132)                                          :: oformat     ! format of data
  character(len=132)                                          :: user        ! user of data
  integer, allocatable, dimension(:)                          :: reac        ! reaction identifier
  integer                                                     :: Nmod        ! number for output of progress of processing
  integer                                                     :: MTrp        ! MT number for residual production
  integer                                                     :: zejec(nummt) ! number of protons out from CN per MT number
  integer                                                     :: nejec(nummt) ! number of neutrons out from CN per MT number
  real(sgl), allocatable, dimension(:)                        :: ebin        ! lower bound of energy bin
  real(sgl), allocatable, dimension(:)                        :: ecbin       ! energy bin
  real(sgl), allocatable, dimension(:)                        :: uncbin      ! uncertainty bin
  real(sgl)                                                   :: base        ! base number for RMS
  real(sgl)                                                   :: limit       ! limit for exponent in RMS
  real(sgl), allocatable, dimension(:,:,:)                    :: Fsigma      ! deviation for F factor
  real(sgl), allocatable, dimension(:)                        :: libweight   ! weight per nuclear data library
!
! fileinitial
!
  character(len=80)                              :: levpath ! directory containing level files to be read
  integer, allocatable, dimension(:,:)           :: mass    ! mass number
!
! readquality
!
  character(len=9), allocatable, dimension(:)        :: Qsub0    ! subentry with quality information
  character(len=9), allocatable, dimension(:)        :: subweight! subentry with quality information
  character(len=2), allocatable, dimension(:)        :: Qscore0  ! quality score
  character(len=10), allocatable, dimension(:)       :: Qdate0   ! quality date
  character(len=150), allocatable, dimension(:)      :: Qaction0 ! recommended action
  integer                                            :: NQ       ! number of items with quality information
  integer                                            :: NW       ! number of items with weight information
  integer, allocatable, dimension(:)                 :: weight0  ! weight
!
! readsub
!
  logical                                                   :: flagsubdone         ! flag to signal end of subentry
  logical                                                   :: flagZAProd          ! flag for ZA of product
  character(len=1)                                          :: isochar_xc5         ! isomer
  character(len=6)                                          :: nuclide_xc5         ! nuclide
  character(len=8)                                          :: protar_xc5          ! projectile-nuclide
  character(len=9)                                          :: entry_xc5           ! entry
  character(len=25)                                         :: author1_xc5         ! author
  character(len=132), allocatable, dimension(:)             :: title_xc5           ! title of paper
  character(len=132), allocatable, dimension(:)             :: authors_xc5         ! authors of paper
  character(len=132), allocatable, dimension(:)             :: reference_xc5       ! reference of paper
  character(len=9)                                          :: subentry_xc5        ! XC5 subentry number
  character(len=9), allocatable, dimension(:)               :: entry_sub           ! entry of subentry
  character(len=132)                                        :: cfile               ! filename
  character(len=60)                                         :: reaction_xc5        ! XC5 reaction
  character(len=300), allocatable, dimension(:)             :: string_xc5          ! string
  integer                                                   :: iso_xc5             ! isomer
  integer                                                   :: date_xc5_nrdc       ! date according to NDRC file
  integer                                                   :: time_xc5_nrdc       ! time according to NDRC file
  integer                                                   :: date_x4_nrdc        ! date according to NDRC file
  integer                                                   :: Nentry_xc5_nrdc     ! number of entries according to NRDC file
  integer                                                   :: Nsub_xc5_nrdc       ! number of subentries according to NRDC file
  integer                                                   :: Nsub_x4_nrdc        ! number of subentries according to NRDC file
  integer                                                   :: Npointstot_xc5_nrdc ! number of total points according to NDRC file
  integer                                                   :: Nentry_xc5          ! number of entries according to NRDC file
  integer                                                   :: year_xc5            ! year
  integer                                                   :: Ndatasets           ! number of data sets
  integer                                                   :: nauthors            ! number of lines with authors
  integer                                                   :: ntitle              ! number of lines with title
  integer                                                   :: nref                ! number of lines with reference
  integer                                                   :: Nsub_xc5            ! counter for XC5 subentries
  integer                                                   :: Nsub_new            ! counter for new subentries
  integer                                                   :: date_xc5            ! date
  integer                                                   :: MF_xc5              ! MF number
  integer                                                   :: MT_xc5              ! MT number
  integer                                                   :: Z_xc5               ! charge number
  integer                                                   :: A_xc5               ! mass number
  integer                                                   :: Npoints_xc5         ! number of points per subentry
  integer                                                   :: Npoints_new         ! number of points per new data set
  integer                                                   :: Nend_new            ! end point per new data set
  integer                                                   :: Npointstot_xc5      ! total number of points
  integer                                                   :: Eindex              ! index for energy
  integer                                                   :: Eindexnew           ! index for energy
!
! reactionsub
!
  logical                                          :: flagMTequiv ! flag for equivalence of MT number and residual product
  logical                                          :: flagratio   ! flag for cross section ratio
  logical                                          :: flagconv    ! flag for conversion to new database
  logical                                          :: flagri      ! flag for resonance integral
  logical                                          :: flagspecav  ! flag for spectrum average
  logical                                          :: flagrespar  ! flag for resonance parameters
  logical                                          :: flagfy      ! flag for fission yields
  logical                                          :: flagtke     ! flag for total kinetic energy
  logical                                          :: flagnubar   ! flag for nubar
  logical                                          :: flagMT      ! flag for MT cross section
  logical                                          :: flagdisc    ! flag for discrete reaction
  logical                                          :: flagres     ! flag for residual production cross section
  logical                                          :: flagspec    ! flag for emission spectrum
  logical                                          :: flagelang   ! flag for elastic angular distribution
  logical                                          :: flagdiscang ! flag for discrete angular distribution
  logical                                          :: flagddx     ! flag for DDX emission spectrum
  character(len=9), allocatable, dimension(:)      :: valstring   ! XC5 string with values
  character(len=1)                                 :: resiso      ! isomer of residual nuclide
  character(len=1)                                 :: proj        !  projectile
  character(len=1)                                 :: frame       ! reference fram
  character(len=6) :: targetnuclide
  integer                                          :: ZAP_xc5     ! projectile
  integer                                          :: Nobs_xc5    ! number of obsolete cases
  integer                                          :: k0_xc5      ! incident particle
  integer                                          :: isom_xc5    ! number of isomer
  integer                                          :: Liso        ! number of isomer
  integer                                          :: MTrat       ! MT number for ratio
  integer                                          :: Lrat        ! isomer for ratio
  integer                                          :: ZAres       ! ZA of residual nucleus
  integer                                          :: kres        ! type of outgoing particle
  integer                                          :: Atarget
  integer                                          :: Ztarget
!
! inelastic
!
  logical                                   :: flaginel    ! flag for inelastic scattering
  logical                                   :: flaginelang ! flag for inelastic angular distribution
  real(sgl)                                 :: Eexc        ! excitation energy
  real(sgl)                                 :: Eexc2       ! excitation energy
  real(sgl)                                 :: Elev        ! energy of discrete level
  real(sgl)                                 :: Jlev        ! spin of discrete level
  integer                                   :: Plev        ! parity of discrete level
!
! readdata
!
  logical                                     :: flagxs         ! flag for cross section
  logical                                     :: flagcomp       ! flag for comparison with libraries and TALYS
  integer                                     :: Zres           ! residual Z
  integer                                     :: Ares           ! residual A
  integer, allocatable, dimension(:)          :: Zfy            ! Z of fission yield
  integer, allocatable, dimension(:)          :: Afy            ! A of fission yield
  integer, allocatable, dimension(:)          :: Ify            ! isomer of fission yield
  integer                                     :: Npointstot_new ! total number of points in new database
  integer, allocatable, dimension(:)          :: NPgroup        ! number of points in group
  real(sgl)                                   :: Emin           ! minimum energy (MeV) of subentry
  real(sgl)                                   :: Emax           ! maximum energy (MeV) of subentry
  real(sgl)                                   :: angmin         ! minimum angle of subentry
  real(sgl)                                   :: angmax         ! maximum angle of subentry
  real(sgl), allocatable, dimension(:)        :: E              ! incident energy
  real(sgl), allocatable, dimension(:)        :: dE             ! incident energy uncertainty
  real(sgl), allocatable, dimension(:)        :: xs             ! cross section
  real(sgl), allocatable, dimension(:)        :: dxs            ! cross section uncertainty
  real(sgl), allocatable, dimension(:)        :: Fnorm          ! normalization factor
  real(sgl), allocatable, dimension(:)        :: angle          ! angle
  real(sgl), allocatable, dimension(:)        :: cosang         ! cosine of angle
  real(sgl), allocatable, dimension(:)        :: xsr            ! cross section divided by Rutherford
  real(sgl), allocatable, dimension(:)        :: dxsr           ! uncertainty of cross section divided by Rutherford
!
! pathsub
!
  character(len=132)             :: path    ! path
  character(len=132)             :: xspath  ! path of cross section
!
! authorsub
!
  logical                                           :: authordone ! flag to check whether this entry is already covered
  character(len=25)                                 :: author2    ! author name
  character(len=29)                                 :: authorcode ! author
!
! filesub
!
  logical, allocatable, dimension(:,:,:,:)     :: ZAexist ! logical for Z, A existence
! logical, allocatable, dimension(:,:,:,:,:,:) :: ZAMTexist ! logical for Z, A, MT existence
  logical, allocatable, dimension(:,:,:,:)     :: ZAresexist ! logical for Z, A, rp existence
  character(len=125)                           :: xsfile ! cross section file
  real(sgl)                                    :: Einc   ! incident energy
  real(sgl)                                    :: dEinc  ! uncertainty of incident energy
!
! compare
!
  character(len=80)                                    :: Ffile      ! file with F values
  character(len=80)                                    :: compdir    ! directory with statistics
  character(len=80)                                    :: uncdir     ! directory with uncertainties
  integer, allocatable, dimension(:)                   :: Npoints    ! number of points compared per library
  integer, allocatable, dimension(:,:)                 :: comppoint  ! designator for comparison of point
  real(sgl), allocatable, dimension(:,:)               :: xsthpoint  ! theoretical cross section
  real(sgl), allocatable, dimension(:,:)               :: chi2point  ! chi-square
  real(sgl), allocatable, dimension(:,:)               :: Rpoint     ! R value
  real(sgl), allocatable, dimension(:,:)               :: Fpoint     ! F value
  real(sgl), allocatable, dimension(:,:)               :: Aspoint    ! asymmetry
  real(sgl), allocatable, dimension(:,:)               :: Flogpoint  ! log of F value
  real(sgl), allocatable, dimension(:,:)               :: Aslogpoint ! log of asymmetry
!
! readtalys
!
  integer                                              :: Ntal  ! number of TALYS points
  integer                                              :: Nnon  ! number of non-elastic energies
  real(sgl), allocatable, dimension(:)                 :: etal  ! energy of TALYS
  real(sgl), allocatable, dimension(:)                 :: xstal ! cross section of TALYS
  real(sgl), allocatable, dimension(:)                 :: Enon  ! energy for non-elastic cross section
  real(sgl), allocatable, dimension(:)                 :: xsnon ! non-elastic cross section
  real(sgl)                                            :: E1mb  ! energy at 1 mb threshold
!
! readlib
!
  logical, allocatable, dimension(:)                        :: libexist ! flag for existence of library
  integer, allocatable, dimension(:)                        :: Nlib     ! number of data libraries for comparison
  real(sgl)                                                 :: resEnergy ! energy end of RR
  real(sgl), allocatable, dimension(:,:)                    :: elib     ! energy of library
  real(sgl), allocatable, dimension(:,:)                    :: xslib    ! cross section of library
  real(sgl), allocatable, dimension(:,:)                    :: dxslib   ! cross section uncertainty of library
!
! readfile
!
  character(len=9), allocatable, dimension(:)   :: subentry_E   ! XC5 subentry number
  character(len=25), allocatable, dimension(:)  :: author1_E    ! author
  character(len=60), allocatable, dimension(:)  :: reaction_E   ! XC5 reaction
  character(len=132), allocatable, dimension(:) :: title_E      ! title of paper
  character(len=132), allocatable, dimension(:) :: authors_E    ! authors of paper
  character(len=132), allocatable, dimension(:) :: reference_E  ! reference of paper
  character(len=132), allocatable, dimension(:) :: expfile      ! experimental data file
  integer                                       :: Nset         ! number of experimental data sets
  integer, allocatable, dimension(:)            :: Npoints_E    ! number of points per subentry
  integer, allocatable, dimension(:)            :: year_E       ! year
  integer, allocatable, dimension(:,:)          :: Nexp         ! numer of experiments
  real(sgl), allocatable, dimension(:)          :: Emax_E       ! maximum energy (MeV) of subentry
  real(sgl), allocatable, dimension(:)          :: Emin_E       ! minimum energy (MeV) of subentry
  real(sgl), allocatable, dimension(:)          :: Eunc         ! average energy uncertainty of subentry
  real(sgl), allocatable, dimension(:)          :: Euncabs      ! average absolute energy uncertainty of subentry
  real(sgl), allocatable, dimension(:,:)        :: Eexp         ! incident energy
  real(sgl), allocatable, dimension(:,:)        :: dEexp        ! incident energy uncertainty
  real(sgl), allocatable, dimension(:,:)        :: xsexp        ! cross section
  real(sgl), allocatable, dimension(:,:)        :: dxsexp       ! cross section uncertainty
  real(sgl), allocatable, dimension(:)          :: xsexpav      ! average experimental cross section
  real(sgl), allocatable, dimension(:)          :: uncertainty  ! average uncertainty of subentry
!
! xsinterpol
!
  real(sgl), allocatable, dimension(:,:,:)      :: xsexplib     ! cross section
  real(sgl), allocatable, dimension(:,:,:)      :: dxsexplib    ! cross section uncertainty
  real(sgl), allocatable, dimension(:,:,:)      :: xsexpint     ! cross section
  real(sgl), allocatable, dimension(:,:,:)      :: dxsexpint    ! cross section uncertainty
  real(sgl), allocatable, dimension(:,:)        :: xsnonel      ! cross section
!
! comparesum
!
  logical, allocatable, dimension(:,:,:)                                                :: Aexist_xc5    ! flag for existence of A
  logical, allocatable, dimension(:)                                                    :: MTexist_xc5   ! flag for existence of MT
  character(len=1), allocatable, dimension(:,:,:)                                       :: processed     ! flag for processed subent
  integer, allocatable, dimension(:,:)                                                  :: NAix          ! number of A's per k0,Z
  integer, allocatable, dimension(:,:,:)                                                :: Aix           ! index for A
  integer, allocatable, dimension(:)                                                    :: MTix          ! index for MT
  integer                                                                               :: NMTix         ! number of MT sections
  integer, allocatable, dimension(:,:,:,:,:,:)                                          :: NpointsnucMT  ! number of points per nucl
  integer, allocatable, dimension(:,:,:)                                                :: NpointsparE   ! number of points per part
  integer, allocatable, dimension(:,:)                                                  :: NpointsallE   ! number of points per ener
  integer, allocatable, dimension(:,:,:,:,:)                                            :: NpointsMTE    ! number of points per MT n
  integer, allocatable, dimension(:,:,:,:,:)                                            :: NpointsnucE   ! number of points per nucl
  integer, allocatable, dimension(:)                                                    :: Npointsset    ! number of points per sube
  integer, allocatable, dimension(:)                                                    :: Npointsall    ! total number of points
  integer, allocatable, dimension(:,:)                                                  :: Npointsentry  ! number of points per entr
  integer, allocatable, dimension(:,:,:,:)                                              :: NpointsMT     ! number of points per MT n
  integer, allocatable, dimension(:,:)                                                  :: Npointspar    ! number of points per part
  integer, allocatable, dimension(:,:,:,:)                                              :: Npointsnuc    ! number of points per nucl
  integer, allocatable, dimension(:,:,:,:)                                              :: Nsetsnuc      ! number of data sets per n
  integer, allocatable, dimension(:,:)                                                  :: Nsetsentry    ! number of data sets per e
  integer, allocatable, dimension(:)                                                    :: MTsumtot      ! total F value per particl
  integer, allocatable, dimension(:)                                                    :: Nsetsall      ! total number of data sets
  integer, allocatable, dimension(:)                                                    :: NsetsMTtot    ! number of sets per MT num
  integer, allocatable, dimension(:)                                                    :: NsetsallMTtot ! all MT sets
  integer, allocatable, dimension(:,:,:)                                                :: NsetsallMT    ! all MT sets
  integer, allocatable, dimension(:,:,:,:)                                              :: NsetsMT       ! number of data sets per M
  integer, allocatable, dimension(:,:,:,:,:,:)                                          :: NsetsnucMT    ! number of data sets per n
  integer, allocatable, dimension(:,:,:,:,:)                                            :: NsetsAMT      ! all MT sets
  integer, allocatable, dimension(:,:,:,:,:)                                            :: NpointsAMT    ! total number of points pe
  integer, allocatable, dimension(:,:)                                                  :: Nsetspar      ! number of data sets per p
  integer, allocatable, dimension(:,:,:,:,:)                                            :: NsetsnucE     ! number of data sets per n
  integer, allocatable, dimension(:,:,:)                                                :: NsetsparE     ! number of data sets per p
  integer, allocatable, dimension(:,:,:,:,:)                                            :: NsetsMTE      ! number of data sets per M
  integer, allocatable, dimension(:,:)                                                  :: NsetsallE     ! total number of data sets
  real(sgl), allocatable, dimension(:,:,:,:)                                            :: FMT           ! average F value per MT nu
  real(sgl), allocatable, dimension(:,:,:,:)                                            :: FMTlog        ! average log F value per M
  real(sgl), allocatable, dimension(:,:,:,:)                                            :: Fnuc          ! average F value per nucle
  real(sgl), allocatable, dimension(:,:,:,:)                                            :: Fnuclog       ! average log F value per n
  real(sgl), allocatable, dimension(:,:,:,:,:,:)                                        :: FnucMTlog     ! average F value per nucle
  real(sgl), allocatable, dimension(:,:,:,:,:,:)                                        :: FnucMT        ! average log F value per n
  real(sgl), allocatable, dimension(:,:,:,:,:)                                          :: FAMT          ! average F for all MT numb
  real(sgl), allocatable, dimension(:,:)                                                :: Fpar          ! average F value per parti
  real(sgl), allocatable, dimension(:)                                                  :: Fall          ! average F value for all r
  real(sgl), allocatable, dimension(:,:)                                                :: Fparlog       ! average log F value per p
  real(sgl), allocatable, dimension(:)                                                  :: Falllog       ! average log F value for a
  real(sgl), allocatable, dimension(:,:,:,:,:)                                          :: FMTE          ! F per MT number and energ
  real(sgl), allocatable, dimension(:,:,:,:,:)                                          :: FMTElog       ! log of F per MT number an
  real(sgl), allocatable, dimension(:,:,:,:,:)                                          :: FnucE         ! F per nucleus and energy
  real(sgl), allocatable, dimension(:,:,:,:,:)                                          :: FnucElog      ! log of F per nucleus and
  real(sgl), allocatable, dimension(:,:,:)                                              :: FparE         ! F per particle and energy
  real(sgl), allocatable, dimension(:,:,:)                                              :: FparElog      ! log of F per particle and
  real(sgl), allocatable, dimension(:,:)                                                :: Fentrylog     ! average log F value per e
  real(sgl), allocatable, dimension(:,:)                                                :: Fentry        ! average F value per entry
  real(sgl), allocatable, dimension(:,:)                                                :: FallE         ! total F per energy bin
  real(sgl), allocatable, dimension(:,:)                                                :: FallElog      ! log of total F per energy
  real(sgl), allocatable, dimension(:,:)                                                :: pvalue        ! p-value
  real(sgl), allocatable, dimension(:)                                                :: xsthset       ! average theoretical cross
  real(sgl), allocatable, dimension(:)                                                :: Fset          ! average F value per suben
  real(sgl), allocatable, dimension(:)                                                :: pset          ! average p value per suben
  real(sgl), allocatable, dimension(:)                                                :: chi2set       ! average Chi-2 per subentr
  real(sgl), allocatable, dimension(:)                                                :: Rset          ! average R per subentry
  real(sgl), allocatable, dimension(:)                                                :: Asset         ! average As value per sube
!
! quality
!
  character(len=10)                                            :: Qdate     ! quality date
  character(len=150)                                           :: Qaction   ! recommended action
  character(len=2)                                             :: Qscore    ! quality score
  character(len=150), allocatable, dimension(:)                :: Qcomment  ! comments
  integer, allocatable, dimension(:,:,:)                       :: Qentry    ! entry with quality information
  integer                                                      :: weight  ! weight
!
! totalstat
!
  logical, allocatable, dimension(:,:,:)                                  :: MTexist        ! flag for existence of MT
  integer, allocatable, dimension(:,:,:)                                  :: MTsum          ! number of reactions per MT number
  integer, allocatable, dimension(:,:,:,:)                                :: MTbin          ! number of F values in MT bin
  integer, allocatable, dimension(:,:,:)                                  :: MTbintot       ! total number of F values in MT bin
  integer                                                                 :: Nsub_tal       ! number of subentries for TALYS compari
  integer                                                                 :: Npointstot_tal ! number of points for TALYS comparison
  integer, allocatable, dimension(:,:,:,:)                                :: MTuncbin       ! uncertainty per bin
  integer, allocatable, dimension(:,:,:)                                  :: MTuncsets      ! number of data sets with uncertainty 
  integer, allocatable, dimension(:,:,:,:,:)                              :: QMT            ! quality per MT number
  integer, allocatable, dimension(:,:,:)                                  :: QMTall         ! total quality per MT number
  real(sgl), allocatable, dimension(:)                                    :: fbin           ! boundaries for F bins
  real(sgl), allocatable, dimension(:,:,:,:)                              :: MTbinav        ! average F value per bin
  real(sgl), allocatable, dimension(:,:,:,:)                              :: MTbinsigma     ! average F deviation per bin
  real(sgl), allocatable, dimension(:,:,:,:)                              :: MTbincum       ! cumulative F value per MT number
  real(sgl), allocatable, dimension(:,:,:,:)                              :: Ffinal         ! Final F value per bin
end module A0_exfortables_mod
