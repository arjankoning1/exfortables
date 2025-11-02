  subroutine constants
!
!-----------------------------------------------------------------------------------------------------------------------------------
! Purpose: Constants and basic properties of particles
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
!              e2c, &     ! square of elementary charge in MeV.fm
!              mainis, &  ! main isotope
!              nuc, &     ! symbol of nucleus
!              parA, &    ! mass number of particle
!              parname, & ! name of particle
!              partype, & ! symbol of particle
!              parZ, &    ! charge number of particle
!              pi         ! pi
!
! *** Declaration of local data
!
  implicit none
!
! ****************** General properties of particles *******************
!
!          photon   = 0
!          neutron  = 1
!          proton   = 2
!          deuteron = 3
!          triton   = 4
!          helium-3 = 5
!          alpha    = 6
!          heavy ion= 7
!
  partype = (/ 'g', 'n', 'p', 'd', 't', 'h', 'a', 'i'/)
  parname = (/'gamma   ', 'neutron ', 'proton  ', 'deuteron', 'triton  ', 'helium-3', 'alpha   ', &
    'heavyion'/)
  parZ = (/ 0, 0, 1, 1, 1, 2, 2, 3 /)
  parA = (/ 0, 1, 1, 2, 3, 3, 4, 6 /)
  nuc =  (/ 'n ', 'H ', 'He', 'Li', 'Be', 'B ', 'C ', 'N ', 'O ', 'F ', 'Ne', &
    'Na', 'Mg', 'Al', 'Si', 'P ', 'S ', 'Cl', 'Ar', 'K ', 'Ca', 'Sc', 'Ti', 'V ', 'Cr', 'Mn', 'Fe', 'Co', 'Ni', 'Cu', 'Zn', &
    'Ga', 'Ge', 'As', 'Se', 'Br', 'Kr', 'Rb', 'Sr', 'Y ', 'Zr', 'Nb', 'Mo', 'Tc', 'Ru', 'Rh', 'Pd', 'Ag', 'Cd', 'In', 'Sn', &
    'Sb', 'Te', 'I ', 'Xe', 'Cs', 'Ba', 'La', 'Ce', 'Pr', 'Nd', 'Pm', 'Sm', 'Eu', 'Gd', 'Tb', 'Dy', 'Ho', 'Er', 'Tm', 'Yb', &
    'Lu', 'Hf', 'Ta', 'W ', 'Re', 'Os', 'Ir', 'Pt', 'Au', 'Hg', 'Tl', 'Pb', 'Bi', 'Po', 'At', 'Rn', 'Fr', 'Ra', 'Ac', 'Th', &
    'Pa', 'U ', 'Np', 'Pu', 'Am', 'Cm', 'Bk', 'Cf', 'Es', 'Fm', 'Md', 'No', 'Lr', 'Rf', 'Db', 'Sg', 'Bh', 'Hs', 'Mt', 'Ds'/)
  mainis = (/ 1, 1,  4,  7,  9, 11, 12, 14, 16, 19, 20, &
     23, 24, 27, 28, 31, 32, 35, 40, 39, 40, 45, 48, 51, 52, 55, 56, 59, 58, 63, 64, &
     69, 74, 75, 80, 81, 84, 85, 88, 89, 90, 93, 98, 99, 102, 103, 108, 107, 114, 115, 120, &
    121, 130, 127, 132, 133, 138, 139, 140, 141, 142, 145, 152, 153, 158, 159, 164, 165, 166, 169, 174, &
    175, 180, 181, 184, 187, 192, 193, 195, 197, 202, 205, 208, 209, 209, 210, 222, 223, 226, 227, 232, &
    231, 238, 238, 242, 242, 247, 247, 250, 254, 257, 258, 260, 262, 264, 266, 268, 270, 272, 274, 273 /)
  pi = 3.14159265358979323
  e2c = 1.4399645   
end subroutine constants
! Copyright A.J. Koning 2019
