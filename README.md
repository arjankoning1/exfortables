# EXFORTABLES
EXFORTABLES is a database with experimental nuclear reaction data mined from EXFOR.

## Documentation and reference
A description of the code and its options can be found in the [EXFORTABLES Tutorial (pdf)](https://github.com/arjankoning1/exfortables/blob/main/doc/exfortables.pdf).
The reference to be used for EXFORTABLES is

A.J. Koning, D. Rochman, J.-Ch. Sublet, N. Dzysiuk, M. Fleming, and S. van der Marck, *TENDL: Complete Nuclear Data Library for innovative Nuclear Science and Technology*, Nuclear Data Sheets 155,1 (2019).

### Prerequisites:

The following are the prerequisites for using EXFORTABLES:
  - git (only if the package is downloaded via Github)

### Downloads:

To download EXFORTABLES, you can use one of the following options:
#### 1. Download the entire tar file:
```
https://nds.iaea.org/talys/exfortables.tar
tar zxf exfortables.tar
```

#### 2. Using git:
```
git clone https://github.com/arjankoning1/exfortables.git
```

## The EXFORTABLES database

The *exfortables/* directory contains the following directories and files:

+ `README.md` is this README file
+ `LICENSE` is the License file
+ `doc/` contains the tutorial in pdf format
+ `source/` contains the source code. You don't need this unless you want to reproduce the database.
+ `special/` contains a set of special output files for all thermal cross sections, MACS etc.
+ `n/ p/ d/ t/ h/ a/ g/ i/ FY/` contains the directory structured experimental reaction database
+ `stat/` contains 'comp/' a a statistical comparison with nuclear data libraries and again `n/ p/ d/ t/ h/ a/ g/ i/ FY/` but this time including
a comparison with nuclear data libraries per experimental data set.

## License and Copyright
This software is distributed and copyrighted according to the [LICENSE](LICENSE) file.
