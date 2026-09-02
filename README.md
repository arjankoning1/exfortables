# EXFORTABLES

EXFORTABLES is a database with experimental nuclear reaction data mined from EXFOR. The database supplied in this repository can be used directly; compiling and running the source code is only necessary when the database needs to be reconstructed or regenerated.

## Documentation and reference

A description of the code and its options can be found in the [EXFORTABLES Tutorial (pdf)](https://github.com/arjankoning1/exfortables/blob/main/doc/exfortables.pdf).

The reference to be used for EXFORTABLES is:

A.J. Koning, D. Rochman, J.-Ch. Sublet, N. Dzysiuk, M. Fleming, and S. van der Marck, *TENDL: Complete Nuclear Data Library for innovative Nuclear Science and Technology*, Nuclear Data Sheets 155, 1 (2019).

## Installation

### Prerequisites

No compilation is required if you only want to use the distributed EXFORTABLES database.

For compiling the EXFORTABLES reconstruction code, the prerequisites are:

- GNU make
- a recent Fortran compiler, such as GNU Fortran (gfortran)
- git, only when EXFORTABLES is downloaded using `git clone`

A complete reconstruction additionally uses:

```text
parent_directory/
├── exfortables/files/
├── drip/
└── libraries/
```

With the default layout, `drip/` and `libraries/` are sibling directories of `exfortables/`:

```text
parent_directory/
├── exfortables/
├── drip/
└── libraries/
```

The current Git repository contains the produced EXFORTABLES database, but does not contain the raw `exfortables/files/` reconstruction tree. That tree must be supplied separately when the database itself is to be regenerated.

The default paths can also be overridden at run time with the existing EXFORTABLES input keywords:

```text
filespath /path/to/files/
talyspath /path/to/drip/
libspath /path/to/libraries/
```

### Downloads

EXFORTABLES can be downloaded in one of the following ways.

#### 1. Frozen version (December 2025)

The frozen EXFORTABLES distribution is available from the [TALYS page](https://nds.iaea.org/talys/). It can be retrieved by clicking on the download link or with

```bash
curl -LO https://nds.iaea.org/talys/codes/exfortables.tar
tar zxf exfortables.tar
```

This version is fixed and will not change.

#### 2. Latest beta version without git

Users who do not have git can download a snapshot of the current `main` branch directly from GitHub:

```bash
curl -L \
  -o exfortables-main.tar.gz \
  https://github.com/arjankoning1/exfortables/archive/refs/heads/main.tar.gz

tar zxf exfortables-main.tar.gz
mv exfortables-main exfortables
```

This produces the same `exfortables/` directory structure as the git version, but without the git history.

The downloaded snapshot contains the latest version of the `main` branch at the time of download. To obtain a newer version later, download the snapshot again.

#### 3. Latest beta version using git

Users with git can clone the repository with

```bash
git clone https://github.com/arjankoning1/exfortables.git
```

The advantage of this method is that the local EXFORTABLES installation can subsequently be updated with

```bash
cd exfortables
git pull --ff-only
```

## Compiling the reconstruction code

For the latest beta version, whether obtained as a GitHub tar snapshot or using `git clone`:

```bash
cd exfortables
./install_exfortables.bash
```

which automatically executes the `Makefile` in `exfortables/source`.

An alternative is:

```bash
cd exfortables/source
make
```

The executable is installed as:

```text
exfortables/bin/exfortables
```

The default compiler is `gfortran`. When `gfortran` is used and no `FFLAGS` are supplied, the Makefile uses:

```text
-w -O3 -ffp-contract=off
```

For other compilers, no default compiler flags are imposed.

Compiler and compilation options can be passed through `install_exfortables.bash`, for example:

```bash
# GNU Fortran
./install_exfortables.bash FC=gfortran FFLAGS="-O3 -ffp-contract=off"

# Intel Fortran
./install_exfortables.bash FC=ifx FFLAGS="-O3"
```

Set `EXFORTABLES_DIR` to the EXFORTABLES installation directory. For example:

```bash
export EXFORTABLES_DIR="/Users/koning/exfortables"
```

If you want to run the reconstruction code from anywhere, add its `bin` directory to `PATH`:

```bash
export PATH="$EXFORTABLES_DIR/bin:$PATH"
```

To include your name in generated output files, set:

```bash
export EXFORTABLES_USER="Your Name"
```

These lines can be added to `~/.zshrc` or `~/.profile`.

If setting `EXFORTABLES_DIR` is not possible, edit `code_dir` in `source/machine.f90` and rebuild EXFORTABLES.

The existing `user` input keyword can override `EXFORTABLES_USER` for an individual run.

## Build check

Because a complete EXFORTABLES reconstruction requires the raw `files/` tree and may regenerate a large database, `make check` intentionally performs a build/executable check only:

```bash
make -C source check
```

It verifies that the executable was successfully produced but does not run a database reconstruction.

## The EXFORTABLES database

The `exfortables/` directory contains the distributed database and supporting material, including:

- `README.md` this README file
- `LICENSE` the license file
- `doc/` the tutorial
- `source/` the reconstruction source code and Makefile
- `special/` special tables such as thermal cross sections and MACS
- `n/`, `p/`, `d/`, `t/`, `h/`, `a/`, `g/`, `i/`, `FY/` the projectile-structured experimental reaction database
- `stat/` statistical comparisons with nuclear data libraries

The `special/` directory is also used as a simple consistency check that `EXFORTABLES_DIR` points to the distributed database.

## License and Copyright

This software is distributed and copyrighted according to the [LICENSE](LICENSE) file.
