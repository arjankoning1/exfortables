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

- git (only if the package is downloaded via GitHub)
- GNU make
- a recent Fortran compiler, such as GNU Fortran (gfortran)

A complete reconstruction additionally uses:

```text
.../exfortables/files/
.../drip/
.../libraries/
```

With the default layout, `drip/` and `libraries/` are sibling directories of `exfortables/`:

```text
.../exfortables/
.../drip/
.../libraries/
```

The current Git repository contains the produced EXFORTABLES database, but does not contain the raw `exfortables/files/` reconstruction tree. That tree must be supplied separately when the database itself is to be regenerated.

The default paths can also be overridden at run time with the existing EXFORTABLES input keywords:

```text
filespath /path/to/files/
talyspath /path/to/drip/
libspath /path/to/libraries/
```

### Downloads

#### 1. Download the entire tar file

```bash
curl -LO https://nds.iaea.org/talys/exfortables.tar
tar zxf exfortables.tar
```

#### 2. Using git

```bash
git clone https://github.com/arjankoning1/exfortables.git
```

## Compiling the reconstruction code

For the modern git version:

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

The default compiler is `gfortran`. When `gfortran` is used and no `FFLAGS` are supplied, the Makefile uses:

```text
-w -O3 -ffp-contract=off
```

For other compilers, no default compiler flags are imposed.

Compiler and compilation options can be passed through `install_exfortables.bash`, for example:

```bash
./install_exfortables.bash FC=gfortran FFLAGS="-O3 -ffp-contract=off"
./install_exfortables.bash FC=ifx FFLAGS="-O3"
```

The executable is installed as:

```text
exfortables/bin/exfortables
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

For the modern git version, `code_build.bash` and `path_change.bash` are no longer required and can be removed after adopting the new installer, Makefile and `machine.f90`.

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
