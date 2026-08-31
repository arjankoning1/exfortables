#!/usr/bin/env bash

set -euo pipefail

# Determine the EXFORTABLES installation directory independently of where
# the script is called from.

exfortables_dir=$(cd "$(dirname "$0")" && pwd)
source_dir="$exfortables_dir/source"

# Verify that the expected source directory and Makefile exist.

if [[ ! -d "$source_dir" ]]; then
  echo "EXFORTABLES installation error: source directory not found:" >&2
  echo "  $source_dir" >&2
  exit 1
fi

if [[ ! -f "$source_dir/Makefile" ]]; then
  echo "EXFORTABLES installation error: Makefile not found:" >&2
  echo "  $source_dir/Makefile" >&2
  exit 1
fi

# Use a stable file from the distributed database to verify that this is
# the complete EXFORTABLES repository rather than only a source directory.

database_file="$exfortables_dir/special/exfor_30keV.txt"

if [[ ! -f "$database_file" ]]; then
  echo "EXFORTABLES installation error: database missing or incomplete:" >&2
  echo "  $database_file" >&2
  exit 1
fi

echo
echo "Installing EXFORTABLES reconstruction code"
echo "Installation directory: $exfortables_dir"
echo

# Pass all command-line arguments directly to make. This permits, e.g.:
#
# ./install_exfortables.bash FC=ifx FFLAGS="-O3"
# ./install_exfortables.bash FC=gfortran FFLAGS="-w -O3 -ffp-contract=off"

make -C "$source_dir" clean
make -C "$source_dir" all "$@"

exfortables_exe="$exfortables_dir/bin/exfortables"

if [[ ! -x "$exfortables_exe" ]]; then
  echo "EXFORTABLES installation error: executable not created:" >&2
  echo "  $exfortables_exe" >&2
  exit 1
fi

echo
echo "EXFORTABLES executable:"
echo "  $exfortables_exe"
echo
echo "If not already done, add the following lines to your shell configuration:"
echo
echo "  export EXFORTABLES_DIR=\"$exfortables_dir\""
echo "  export PATH=\"\$EXFORTABLES_DIR/bin:\$PATH\""
echo "  export EXFORTABLES_USER=\"Your Name\""
echo
echo "A complete database reconstruction additionally requires the raw"
echo "EXFORTABLES files/ tree and, for the default layout, sibling"
echo "drip/ and libraries/ directories."
echo
echo "Alternatively, edit code_dir in source/machine.f90 and rebuild EXFORTABLES."
echo
