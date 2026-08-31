# EXFORTABLES Makefile
#
# Examples:
#
#   make
#   make FC=gfortran FFLAGS="-w -O3 -ffp-contract=off"
#   make FC=ifx FFLAGS="-O3"
#   make clean
#   make check

# Compiler-specific flags can be supplied by the user.
ifeq ($(origin FC), default)
  FC = gfortran
endif

ifeq ($(origin FFLAGS), undefined)
  ifneq ($(findstring gfortran,$(FC)),)
    FFLAGS = -w -O3 -ffp-contract=off
  else
    FFLAGS =
  endif
endif

# Installation directories.

EXFORTABLES_DIR := $(abspath ..)
BIN_DIR         := $(EXFORTABLES_DIR)/bin
TARGET          := $(BIN_DIR)/exfortables

# EXFORTABLES source files.

SOURCES := $(sort $(wildcard *.f) $(wildcard *.f90))

.PHONY: all install clean check

all install: $(TARGET)

$(TARGET): $(SOURCES)
	@echo "Compiler: $(FC)"
	@echo "Flags:    $(FFLAGS)"
	@mkdir -p "$(BIN_DIR)"
	$(FC) $(FFLAGS) -o "$(TARGET)" $(SOURCES)
	@rm -f *.o *.mod
	@echo "Installed: $(TARGET)"

clean:
	rm -f *.o *.mod exfortables "$(TARGET)"

# A full run is intentionally not part of check: reconstruction requires
# additional raw input data and may regenerate a large database.

check: all
	@test -x "$(TARGET)" || { \
	  echo "EXFORTABLES check error: executable not found:" >&2; \
	  echo "  $(TARGET)" >&2; \
	  exit 1; \
	}
	@echo "EXFORTABLES build check completed successfully"
