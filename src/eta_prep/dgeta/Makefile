###################################################################
#
#       Makefile for NWS/COMET specific Eta model related software
#               NWS 11/99 R.Rozumalski
#
####################################################################
####################################################################
#
#

.SILENT:

# Define compiler flags from machine dependent make.inc.

include $(ETA_HOME)/src/config/Makeinc.common

# Define main routine.

MAIN = dgeta2model.f

# Define executable.

EXE = $(ETA_EXE)/dgeta2model.exe

# DEFINE LIBS

LIBS = $(W3LIB) $(BACIO)

# Default target

$(EXE): $(MAIN)
	-@$(F90) $(FFLAGS) $(MAIN) $(LIBS) -o $@
	-@$(RM) $(<:.f=.o)
	-@$(RM) *.o

exe: $(EXE)

all: $(EXE)

clean:
	-@$(RM) *.o *.bif *.int *.s

clobber:
	-@$(RM) *.o $(EXE) *.bif *.int *.s

