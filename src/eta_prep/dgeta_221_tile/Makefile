
.SILENT:

# Define compiler flags from machine dependent make.inc.

include $(ETA_HOME)/src/config/Makeinc.common

# Define main routine.

MAIN = dgeta2model.f

LIBS = $(ETA_LIB)/w3lib.a $(ETA_LIB)/bacio.a 

# Define executable.

EXE = $(ETA_EXE)/dgeta2model_221_tile.exe

# Default target

$(EXE): $(MAIN)
	-@$(F90) $(FFLAGS) $(MAIN) -o $@ $(LIBS)
	-@$(RM) $(<:.f=.o)
	-@$(RM) *.o

exe: $(EXE)

all: $(EXE)

clean:
	-@$(RM) *.o *.bif  *.int  *.s

clobber:
	-@$(RM) *.o $(EXE)
