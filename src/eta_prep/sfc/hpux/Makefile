
##################################################################
#
#       Makefile for NWS/COMET specific Eta model related software
#               NWS 11/99 R.Rozumalski
#
####################################################################
####################################################################
#
#

.SILENT:

include $(ETA_HOME)/src/config/Makeinc.common

UTIL_LIB = $(LIBETAUTIL)
EXE 	 = $(ETA_EXE)/etatopo.exe
INC      = $(ETA_HOME)/src/include

MAIN 	 = etatopo.f

all : 	$(UTIL_LIB)
	-@$(F90) $(MAIN) $(FFLAGS) -o $(EXE)  $(UTIL_LIB) -I$(INC)
	-@$(RM) *.o

clean :
	-@$(RM) *.o core 

clobber:
	-@$(RM) *.o core $(EXE)

