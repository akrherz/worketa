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

include $(ETA_HOME)/src/config/Makeinc.common

COBJ = initdate.exe
DOBJ = datem1.exe

CEXE = initdate
DEXE = datem1

all : $(COBJ) $(DOBJ)

$(COBJ):
	$(CC) $(CFLAGS) -o $(CEXE) $(@:.exe=.c)
	@$(INSTALL) $(CEXE)  $(ETA_EXE)
	-@$(RM) *.o $(CEXE)

$(DOBJ):
	$(CC) $(CFLAGS) -o $(DEXE) $(@:.exe=.c)
	@$(INSTALL) $(DEXE)  $(ETA_EXE)
	-@$(RM) *.o $(DEXE)
	@echo " "

clean :
	-@$(RM) $(COBJ) $(DOBJ) core

clobber:
	-@$(RM) *.o core  $(ETA_EXE)/$(CEXE) $(CEXE) $(ETA_EXE)/$(DEXE) $(DEXE)

