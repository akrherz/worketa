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

LIBS  = $(BACIO) $(IPLIB) $(W3LIB)

EXE  = $(ETA_EXE)/copygb.exe

OBJS = copygb.o

all: 	$(OBJS)
	-@$(F90) $(SFOPT) -o $(EXE) $(OBJS) $(LIBS) 
	-@$(RM) *.o
	-@echo " "

$(OBJS):
	-@$(F90) -c $(SFOPT) $(@:.o=.f)

clean :
	-@$(RM) $(OBJS)  core *.o

clobber:
	-@$(RM) *.o core  $(EXE)

