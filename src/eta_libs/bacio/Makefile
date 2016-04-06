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

THIS_LIB = bacio.a

COBJ = bacio.v1.3.o
FOBJ = baciof.o

all : $(THIS_LIB)

$(FOBJ):
	$(F90) -c $(FFLAGS)  $(@:.o=.f) 

$(COBJ):
	$(CC) $(CFLAGS) $(COMP) -c $(@:.o=.c)

$(THIS_LIB): $(COBJ)  $(FOBJ)
	-@$(RM) $(THIS_LIB)
	-@echo " "
	-@echo "    Creating archive `pwd`/$(THIS_LIB)" 
	-@echo " "
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(COBJ)
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(FOBJ)
	-@$(RANLIB) $(THIS_LIB)
	-@$(INSTALL) $(THIS_LIB) $(ETA_LIB)
	-@$(RM) *.o *.a *.mod

clean :
	-@$(RM) $(COBJ) $(FOBJ)  core $(THIS_LIB) BACIO_MODULE.mod bacio_module.mod

clobber:
	-@$(RM) *.o core BACIO_MODULE.mod bacio_module.mod
	-@$(RM) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)

