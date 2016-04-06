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

MACHINE= -DCRAY

THIS_LIB = libmpi.a

OBJF =	\
	mpi_dummy.o		

OBJC =	\
	mpi_dummy_c.o	\
	wtime_.o	\
	wtime.o

all : $(THIS_LIB)

$(OBJF):
	$(F90) $(FFLAGS) -c $(@:.o=.f) -I$(ETA_INCLUDE)

$(OBJC):
	$(CC) $(CFLAGS) -c $(MACHINE) $(@:.o=.c)  -I$(ETA_INCLUDE)

$(THIS_LIB): $(OBJF) $(OBJC)
	-@$(RM) $(THIS_LIB)
	@echo " "
	@echo "    Creating archive `pwd`/$(THIS_LIB)"
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJF)
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJC)
	-@$(RANLIB) $(THIS_LIB)
	-@$(INSTALL) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)
	-@$(RM) *.o 
	@echo " "

clean :
	-@$(RM) $(OBJC) $(OBJF) test* core $(THIS_LIB)

clobber:
	-@$(RM) *.o *~* core
	-@$(RM) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)

