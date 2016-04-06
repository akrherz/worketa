# Makefile for MPI dummy library. 

.SILENT:

include $(ETA_HOME)/src/config/Makeinc.common

MACHINE = -DIBM

THIS_LIB = libmpi.a

OBJF =  \
	mpi_dummy.o

OBJC =  \
        mpi_dummy.o   \
        wtime.o


all:	$(THIS_LIB)

$(OBJF):
	$(F90) $(FFLAGS) -c $(@:.o=.f) -I$(ETA_INCLUDE)

$(OBJC):
	$(CC) $(CFLAGS) -c $(ANSI) $(MACHINE) $(@:.o=.c)  -I$(ETA_INCLUDE)

$(THIS_LIB): $(OBJF) $(OBJC)
	-@$(RM) $(THIS_LIB)
	@echo " "
	@echo "    Creating archive `pwd`/$(THIS_LIB)"
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJF)
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJC)
	-@$(RANLIB) $(THIS_LIB)
	-@$(INSTALL) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)
	-@$(RM) *.o $(THIS_LIB)
	@echo " "

clean :
	-@$(RM) $(OBJC) $(OBJF) core $(THIS_LIB)

clobber:
	-@$(RM) *.o *~* core
	-@$(RM) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)

