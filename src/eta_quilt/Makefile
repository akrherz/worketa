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

LIBS     = $(ETA_LIB)/libmpi.a
EXE 	 = $(ETA_EXE)/quilt.exe
INC      = ./Include

MAIN 	 = QUILT.f

OBJ =	\
	COLLECT.o       \
	DIST.o		\
	MPI_FIRST.o	\
	MPI_LAST.o	\
	PARA_RANGE.o	\
	SLP.o		\
	SLPSIG.o	\
	SLPSIGSPLINE.o	\
	UPDATE.o	\
	timef_bog.o	 


all : 	$(LIBS) $(OBJ)
	-@$(F90) $(MAIN) $(FFLAGS) -o $(EXE) $(OBJ)  $(LIBS) -I$(INC)
	@echo " "
	-@$(RM) *.o

$(OBJ):
	-@$(F90) $(FFLAGS) -c $(@:.o=.f) -I$(INC)

clean :
	-@$(RM) *.o core 

clobber:
	-@$(RM) core $(EXE) *.o

