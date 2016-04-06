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

UTIL_LIB = $(LIBETAUTIL)
THIS_LIB = $(LIBINITBC)
LIBS     = $(W3LIB) $(BACIO)
EXE 	 = $(ETA_EXE)/initbc.exe
INC      = $(ETA_INCLUDE)

MAIN 	 = initbc.f

OBJ =	\
	coastal.o	\
	boco.o	        \
	dutil.o         \
	const.o         \
        interp.o        \
        pusi.o          \
	GRIBSTHR.o	\
        GRIBSTLR.o      \
	VFRAC.o		\
	SSTHIRES.o	\
        SSTLORES.o      \
	SST14K.o	\
	C2K.o		\
	ZTERPG.o	\
	PRINTAF.o	\
	SNOHIRES.o	\
	SNO16GET.o	\
	SNO8GET.o	\
	PRINTIMS.o 	\
	PUTVEG.o	\
	PUTEM.o		\
	GAULAT.o	\
	GAUTOETA.o	\
	GAUTOICE.o	\
	READSFC.o	\
	PRINTETA.o      \
	BSSLZ1.o


all : 	$(THIS_LIB) $(LIBS) $(UTIL_LIB) $(OBJ)
	$(F90) $(MAIN) $(FFLAGS) -o $(EXE)  $(THIS_LIB) $(UTIL_LIB)  $(LIBS) -I$(INC)
	-@$(RM) *.o

$(OBJ):
	$(F90) $(FFLAGS) -c $(@:.o=.f) -I$(INC)

$(THIS_LIB): $(OBJ)
	-@$(RM) $(THIS_LIB)
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJ)
	-@$(RANLIB) $(THIS_LIB)
	-@$(RM) *.o


clean :
	-@$(RM) $(OBJ) core  *.o

clobber:
	-@$(RM) *.o core $(THIS_LIB) $(EXE)
