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

THIS_LIB = $(LIBETAUTIL)

OBJ =	\
	conv_index.o	\
	postcommons.o   \
	ecommons.o      \
        pqk.o           \
	rtll.o		

all : $(THIS_LIB)

$(OBJ): 
	-@$(F90) -c $(FFLAGS) $(@:.o=.f) -I$(ETA_INCLUDE)

$(THIS_LIB): $(OBJ)
	-@$(RM) $(THIS_LIB)
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJ)
	-@$(RANLIB) $(THIS_LIB)
	-@$(RM) *.o

clean :
	-@$(RM) $(OBJ) core 

clobber:
	-@$(RM) *.o core $(THIS_LIB)
