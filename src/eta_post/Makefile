###################################################################
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

INC     = ./Include

LIBS    = $(BACIO) $(W3LIB) $(ETA_LIB)/libmpi.a
EXE     = $(ETA_EXE)/etapost.exe

#
#
#   Set source and object files.
#
OBJ =	BLOSFC2.o    BNDLYR2.o   BOUND.o    BOUNDL.o    CALCAPE.o 	\
        CALDWP2.o    CALEKM.o    CALEKM2.o  CALHEL.o    CALLCL.o  	\
        CALMCVG.o    CALMXW.o    CALPOT2.o  CALPW.o     CALRCH.o  	\
        CALRH2.o     CALSTRM.o   CALTAU.o   CALTHTE.o   CALVIS.o  	\
        CALVOR.o     CALWXT.o    CETLIH4.o  CHR2INT.o   CLDRAD.o  	\
        CLMAX.o      COEFI.o     COEFJ.o    COLLECT.o   DEWPOINT.o  	\
        DIST.o       E2OUT.o     E2TLL.o    EFILL.o     EFILT.o     	\
        ETAFLD2.o    ETAPOST.o   EUVGUV.o   EXCH.o      EXCH2.o     	\
        FDLVL.o      FILLH.o     FILLV.o    FILTER.o    FIXED.o   	\
        FRZLVL2.o    GD2EG.o     GD2EGK.o   GD2TLL.o    GENBIL.o  	\
        GETGDEF.o    CALDRG.o    DIR.o      EXTEND.o    EXTRACT.o 	\
        GENEUV.o     GENLL.o     GET_BITS.o GRIBIT.o    H2V.o     	\
        INITPOST.o   INTERP3.o   LFMFLD.o   LL2EG.o     MINMAX.o  	\
        MISCLN.o     MIXLEN.o    MPI_FIRST.o MPI_LAST.o NETAL.o  	\
        NOPACK.o     OTLIFT2.o   OTLFT2.o   OUTPUT.o    PARA_RANGE.o 	\
        P2FLTV.o     POSTDATA.o  PROCESS.o  RDRST2D.o   SIG2P.o  	\
        READCNTRL2.o SCLFLD.o    STAT.o     SURFCE2.o   TRNSEG.o     	\
        TRPAUS.o     TTBLEX.o    WDIR.o     WETBULB.o   P2FILTF.o  	\
        timef_bog.o  FRZLVL.o    NGMFLD2.o  NGMSLP2.o   SIG2PSPLINE.o	\
	P2FILT.o     TRNSGD.o    P2FLTVF.o  ETA2P.o

all:	$(EXE)

$(EXE):	$(OBJ)
	@echo "    Building Executable: $(EXE)"
	-@$(F90) $(FFLAGS) -o $(EXE) $(OBJ) $(LIBS)
#	-@$(RM) $(OBJ) 

$(OBJ):
	@echo "    Building Object file from: $(@:.o=.f)"
	-@$(F90) -c $(FFLAGS) $(@:.o=.f) -I$(INC)

#
#   Option to remove object files (*.o) after make.
#
clean :
	-@$(RM) core *.o *.a

clobber:
	-@$(RM) core $(EXE) *.o *.a

