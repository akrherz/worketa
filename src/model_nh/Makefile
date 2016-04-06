###################################################################
###################################################################
#
#       Makefile for NWS/COMET specific Eta model related software
#               NWS 11/99 R.Rozumalski
#
####################################################################

.SILENT:

include $(ETA_HOME)/src/config/Makeinc.common

INC      = ./Include

EXE 	 = $(ETA_EXE)/eta_nh.exe

MAIN 	 = EBU.f


OBJ =	BOCOH.o    EXCHM.o     MPPINIT.o       			\
	BOCOHF.o   FILT25.o    O3CLIM.o        			\
	BOCOV.o    FST88.o     O3INT.o         SOLARD.o		\
	CHKOUT.o   GFDLRD.o    OZON2D.o        SPA88.o		\
	CLO89.o    GOSSIP.o    PARA_RANGE.o    SURFCE.o		\
	COLLECT.o  GRADFS.o    PDNEW.o         SWR93.o		\
	CONDLOAD.o GSCOND.o    PDTEDT.o        TABLE.o		\
	CONRAD.o   TPMIX2.o    HCONST.o         PRECPD.o	\
	TPMIX2DD.o						\
	DDAMP.o    HDIFF.o     PRODQ2.o        TTBLEX.o		\
	DIFCOF.o   PROF5.o     TURBL.o				\
	DIGFLT.o   EPS.o       MPI_FIRST.o     timef_bog.o	\
	DIST.o     IDSTRB.o    RADFS.o         VADZ.o		\
	DIVHOA.o   ILOC2GLB.o  RADTN.o         VDIFH.o		\
	DSTRB.o    INIT.o      RDTEMP.o        VDIFQ.o		\
	DTFRZ2.o   KFDRIVE.o   READ_NHB.o      VDIFV.o		\
	E1E290.o   KFPARA.o    READ_RESTRT.o   			\
	E290.o     KFTEND.o    READ_RESTRT2.o  			\
	E2SPEC.o   LOC2GLB.o   SETUP_SERVERS.o ZENITH.o		\
	E3V88.o    LUTAB.o     SFCDIF.o        ZERO2.o		\
	LWR88.o     SFLX.o          ZERO3.o		\
	ENVIRTHT.o MIXLEN.o    SGETMO.o        ZERO3_T.o	 

OBJA =	CUCNVC.o   HZADV.o     HADZ.o 	       PGCOR.o 	\
	HZADV2.o   VTADV.o     VTADVF.o

all	:  $(EXE)

$(OBJA)	: 
	@echo "    Building Eta Routine: $(@:.o=.f)"
	-@$(F90) -c  $(FFLAGSA) $(@:.o=.f)  -I$(INC)

$(OBJ) 	:
	@echo "    Building Eta Routine: $(@:.o=.f)"
	-@$(F90) -c  $(FFLAGS)  $(@:.o=.f)  -I$(INC)

 
$(EXE):	$(MAIN) $(LIBETAUTIL) $(LIBMPI) $(W3LIB) $(OBJA) $(OBJ) 
	@echo " "
	@echo "    Building Eta Model: $(MAIN)"
	@echo " "
	-@$(F90) $(FFLAGSA) -o $(EXE) $(MAIN) $(OBJ) $(OBJA) $(LIBMPI) $(W3LIB) -I$(INC) 
	-@$(RM) $(OBJ) $(OBJA) EBU.o exchm.mod

clean :
	-@$(RM) $(OBJ) $(OBJA) core $(PREP) EBU.o  *.mod $(LIBETA) 

clobber:
	-@$(RM) *.o core $(LIBETA) $(EXE) EBU.o *.mod
