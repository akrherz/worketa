###################################################################
###################################################################
#
#       Makefile for NWS/COMET specific Eta model related software
#               NWS 11/99 R.Rozumalski
#
####################################################################

.SILENT:

include $(ETA_HOME)/src/config/Makeinc.common

#INC      = $(ETA_INCLUDE)
INC      = ./Include

EXE 	 = $(ETA_EXE)/eta_hy.exe

MAIN 	 = EBU.f

.f.o:
	@echo "    Building Eta Routine: $(@:.o=.f)"
	-@$(F90) -c $(FFLAGSA) $<  -I$(INC)

OBJ =	\
	BOCOH.o   BOCOV.o   CLO89.o   \
	CONRAD.o  CUCNVC.o  DDAMP.o   \
	DIFCOF.o  E1E290.o  E290.o    \
	E2SPEC.o  E3V88.o   FST88.o   \
	GFDLRD.o  GRADFS.o  GSCOND.o  \
	HCONST.o  HDIFF.o   INIT.o    \
	DSTRB.o   SGETMO.o  SLP.o     \
	LWR88.o   MIXLEN.o  EXCHM.o   \
	O3CLIM.o  O3INT.o   OZON2D.o  \
	PDNEW.o   PDTE.o    FILT25.o  \
	PRECPD.o  PRODQ2.o  RADFS.o   \
	RADTN.o   RDTEMP.o  SFCDIF.o  \
	SFLX.o    SOLARD.o  SPA88.o   \
	SURFCE.o  GOSSIP.o  IDSTRB.o  \
	SWR93.o   TABLE.o   TTBLEX.o  \
	TURBL.o   VDIFH.o   VDIFQ.o   \
	VDIFV.o   ZENITH.o  PGCOR.o   \
	ZERO2.o   ZERO3.o   CHKOUT.o  \
	LOC2GLB.o QUILT.o   MPI_FIRST.o  \
	READ_NHB.o READ_RESTRT.o LUTAB.o \
	READ_RESTRT2.o SETUP_SERVERS.o   \
	ZERO3_T.o timef_bog.o BOCOHF.o   \
	COLLECT.o MPPINIT.o PARA_RANGE.o \
	DIST.o    DIGFLT.o  ILOC2GLB.o   \
	KFDRIVE.o CONDLOAD.o DTFRZ2.o    \
	ENVIRTHT.o KFPARA.o KFTEND.o     \
	PROF5.o   TPMIX2.o               \
	TPMIX2DD.o

OBJA	=	\
		DIVHOA.o VTADV.o HZADV.o HZADV2.o \
		VTADVF.o

all	:  $(EXE)

$(OBJA)	:
	@echo "    Building Eta Routine: $(@:.o=.f)"
	-@$(F90) -c  $(FFLAGSA) $(@:.o=.f)  -I$(INC) -L$(INC)

$(OBJ)	:
	@echo "    Building Eta Routine: $(@:.o=.f)"
	-@$(F90) -c  $(FFLAGS)  $(@:.o=.f)  -I$(INC) -L$(INC)

 
$(EXE):	$(MAIN) $(LIBETAUTIL) $(LIBMPI) $(W3LIB)  $(OBJ) $(OBJA)
	@echo " "
	@echo "    Building Eta Model: $(MAIN)"
	@echo " "
	-@$(F90) $(FFLAGSA) -o $(EXE) $(MAIN) $(OBJ) $(OBJA) $(LIBMPI) $(W3LIB) -I$(INC) -L$(INC)
	-@$(RM) $(OBJ) $(OBJA) EBU.o exchm.mod

all :   $(EXE) 

clean :
	-@$(RM) $(OBJ) core $(PREP) EBU.o  *.mod

clobber:
	-@$(RM) *.o *~* core $(LIBETA) $(EXE) EBU.o *.mod
