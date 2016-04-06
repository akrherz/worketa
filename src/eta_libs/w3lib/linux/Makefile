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

THIS_LIB = w3lib.a

OBJF90 =	\
	gbyte.o		\
	gbytes.o	\
	sbyte.o		\
	sbytes.o	\
	gbytec.o	\
	gbytesc.o	\
	sbytec.o	\
	sbytesc.o	\
	datimx.o	\
	errexit.o	\
	errmsg.o	\
	fparsei.o	\
	fparser.o	\
	getbit.o	\
	getgb.o		\
	getgb1.o	\
	getgb1r.o	\
	getgb1s.o	\
	getgbm.o	\
	getgbmh.o	\
	getgi.o		\
	getgir.o	\
	gtbits.o	\
	instrument.o	\
	iw3jdn.o	\
	ixgb.o		\
	lengds.o	\
	pdseup.o	\
	putgb.o		\
	putgbn.o	\
	r63w72.o	\
	skgb.o		\
	swap32.o	\
	w3difdat.o	\
	w3doxdat.o	\
	w3fi01.o	\
	w3fi58.o	\
	w3fi59.o	\
	w3fi63.o	\
	w3fi68.o	\
	w3fi71.o	\
	w3fi72.o	\
	w3fi73.o	\
	w3fi74.o	\
	w3fi75.o	\
	w3fi76.o	\
	w3fi82.o	\
	w3fi83.o	\
	w3fs26.o	\
	w3locdat.o	\
	w3movdat.o	\
	w3reddat.o	\
	w3tagb.o	\
	w3utcdat.o	\
	xmovex.o	\
	xstore.o

all : $(THIS_LIB)

$(OBJF90):
	$(F90) -c $(@:.o=.f) 

$(THIS_LIB): $(OBJF90) 
	-@$(RM) $(THIS_LIB)
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJF90)
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJFC)
	-@$(RANLIB) $(THIS_LIB)
	-@$(INSTALL) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)
	-@$(RM) *.o *.a
	@echo " "


clean :
	-@$(RM) $(OBJF90) $(OBJFC) core $(THIS_LIB) 

clobber:
	-@$(RM) *.o core
	-@$(RM) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)

