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

THIS_LIB = iplib.a

OBJ =	\
	gausslat.o		\
	gcdist.o		\
	gdsawt.o 		\
	gdswiz.o		\
	gdswiz00.o		\
	gdswiz01.o		\
	gdswiz03.o		\
	gdswiz04.o		\
	gdswiz05.o		\
	gdswizc9.o		\
	gdswizca.o		\
	gdswizcb.o		\
	gdswzd.o		\
	gdswzd00.o		\
	gdswzd01.o		\
	gdswzd03.o		\
	gdswzd04.o		\
	gdswzd05.o		\
	gdswzdc9.o		\
	gdswzdca.o		\
	gdswzdcb.o		\
	ijkgds.o		\
	ipolates.o		\
	ipolatev.o		\
	ipxetas.o		\
	ipxwafs.o		\
	ipxwafs2.o		\
	makgds.o		\
	movect.o		\
	polateg0.o		\
	polateg1.o		\
	polates0.o		\
	polates1.o		\
	polates2.o		\
	polates3.o		\
	polates5.o		\
	polates6.o		\
	polatev0.o		\
	polatev1.o		\
	polatev2.o		\
	polatev3.o		\
	polatev6.o		\
	polfixs.o		\
	polfixv.o		


all : $(THIS_LIB)

$(OBJ):
	$(F90) $(SFOPT) -c $(@:.o=.f) 

$(THIS_LIB): $(OBJ)
	-@$(RM) $(THIS_LIB)
	@echo " "
	@echo "    Creating archive `pwd`/$(THIS_LIB)"
	-@$(AR) $(ARFLAGS) $(THIS_LIB) $(OBJ)
	-@$(RANLIB) $(THIS_LIB)
	-@$(INSTALL) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)
	-@$(RM) *.o *.a
	@echo " "

clean :
	-@$(RM) $(OBJ) core $(THIS_LIB)

clobber:
	-@$(RM) *.o core
	-@$(RM) $(THIS_LIB) $(ETA_LIB)/$(THIS_LIB)

