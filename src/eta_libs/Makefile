###################################################################
#
#	Makefile for NWS/COMET specific Eta model related software
#		NWS 11/99 R.Rozumalski
#
####################################################################
####################################################################
#
#

.SILENT:

include $(ETA_HOME)/src/config/Makeinc.common

SUBDIRS = \
	bacio	\
	iplib	\
	w3lib	\
	mpi	\
	date

all :
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) if [ -d $$dir ]; then \
				(cd $$dir; echo "    Making $@ in `pwd`" ; \
				$(MAKE) $@) ; \
			   else \
				echo "Skipping $$dir"; \
			   fi; \
			;; \
		esac ; \
	done

All : all

clean:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) if [ -d $$dir ]; then \
				(cd $$dir; echo "    Making $@ in `pwd`" ; \
				$(MAKE) $@) ; \
			   else \
				echo "Skipping $$dir"; \
			   fi; \
			;; \
		esac ; \
	done

clobber:
	@for dir in NODIR $(SUBDIRS); do \
		case $$dir in \
			NODIR) ;; \
			*) if [ -d $$dir ]; then \
			(cd $$dir; echo "    Making $@ in `pwd`" ; \
			$(MAKE) $@) ; \
				else \
				echo "Skipping $$dir"; \
				fi; \
			;; \
		esac ; \
	done

