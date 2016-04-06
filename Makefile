###################################################################
###################################################################
#
#	Makefile for NWS/COMET specific Eta model related software
#		NWS 11/99 R.Rozumalski
#
####################################################################
#
#

.SILENT:

include $(WS_ETA)/src/config/Makeinc.common

SUBDIRS = 		\
	src	

RUNDIR  = 		\
	src

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
	echo "You are Done Building the Workstation ETA"

All : all

model:
	@for dir in NODIR $(RUNDIR); do \
		case $$dir in \
			NODIR) ;; \
			*) if [ -d $$dir ]; then \
				(cd $$dir; echo "    Making $@ in `pwd`" ; \
				$(MAKE) all) ; \
				else \
				echo "Skipping $$dir"; \
			fi; \
			;; \
		esac ; \
	done
	echo "You are Done Building the Workstation ETA"

MODEL: model

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
				(cd $$dir; echo "Making $@ in `pwd`" ; \
				$(MAKE) $@) ; \
			else \
				echo "Skipping $$dir"; \
			fi; \
			;; \
		esac ; \
	done
