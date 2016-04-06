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

include $(ETA_HOME)/src/config/Makeinc.common

SUBDIRS = 		\
	eta_libs	\
	eta_prep	\
	model_hy	\
 	model_nh        \
	eta_quilt	\
	eta_post	

RUNDIR  = 		\
	eta_prep	\
	model_hy        \
	model_nh        \
	eta_quilt       \
	eta_post

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

MODEL: model

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
