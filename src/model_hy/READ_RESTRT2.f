C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                        SUBROUTINE READ_RESTRT2
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .

C SUBPROGRAM:    READ_RESTRT2 READ MULTIPLE SMALL RESTRT FILES
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 99-09-01
C
C ABSTRACT:
C     READ_RESTRT2 READS IN QUANTITIES FROM THE SMALL RESTRT FILES
C     WHICH WERE PREVIOUSLY WRITTEN BY INDIVIDUAL NODES
C
C PROGRAM HISTORY LOG:
C   99-09-01  BLACK      - REWRITTEN ROM READ_RESTRT
C
C USAGE: CALL READ_RESTRT2 FROM SUBROUTINE INIT
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED: NONE
C
C     LIBRARY: NONE
C
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  PHYS2
C                  MAPOT1
C                  VRBLS
C                  CONTIN
C                  PVRBLS
C                  BOCO
C                  ACMCLH
C                  ACMCLD
C                  ACMPRE
C                  ACMRDL
C                  ACMRDS
C                  ACMSFC
C                  CLDWTR
C                  CNVCLD
C                  SOIL
C                  INDX
C                  OUTFIL
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$
C-----------------------------------------------------------------------
C     INCLUDE/SET PARAMETERS.
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JMP1=JM+1,JAM=6+2*(JM-10),LB=2*IM+JM-3
     &, LM1=LM-1,LP1=LM+1,IMT=2*IM-1)
C
C-----------------------------------------------------------------------
C     
C                            DECLARE VARIABLES
C     
C-----------------------------------------------------------------------
                              L O G I C A L
     & RUN,RUNB,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
                              C H A R A C T E R
     & RESTHR*4,LABEL*32
                              C H A R A C T E R *40
     & CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV
     &,FILCLD,FILRAD,FILSFC
C-----------------------------------------------------------------------
                              R E A L
     & PSLP(IDIM1:IDIM2,JDIM1:JDIM2)
                              R E A L
     & TEMPSOIL(IM,JM,NSOIL)
C-----------------------------------------------------------------------
                              I N T E G E R
     & IDATB(3)
C-----------------------------------------------------------------------
C     
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "CTLBLK.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "DYNAM.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "MAPOT1.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "CONTIN.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "BOCO.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "INDX.comm"
      INCLUDE "OUTFIL.comm"
C
C-----------------------------------------------------------------------
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"


C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE)
C

C
C***********************************************************************
C***********************************************************************
C
C     READ INITIAL CONDITIONS FROM RESTART FILES.
C
C-------------------------------------------------------------------
      IF(MYPE.EQ.0)WRITE(LIST,*)'INIT:  READ RESTART FILES'
C***
C***  CREATE NAME FOR RESTART FILE
C***
ccccc ITAG=NTSD/TSPH+0.5
c
C hardwired
C hardwired
      ITAG=3
C hardwired
C hardwired
c
      CALL GETENV("tmmarkb",RESTHR)
C
      IF(RESTHR.EQ.'    ')THEN
        WRITE(RSTFIL,50)ITAG
   50   FORMAT('restrt',I2.2)
      ELSE
        WRITE(RSTFIL,55)ITAG,RESTHR
   55   FORMAT('restrt',I2.2,'.',a4)
      ENDIF
C***
C***  OPEN UNIT TO RESTART FILE.
C***
      LRSTRT=8
C
      CLOSE(LRSTRT)
      OPEN(UNIT=LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED',IOSTAT=IER)
      IF(IER.NE.0)WRITE(LIST,*)' LRSTRT OPEN UNIT ERROR IER=',IER
C-------------------------------------------------------------------
C

      READ(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL

      NTSD=MAX(NTSD-1,0)
      READ(LRSTRT)PDOMG,RESOMG
C
C-------------------------------------------------------------------
      DO L=1,LM
        READ(LRSTRT)((OMGALF(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
      ENDDO
C-------------------------------------------------------------------

      READ(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL,FIRST,IOUT,NSHDE

C
      NTSD=MAX(NTSD-1,0)
      FIRST=.TRUE.
C-------------------------------------------------------------------
      READ(LRSTRT)((PD(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((RES(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((FIS(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
      READ(LRSTRT)
C-------------------------------------------------------------------
C***
C***  PRIMARY 3-D VARIABLES
C***
      DO L=1,LM
        READ(LRSTRT)((T(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
        READ(LRSTRT)((Q(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
        READ(LRSTRT)((U(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
        READ(LRSTRT)((V(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
        READ(LRSTRT)((Q2(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
        READ(LRSTRT)
        READ(LRSTRT)((CWM(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
        READ(LRSTRT)((TRAIN(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
        READ(LRSTRT)((TCUCN(I,J,L),I=MYIS,MYIE),J=MYJS,MYJE)
      ENDDO
C-------------------------------------------------------------------

      READ(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
     1,         ((RSWIN(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,         ((RSWOUT(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,         ((TG(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,         ((Z0(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,         ((AKMS(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     6,         ((CZEN(I,J),I=MYIS,MYIE),J=MYJS,MYJE)

      NTSD=MAX(NTSD-1,0)
C
C-------------------------------------------------------------------
      READ(LRSTRT)((AKHS(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((THS(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((QS(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((TWBS(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((QWBS(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((HBOT(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     6,           ((CFRACL(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((THZ0(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((QZ0(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((UZ0(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((VZ0(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((USTAR(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((HTOP(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     6,           ((CFRACM(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((SNO(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((WET(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((CLDEFI(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((RF(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((PSLP(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((CUPPT(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     6,           ((CFRACH(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((SOILTB(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((SFCEXC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((SMSTAV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((SMSTOT(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((GRNFLX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((PCTSNO(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((RLWIN(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((RADOT(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((CZMEAN(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((SIGT4(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((U00(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,             UL
     2,           ((LC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((SR(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***

      READ(LRSTRT)RUN,IDAT,IHRST,NTSD,LABEL
     1,         ((PREC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,         ((ACPREC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,         ((ACCLIQ(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,         ((CUPREC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)

      NTSD=MAX(NTSD-1,0)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((ACFRCV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((NCFRCV(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((ACFRST(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((NCFRST(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((ACSNOW(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((ACSNOM(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((SSROFF(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((BGROFF(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((SFCSHX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((SFCLHX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((SUBSHX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((SNOPCX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((SFCUVX(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((SFCEVP(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     6,           ((POTEVP(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((ASWIN(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((ASWOUT(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((ASWTOA(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((ALWIN(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((ALWOUT(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((ALWTOA(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((TH10(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     1,           ((Q10(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     2,           ((U10(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     3,           ((V10(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     4,           ((TSHLTR(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     5,           ((QSHLTR(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
     6,           ((PSHLTR(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)(((SMC(I,J,N),I=MYIS,MYIE),J=MYJS,MYJE),N=1,NSOIL)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)((CMC(I,J),I=MYIS,MYIE),J=MYJS,MYJE)
C-------------------------------------------------------------------
C***
      READ(LRSTRT)(((STC(I,J,N),I=MYIS,MYIE),J=MYJS,MYJE),N=1,NSOIL)
C-------------------------------------------------------------------
C***  CALL RADIATION TO OBTAIN THE SHORT AND LONGWAVE
C***  TEMPERATURE TENDENCIES
C***
C
c     CALL RADTN
C     
C     DONE READING RESTART FILES.
C
C     END OF SUBROUTINE READ_RESTRT2
C     
      IF(MYPE.EQ.0)THEN
        WRITE(LIST,*)' '
      ENDIF
C
      RETURN
      END
