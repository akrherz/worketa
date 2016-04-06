      SUBROUTINE SURFCE(APE,ZINT,CKLQ)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    SURFCE      CALCULATE SURFACE CONDITIONS
C   PRGRMMR: JANJIC  ORG: W/NP22     DATE: 95-03-23
C
C ABSTRACT:
C   THIS ROUTINE IS THE DRIVER FOR COMPUTATION OF GROUND
C   CONDITIONS.  FOR GCIP, ACCUMULATOR AND OTHER
C   INSTANTANEOUS HOLDING ARRAYS ARE INCLUDED.
C
C PROGRAM HISTORY LOG:
C   95-03-23  JANJIC - ORIGINATOR
C   95-03-28  BLACK  - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-29  BLACK  - REMOVED SCRCH COMMON
C
C USAGE:    CALL SURFCE FROM SUBROUTINE TURBL
C   INPUT ARGUMENT LIST:
C     APE  - EXNER FUNCTION
C     ZINT - INTERFACE HEIGHTS
C     CKLQ - MASK VALUE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED:
C     SFLX
C
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  LOOPS
C                  MASKS
C                  PHYS
C                  VRBLS
C                  PVRBLS
C                  SOIL
C                  ACMSFC
C                  ACMPRE
C                  ACMRDS
C                  ACMRDL
C                  OPTIONS
C
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN 90
C     MACHINE : IBM SP
C$$$
C
C     SET LOCAL PARAMETERS.
C-----------------------------------------------------------------------
                          P A R A M E T E R
     & (EPSWET=.001
     &, PQ0=379.90516,SEAFC=.98,TRESH=.95
     &, A2=17.2693882,A3=273.16,A4=35.86
     &, T0=273.16,T1=274.16,CAPA=0.28589641
     &, CP=1004.6,STBOL=5.67E-8,R=287.04,ROW=1.E3
     &, ELWV=2.50E6,ELIV=2.834E6,ELIW=.334E6)
C
                          P A R A M E T E R
     & (A23M4=A2*(A3-A4),PQ0SEA=PQ0*SEAFC,PQ0C=PQ0*TRESH
     &, RLIVWV=ELIV/ELWV,ROWLIW=ROW*ELIW,ROWLIV=ROW*ELIV)
C-----------------------------------------------------------------------
C***  INCLUDE GLOBAL PARAMETERS.
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
C***  SET LOCAL PARAMETERS DEPENDENT ON GLOBAL PARAMETERS.
C-----------------------------------------------------------------------
                          P A R A M E T E R
     & (LP1=LM+1,JAM=6+2*(JM-10))
C-----------------------------------------------------------------------
                          L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
                          D I M E N S I O N
     & ZLM   (idim1:idim2,jdim1:jdim2)
     &,PS    (idim1:idim2,jdim1:jdim2),APES  (idim1:idim2,jdim1:jdim2)
     &,ETALM (idim1:idim2,jdim1:jdim2),PLM   (idim1:idim2,jdim1:jdim2)
     &,APELM (idim1:idim2,jdim1:jdim2),RDSIN (idim1:idim2,jdim1:jdim2)
     &,TLM   (idim1:idim2,jdim1:jdim2),THLM  (idim1:idim2,jdim1:jdim2)
     &,QLM   (idim1:idim2,jdim1:jdim2),QLMS  (idim1:idim2,jdim1:jdim2)
     &,DQSDT (idim1:idim2,jdim1:jdim2)
     &,CKLQ  (idim1:idim2,jdim1:jdim2)
     &,FFS   (idim1:idim2,jdim1:jdim2),QFC1  (idim1:idim2,jdim1:jdim2)
     &,APE   (idim1:idim2,jdim1:jdim2,LM)
     &,ZINT  (idim1:idim2,jdim1:jdim2,LP1)
C-----------------------------------------------------------------------
                          D I M E N S I O N
     & SMCK  (NSOIL),STCK  (NSOIL)
C-----------------------------------------------------------------------
C***  INCLUDE COMMON BLOCKS.
C***     COMMON BLOCKS SOIL, ACMPRE, ACMSFC WERE ADDED FOR GCIP.
C***     COMMON BLOCK OPTIONS WAS ADDED FOR THE POST.
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PHYS.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "SOIL.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMPRE.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMSFC.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMRDS.comm"
C-----------------------------------------------------------------------
      INCLUDE "ACMRDL.comm"
C-----------------------------------------------------------------------
      INCLUDE "OPTIONS.comm"
C-----------------------------------------------------------------------
                          C H A R A C T E R
     &  WORD*80
C***********************************************************************
C                         START SURFCE HERE
C
C***  INITIALIZE SOME WORKING ARRAYS
C
      CALL ZERO2(QLM)
      CALL ZERO2(QLM)
      CALL ZERO2(QLMS)
C***
C***  SET CONSTANTS CALCULATED HERE FOR CLARITY.
C***
      FDTLIW=DTQ2/ROWLIW
      FDTLIV=DTQ2/ROWLIV
      FDTW=DTQ2/2.5E9
C***
C***  SET OSU MODEL CONSTANTS AND TIME INDEPENDENT VARIABLES
C***  INITIALIZE OSU MODEL HISTORICAL VARIABLES
C***
C-----------------------------------------------------------------------
      IF(NTSD.LT.NPHS)THEN
!$omp parallel do private(i,j)
        DO 50 J=MYJS,MYJE
        DO 50 I=MYIS,MYIE
        PS(I,J)=PD(I,J)+PT
        APES(I,J)=(1.E5/PS(I,J))**CAPA
        PCTSNO(I,J)=-999.0

C
C*** SET ZERO-VALUE FOR SOME OUTPUT DIAGNOSTIC ARRAYS
        IF(SM(I,J).LT.0.5)THEN
          IF(SICE(I,J).GT.0.5) THEN
C***        SEA-ICE CASE
            SMSTAV(I,J)=1.0
            SMSTOT(I,J)=1.0
            SSROFF(I,J)=0.0
            BGROFF(I,J)=0.0
            CMC(I,J)=0.0
            DO NS=1,NSOIL
              SMC(I,J,NS)=1.0
            ENDDO
          ENDIF
        ELSE
C***       Water Case
          SMSTAV(I,J)=1.0
          SMSTOT(I,J)=1.0
          SSROFF(I,J)=0.0
          BGROFF(I,J)=0.0
          SOILTB(I,J)=273.16
          GRNFLX(I,J)=0.
          SUBSHX(I,J)=0.0
          ACSNOW(I,J)=0.0
          ACSNOM(I,J)=0.0
          SNOPCX(I,J)=0.0
          CMC(I,J)=0.0
          SNO(I,J)=0.0
          DO NS=1,NSOIL
            SMC(I,J,NS)=1.0
            STC(I,J,NS)=273.16
          ENDDO
        ENDIF
C
   50   CONTINUE
      ENDIF
C-----------------------------------------------------------------------
C***
C***  SET LOWEST MODEL LAYER VARIABLES.
C***
!$omp parallel do private(i,j,llmh)
      DO 100 J=MYJS2,MYJE2
      DO 100 I=MYIS,MYIE
      LLMH=LMH(I,J)
      ETALM(I,J)=AETA(LLMH)
      APELM(I,J)=APE(I,J,LLMH)
      TLM(I,J)=T(I,J,LLMH)
      QLM(I,J)=Q(I,J,LLMH)
      ZLM(I,J)=(ZINT(I,J,LLMH)-ZINT(I,J,LLMH+1))*0.50
  100 CONTINUE
C
!$omp parallel do private(i,j)
      DO 110 J=MYJS2,MYJE2
      DO 110 I=MYIS,MYIE
      PS(I,J)=PD(I,J)+PT
      APES(I,J)=(1.E5/PS(I,J))**CAPA
      PLM(I,J)=ETALM(I,J)*PD(I,J)*RES(I,J)+PT
      QLMS(I,J)=((1.-SM(I,J))*PQ0+SM(I,J)*PQ0SEA)
     1           /PLM(I,J)*EXP(A2*(TLM(I,J)-A3)/(TLM(I,J)-A4))
      DQSDT(I,J)=QLMS(I,J)*A23M4/(TLM(I,J)-A4)**2
      FFSK=AKHS(I,J)*PLM(I,J)*HBM2(I,J)/((QLM(I,J)*.608+1.)*TLM(I,J)*R)
      QFC1(I,J)=APES(I,J)*FFSK*ELWV
      FFS(I,J)=FFSK*CP
  110 CONTINUE
C-----------------------------------------------------------------------
!$omp parallel do private(i,j,factrs,factrl,tlmh)
      DO 120 J=MYJS2,MYJE2
      DO 120 I=MYIS,MYIE
C***
C***  COMPUTE RADIN AND RDSIN FOR THIS TIMESTEP
C***  CZEN IS IN PHYS COMMON AND IS CURRENT FROM CALL TO RDTEMP
C***
      IF(CZMEAN(I,J).GT.0.)THEN
        FACTRS=CZEN(I,J)/CZMEAN(I,J)
      ELSE
        FACTRS=0.
      ENDIF
C
      IF(SIGT4(I,J).GT.0.)THEN
        TLMH=TLM(I,J)
        FACTRL=STBOL*TLMH*TLMH*TLMH*TLMH/SIGT4(I,J)
      ELSE
        FACTRL=0.
      ENDIF
C
      RADIN(I,J)=((RSWIN(I,J)-RSWOUT(I,J))*FACTRS+
     &            RLWIN(I,J)*FACTRL)*HBM2(I,J)
      RDSIN(I,J)= RSWIN(I,J)*HBM2(I,J)*FACTRS
C***
C***  DIAGNOSTIC RADIATION ACCUMULATION
C***
      ASWIN (I,J)=ASWIN (I,J)+RSWIN (I,J)*HBM2(I,J)*FACTRS
      ASWOUT(I,J)=ASWOUT(I,J)-RSWOUT(I,J)*HBM2(I,J)*FACTRS
      ASWTOA(I,J)=ASWTOA(I,J)+RSWTOA(I,J)*HBM2(I,J)*FACTRS
      ALWIN (I,J)=ALWIN (I,J)+RLWIN (I,J)*HBM2(I,J)*FACTRL
      ALWOUT(I,J)=ALWOUT(I,J)-RADOT (I,J)*HBM2(I,J)
      ALWTOA(I,J)=ALWTOA(I,J)+RLWTOA(I,J)*HBM2(I,J)
C***
C***  CHECK FOR SATURATION AT THE LOWEST MODEL LEVEL
C***
      IF((QLM(I,J).GE.QLMS(I,J)*TRESH).AND.(QLM(I,J).LT.QZ0(I,J)))THEN
        CKLQ(I,J)=0.
      ELSE
        CKLQ(I,J)=HBM2(I,J)
      ENDIF
 120  CONTINUE
C-----------------------------------------------------------------------
C***
C***  THS, THLM, CHEATING WET FOR PROFS
C***
!$omp parallel do private(i,j)
      DO 130 J=MYJS2,MYJE2
      DO 130 I=MYIS,MYIE
      THLM(I,J)=TLM(I,J)*APELM(I,J)
      QFC1(I,J)=QFC1(I,J)*CKLQ(I,J)
  130 CONTINUE
C
!!$omp parallel do
!!$omp&  private(chk,chkff,cmck,dqsdtk,dtk,elflx,fk,gflx)
!!$omp&  private (hflx,i,ice,isltpk,ivgtpk,j,ns,plflx,prcp)
!!$omp&  private (q1k,q2k,q2sat,rnof1k,rnof2k,satflg,scheck)
!!$omp&  private (sfcprs,sfcth2,sfctmp,smck,smeltk,snodpk)
!!$omp&  private (soilqm,soilqw,soldn,stck,t1k,tbot,vgfrck,z)
C
      DO 160 J=MYJS2,MYJE2
      DO 155 I=MYIS,MYIE
      IF(HBM2(I,J).LT.0.5)GO TO 155
      IF(SM(I,J).GT.0.5)THEN
        THS(I,J)=SST(I,J)*APES(I,J)
        QS(I,J)=HBM2(I,J)*PQ0SEA/PS(I,J)
     1         *EXP(A2*(THS(I,J)-A3*APES(I,J))/(THS(I,J)-A4*APES(I,J)))
      ENDIF
C***
C***  LOADING AND UNLOADING NMC/OSU LAND SOIL VARIABLES
C***
      IF(SM(I,J).LT.0.5)THEN
        ICE=INT(SICE(I,J)+0.3)
        SATFLG=CKLQ(I,J)
        DTK=DTQ2
        Z=ZLM(I,J)
        FK=RADIN(I,J)
        SOLDN=RDSIN(I,J)
        SFCPRS=PLM(I,J)
        PRCP=PREC(I,J)*ROW/DTQ2
        Q2K=QLM(I,J)
        Q2SAT=QLMS(I,J)
        DQSDTK=DQSDT(I,J)
        TBOT=TG(I,J)
        CHK=AKHS(I,J)
        CHKFF=FFS(I,J)
        IVGTPK=IVGTYP(I,J)
        ISLTPK=ISLTYP(I,J)
C  MEB  PREVENT ROUTINES IN SFLX FROM GOING OUT OF BOUNDS
        IF (IVGTPK.EQ.0) IVGTPK=13
        IF (ISLTPK.EQ.0) ISLTPK=9
C  MEB  PREVENT ROUTINES IN SFLX FROM GOING OUT OF BOUNDS
        VGFRCK=VEGFRC(I,J)
        Q1K=QS(I,J)
        SFCTMP=THLM(I,J)/APELM(I,J)
        SFCTH2=THLM(I,J)/APES(I,J)
        T1K=THS(I,J)/APES(I,J)
        CMCK=CMC(I,J)
        SNODPK=SNO(I,J)
C
        DO 140 NS=1,NSOIL
        SMCK(NS)=SMC(I,J,NS)
        STCK(NS)=STC(I,J,NS)
  140   CONTINUE
C
C-----------------------------------------------------------------------
        CALL SFLX
     &     (ICE   ,SATFLG,DTK   ,Z, NSOIL, NROOT, SLDPTH
     &,     FK    ,SOLDN ,SFCPRS,PRCP  ,SFCTMP,SFCTH2
     &,     Q2K   ,Q2SAT ,DQSDTK,TBOT  ,CHK,   CHKFF
     &,     IVGTPK,ISLTPK,VGFRCK
     &,     PLFLX ,ELFLX ,HFLX  ,GFLX  ,RNOF1K,RNOF2K
     &,     Q1K   ,SMELTK,T1K   ,CMCK  ,SMCK  ,STCK  ,SNODPK
     &,     SOILQW,SOILQM )
C-----------------------------------------------------------------------
C
      SCHECK=Z*CHK
      IF(SCHECK.LE.1.3E-3)THEN
        PLFLX=0.
        ELFLX=0.
      ENDIF
C***
C***  GCIP DIAGNOSTICS & MODIFICATION OF QFC1 OVER SNOW
C***
        SSROFF(I,J)=SSROFF(I,J)+RNOF1K*DTQ2
        BGROFF(I,J)=BGROFF(I,J)+RNOF2K*DTQ2
        SMSTAV(I,J)=SOILQW
        SOILTB(I,J)=TBOT
        SFCEXC(I,J)=CHK
        GRNFLX(I,J)=GFLX
        IF(SNO (I,J).GT.0..OR.SICE(I,J).GT.0.5)THEN
          QFC1(I,J)=QFC1(I,J)*RLIVWV
        ENDIF
        IF(SNO(I,J).GT.0.)THEN
          ACSNOM(I,J)=ACSNOM(I,J)+SMELTK
          SNOPCX(I,J)=SNOPCX(I,J)-SMELTK/FDTLIW
        ENDIF
        POTEVP(I,J)=POTEVP(I,J)+PLFLX*FDTW
        POTFLX(I,J)=POTFLX(I,J)-PLFLX
        SUBSHX(I,J)=SUBSHX(I,J)+GFLX
C***
C***  ETA MODEL LOWER BOUNDARY CONDITIONS
C***
C       THS(I,J)=THLM(I,J)+HFLX*APES(I,J)/FFS(I,J)
        THS(I,J)=T1K*APES(I,J)
        IF(QFC1(I,J).GT.0.)
     1    QS(I,J)=QLM(I,J)+ELFLX*APES(I,J)/QFC1(I,J)
C***
C***  HISTORICAL VARIABLES
C***
        SNO(I,J)=SNODPK
        CMC(I,J)=CMCK
C
        SMSTOT(I,J)=SOILQM
        DO 150 NS=1,NSOIL
        SMC(I,J,NS)=SMCK(NS)
        STC(I,J,NS)=STCK(NS)
  150   CONTINUE
      ENDIF
C
  155 CONTINUE
  160 CONTINUE
C
C***  VARIABLES TWBS AND QWBS COMPUTED HERE FOR GCIP.
C***  ACCUMULATE SURFACE HEAT FLUXES HERE.
C***  FOR GCIP ACCUMULATE ACTUAL AND POTENTIAL EVAPORATION.
C***  FOR GCIP ACCUMULATE TOTAL SNOW MELT AND
C***  THE ASSOCIATED NET HEAT FLUX.
C
!$omp parallel do private(i,j)
      DO 200 J=MYJS2,MYJE2
      DO 200 I=MYIS,MYIE
      TWBS(I,J)=(THLM(I,J)-THS(I,J)*(1.-SM(I,J))-THZ0(I,J)*SM(I,J))
     1       *FFS (I,J)/APES(I,J)
      QWBS(I,J)=(QLM (I,J)-QS (I,J)*(1.-SM(I,J))-QZ0 (I,J)*SM(I,J))
     1       *QFC1(I,J)/APES(I,J)
      SFCSHX(I,J)=SFCSHX(I,J)+TWBS(I,J)
      SFCLHX(I,J)=SFCLHX(I,J)+QWBS(I,J)
      SFCEVP(I,J)=SFCEVP(I,J)-QWBS(I,J)*FDTW
      POTEVP(I,J)=POTEVP(I,J)-QWBS(I,J)*SM(I,J)*FDTW
      POTFLX(I,J)=POTFLX(I,J)+QWBS(I,J)*SM(I,J)
C
C***  IF COLD ENOUGH, IT SNOWS (IN OSU MODEL)...
C***  FOR GCIP ACCUMULATE TOTAL SNOWFALL.
C
      IF(THLM(I,J)/APELM(I,J).LE.T0.AND.SICE(I,J)+SM(I,J).LT.0.5)THEN
        ACSNOW(I,J)=ACSNOW(I,J)+PREC(I,J)
C***
C***  ... OTHERWISE IT RAINS.
C***
      ELSE
        ACCLIQ(I,J)=ACCLIQ(I,J)+PREC(I,J)
      ENDIF
C
      PREC(I,J)=0.
  200 CONTINUE
C***
C***  LONGWAVE OUTGOING RADIATION
C***
!$omp parallel do private(i,j,tsfc,tsfc2)
      DO 210 J=MYJS2,MYJE2
      DO 210 I=MYIS,MYIE
      TSFC=THS(I,J)/APES(I,J)
      TSFC2=TSFC*TSFC
      RADOT(I,J)=HBM2(I,J)*EPSR(I,J)*STBOL*TSFC2*TSFC2
  210 CONTINUE
C
C-----------------------------------------------------------------------
C
C   INCREMENT TIME STEP COUNTERS FOR USE IN COMPUTING TIME AVE VALUES
C
      APHTIM = APHTIM + 1.
      ARDSW  = ARDSW  + 1.
      ARDLW  = ARDLW  + 1.
      ASRFC  = ASRFC  + 1.
C-----------------------------------------------------------------------
                             RETURN
                             END
C
      BLOCK DATA OPT
      COMMON /OPTIONS/ SPVAL,IBESSL,KSB,IOFFS,IFLAG,SATDEL
      DATA SPVAL  / 99999  /
      DATA IBESSL /   0    /
      DATA KSB    /   3    /
      DATA IOFFS  /   2    /
      DATA IFLAG  /   0    /
      DATA SATDEL / 0.05   /
      END BLOCK DATA OPT
