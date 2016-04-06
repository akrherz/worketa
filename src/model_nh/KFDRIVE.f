                 SUBROUTINE KFDRIVE(HTOP,HBOT,CNVTOP,CNVBOT)
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    KFDRIVE     CONVECTIVE PRECIPITATION PARAMETERIZATION
C   PRGRMMR: KAIN            ORG: W/NP2      DATE: 00-03-30
C
C ABSTRACT:
C     KFDRIVE IS THE MAIN DRIVER FOR THE KAIN-FRITSCH CONVECTION.
C     GRID POINTS ARE ISOLATED TO BE CHECKED FOR POSSIBLE
C     INITAITION OF THE K-F SCHEME.
C
C
C PROGRAM HISTORY LOG:
C   ??-??-??  KAIN       - ORIGINATOR
C   00-04-13  BLACK      - INCORPORATED INTO ETA MODEL
C
C USAGE: CALL KFDRIVE FROM MAIN PROGRAM EBU
C
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     HTOP   - CONVECTIVE CLOUD TOP (MODEL LAYER) FOR RADTN
C     HBOT   - CONVECTIVE CLOUD BOTTOM (MODEL LAYER) FOR RADTN
C     CNVTOP - CONVECTIVE CLOUD TOP (MODEL LAYER) FOR CHKOUT
C     CNVBOT - CONVECTIVE CLOUD BOTTOM (MODEL LAYER) FOR CHKOUT
C
C   SUBPROGRAMS CALLED:
C
C     UNIQUE:
C        KFPARA
C
C     LIBRARY:
C        NONE
C
C   COMMON BLOCKS: CTLBLK
C                  VRBLS
C                  PVRBLS
C                  MASKS
C                  CONTIN
C                  DYNAM
C                  LOOPS
C                  KFFDBK
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
C----------------------------------------------------------------------
      PARAMETER(LP1=LM+1,JAM=6+2*(JM-10))
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C----------------------------------------------------------------------
      INCLUDE "KFFDBK.comm"
C----------------------------------------------------------------------
      PARAMETER(CP=1004.6,XLV=2.5E6,G=9.8)
      PARAMETER(ALIQ=613.3,BLIQ=17.502,CLIQ=4780.8
     1,         DLIQ=32.19)
C----------------------------------------------------------------------
                              R E A L
     1 HTOP  (IDIM1:IDIM2,JDIM1:JDIM2),HBOT  (IDIM1:IDIM2,JDIM1:JDIM2)
     2,CNVTOP(IDIM1:IDIM2,JDIM1:JDIM2),CNVBOT(IDIM1:IDIM2,JDIM1:JDIM2)
C
                              R E A L
     1 SEM(LM),SEMS(LM),PRS(LM),TV0(LM),Z00(LM),SED(LM),DP(LM)
C
                              I N T E G E R
     1 ICUYES(IDIM1:IDIM2),LSB(IDIM1:IDIM2),NSHALL(JDIM1:JDIM2)
C------------------------------------------------------------------
      ROVG=R/G
C------------------------------------------------------------------
C...NOTE:  SEARCHING ONLY LOWEST 200 mb!!!!!
C------------------------------------------------------------------
      DO L=1,LM
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          W0=-OMGALF(I,J,L)*CP/(G*DT)
          W0AVG(I,J,L)=(W0AVG(I,J,L)*(TSTKF-1.)+W0)/TSTKF
        ENDDO
        ENDDO
      ENDDO
C
      IF(MOD(NTSD-NCLDCK/2,NCLDCK).NE.0)RETURN
C
      DO J=JDIM1,JDIM2
        NSHALL(J)=0
      ENDDO
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
C***  MAKE A QUICK FOR CHECK FOR THESE THINGS THAT CAN ELIMINATE
C***  GRID POINTS FROM THE POSSIBILITY OF CONVECTIVE INITIATION:
C
C        1.) CONVECTION ALREADY ACTIVE AT THIS POINT
C        2.) POINT CLOSE TO GRID-DOMAIN BOUNDARY
C        3.) DOWNWARD MOTION AT ALL LEVELS IN LOWEST 300 MB
C        4.) NO CAPE
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
C
!$omp parallel do
!$omp& private(dp,es,i,icuyes,isink,l,l200,l400,lg,lsb,
!$omp&         ncuyes,p200,p400,prs,psfck,qss,qtmp,
!$omp&         sed,sem,sems,ttmp,tv0,z00)
      DO 100 J=MYJS2,MYJE2
C
      NCUYES=0
C

      ICUYES = 0
      LSB    = 0

      DO I=MYIS,MYIE
        ICUYES(I)=0
        LSB(I)=0
      ENDDO
C
      DO 60 I=MYIS1,MYIE1
      IF ( CNVBOT(I,J).LT.1. ) CNVBOT(I,J) = 0.0

C
      IF(NCA(I,J).GT.NCLDCK.OR.HBM2(I,J).EQ.0.)GO TO 60
      LG=LMH(I,J)
      PSFCK=PD(I,J)*RES(I,J)*AETA(LG)+PT
c     P300=PSFCK-3.E4
      P200=PSFCK-2.E4
      P400=PSFCK-4.E4
C
      DO L=LG,1,-1
        PRS(L)=PD(I,J)*RES(I,J)*AETA(L)+PT  
      ENDDO
C
      DO L=LG,1,-1
        IF(PRS(L).GT.P200)L200=L
        IF(PRS(L).GT.P400)L400=L
      ENDDO
C
C***  VERTICAL VELOCITY MUST BE UPWARD AT SOME LEVEL IN THE
C***  LOWEST 300 MB...
C***  LOWEST 400 MB...!!!!! 9/25/97 JSK
C
      ISINK=0
C
c     DO L=LG,L400,-1
      DO L=LG,L200,-1
c       IF(OMGALF(I,J,L).LT.0.)GO TO 25
        IF(OMGALF(I,J,L).LT.0..OR.Q2(I,J,L).GT.1.)GO TO 25
      ENDDO
C
      ISINK=1
C
   25 CONTINUE
C
C***  CALCULATE MOIST STATIC ENERGY AND SATURATION MOIST STATIC ENERGY
C***  AT EACH VERTICAL LEVEL.
C
      DO L=LG,1,-1
        TTMP=T(I,J,L)
        QTMP=Q(I,J,L)
        DP(L)=(ETA(L+1)-ETA(L))*PD(I,J)*RES(I,J)
        TV0(L)=TTMP*(1.+0.608*QTMP)
C
        IF(L.EQ.LG)THEN
          Z00(L)=0.
        ELSE
          Z00(L)=Z00(L+1)-ROVG*0.5*(TV0(L)+TV0(L+1))*
     1           ALOG(PRS(L)/PRS(L+1))
        ENDIF
C
        ES=ALIQ*EXP((BLIQ*TTMP-CLIQ)/(TTMP-DLIQ))
        QSS=0.622*ES/(PRS(L)-ES)
        SED(L)=CP*TTMP+G*Z00(L)
        SEMS(L)=SED(L)+XLV*QSS
        SEM(L)=SED(L)+XLV*QTMP
      ENDDO
C
C***  IF AIR IS SINKING EVERYWHERE IN LOWEST 400 MB, 
C***  REQUIRE SUPERADIABATIC LAYER IN LOWEST 200 MB 
C***  BEFORE CHECKING FOR CONVECTION IN KF SCHEME.
C
      IF(ISINK.EQ.1)THEN
        DO L=LG,L200-1,-1
          DO NL=L-1,L200,-1 
            IF(SED(L).GT.SED(NL))THEN
              NCUYES=NCUYES+1
              ICUYES(NCUYES)=I
              LSB(I)=LG-L+1
              GO TO 60   
            ENDIF
          ENDDO
        ENDDO
      ELSE
C
C***  IF THERE IS UPWARD MOTION, REQUIRE CONDITIONAL INSTABILITY 
C***  FOR A PARCEL ORIGINATING IN THE LOWEST 400 MB.
C
c       DO L=LG,L400,-1
        DO L=LG,L200,-1
          DO NL=L-1,1,-1
           IF(SEM(L).GT.SEMS(NL))THEN
             NCUYES=NCUYES+1
             ICUYES(NCUYES)=I
             LSB(I)=LG-L+1
             GO TO 60
          ENDIF
          ENDDO
        ENDDO
C
      ENDIF
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   60 CONTINUE       ! End of I loop
C- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
      IF(NCUYES.GT.0)THEN
        CALL KFPARA(NCUYES,ICUYES,J,LSB,HTOP,HBOT
     1,             CNVTOP,CNVBOT,NSHALL(J))
      ENDIF
C-----------------------------------------------------------------
  100 CONTINUE       ! End of J loop
C-----------------------------------------------------------------
      NSHALLOW=0
C
      DO J=MYJS2,MYJE2
        NSHALLOW=NSHALLOW+NSHALL(J)
      ENDDO
C
      CALL MPI_REDUCE(NSHALLOW,NSHAL,1,MPI_INTEGER,MPI_SUM,0
     1,               MPI_COMM_COMP,IRECV)
C-----------------------------------------------------------------
      RETURN
      END     
