C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE PDTEDT
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    PDTEDT      SURFACE PRESSURE TENDENCY CALC
C   PRGRMMR: JANJIC          ORG: W/NMC2     DATE: 96-07-??
C
C ABSTRACT:
C     PDTEDT VERTICALLY INTEGRATES THE MASS FLUX DIVERGENCE TO
C     OBTAIN THE SURFACE PRESSURE TENDENCY AND ETADOT ON THE
C     LAYER INTERFACES.  THEN IT UPDATES THE HYDROSTATIC SURFACE
C     PRESSURE, THE NONHYDROSTATIC PRESSURE, AND ADDS THE LOCAL TIME
C     DERIVATIVE AND VERTICAL ADVECTION OF NONHYDROSTATIC PRESSURE
C     CONTRIBUTION TO THE OMEGA-ALPHA TERM OF THE THERMODYNAMIC
C     EQUATION.  ALSO, THE OMEGA-ALPHA TERM IS COMPUTED FOR DIAGNOSTICS.
C
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-05-??  JANJIC     - ADDED NONHYDROSTATIC EFFECTS & MERGED THE
C                          PREVIOUS SUBROUTINES PDTE & PDNEW
C   00-01-03  BLACK      - DISTRIBUTED MEMORY AND THREADS
C
C USAGE: CALL PDTEDT FROM MAIN PROGRAM
C   INPUT ARGUMENT LIST:
C       NONE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED:
C
C     UNIQUE: NONE
C
C     LIBRARY: NONE
C
C   COMMON BLOCKS: CTLBLK
C                  LOOPS
C                  MASKS
C                  DYNAM
C                  CONTIN
C                  VRBLS
C                  NHYDRO
C                  INDX
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C***********************************************************************
C-----------------------------------------------------------------------
      INCLUDE "EXCHM.h"
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)
     &,LM1=LM-1,LP1=LM+1
     &,ITRMX=0,WC=2./3.,RWCQ=(1.-WC)*0.25,RWC=1./WC)
C
                             P A R A M E T E R
     & (KSMUD=7,LNSDT=7)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
c-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
                             R E A L
     & PRET  (IDIM1:IDIM2,JDIM1:JDIM2),RPSL  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNE   (IDIM1:IDIM2,JDIM1:JDIM2),FSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,HBMS  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TTB   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,APDT  (IDIM1:IDIM2,JDIM1:JDIM2),PPDT  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TPM   (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***
C***  THE FOLLOWING ARE USED FOR TIMIMG PURPOSES ONLY
C***
      real*8 timef
      real nhb_tim,mpp_tim,init_tim
      common/timing/surfce_tim,nhb_tim,res_tim,exch_tim
C***********************************************************************
C-----------------------------------------------------------------------
C
      CALL ZERO2(PDSLO)
C
C--------------COMPUTATION OF PRESSURE TENDENCY & PREPARATIONS----------
C
      DO 100 L=2,LM
C
!$omp parallel do
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        DIV(I,J,L)=DIV(I,J,L-1)+DIV(I,J,L)
      ENDDO
      ENDDO
C
  100 CONTINUE
C-----------------------------------------------------------------------
!$omp parallel do
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PSDT(I,J)=-DIV(I,J,LM)
        APDT(I,J)=PSDT(I,J)
        PPDT(I,J)=PSDT(I,J)
        PDSLO(I,J)=PDSL(I,J)
        RPSL(I,J)=1./PDSL(I,J)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C--------------HARD+SOFT AIR DEAVERAGING BLOCK--------------------------
C-----------------------------------------------------------------------
C     IF(ITRMX.GT.0)THEN
C----------------------------------------------------------------------
C       DO 220 ITR=1,ITRMX
C-----------------------------------------------------------------------
C
c!$omp parallel do
c       DO J=MYJS2_P4,MYJE2_P4
c         IHL=2
c         IHH=IM-2+MOD(J,2)
c
c         DO I=MYIS1_P4,MYIE1_P4
c           PPDT(I,J)=(APDT(I,J)
c    2               -(PSDT(I+IHW(J),J-1)+PSDT(I+IHE(J),J-1)
c    3                +PSDT(I+IHW(J),J+1)+PSDT(I+IHE(J),J+1))*RWCQ)
c    4               *RWC
c         ENDDO
c       ENDDO
c
c!$omp parallel do
c       DO J=MYJS2_P4,MYJE2_P4
c       DO I=MYIS1_P4,MYIE1_P4
c         PSDT(I,J)=PPDT(I,J)*HBM2(I,J)+(1.-HBM2(I,J))*PSDT(I,J)
c       ENDDO
c       ENDDO
C-----------------------------------------------------------------------
c 220   CONTINUE
C-----------------------------------------------------------------------
c     ENDIF
C-----------------------------------------------------------------------
!$omp parallel do
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        PRET(I,J)=PSDT(I,J)*RES(I,J)
        PDSL(I,J)=PD(I,J)*RES(I,J)
C
        PINT(I,J,1)=PT
C
        TPM(I,J)=PT+PINT(I,J,2)
        TTB(I,J)=0.
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C--------------COMPUTATION OF ETADT-------------------------------------
C-----------------------------------------------------------------------
!$omp parallel do
      DO 300 L=1,LM1
C
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        ETADT(I,J,L)=-(PRET(I,J)*ETA(L+1)+DIV(I,J,L))
     1                *HTM(I,J,L+1)*HBM2(I,J)*RPSL(I,J)
      ENDDO
      ENDDO
  300 CONTINUE
C-----------------------------------------------------------------------
C--------------KINETIC ENERGY GENERATION TERMS IN T EQUATION------------
C-----------------------------------------------------------------------
!$omp parallel do private (dwdtp,rhs,tpmp,ttal)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        DWDTP=DWDT(I,J,1)
        TPMP=PINT(I,J,2)+PINT(I,J,3)
c
c       TTAL=(T(I,J,2)-T(I,J,1))*ETADT(I,J,1)*F4D*0.5
c       TTAL=(T(I,J,2)-T(I,J,1))*ETADT(I,J,1)*F4D
c
        TTAL=0.
C
        RHS=-DIV(I,J,1)*RTOP(I,J,1)*HTM(I,J,1)*DWDTP*EF4T
        OMGALF(I,J,1)=OMGALF(I,J,1)+RHS
        T(I,J,1)=(TTAL*RDETA(1)+RHS)*HBM2(I,J)+T(I,J,1)
        PINT(I,J,2)=PRET(I,J)*(ETA(1)+ETA(2))*DWDTP*DT
     1             +TPM(I,J)-PINT(I,J,1)
C
        TPM(I,J)=TPMP
        TTB(I,J)=TTAL
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
      DO 410 L=2,LM1
C
!$omp parallel do private (dwdtp,rhs,tpmp,ttal)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        DWDTP=DWDT(I,J,L)
        TPMP=PINT(I,J,L+1)+PINT(I,J,L+2)
c
c       TTAL=(T(I,J,L+1)-T(I,J,L))*ETADT(I,J,L)*F4D*0.5
c       TTAL=(T(I,J,L+1)-T(I,J,L))*ETADT(I,J,L)*F4D
c
        TTAL=0.
C
        RHS=-(DIV(I,J,L-1)+DIV(I,J,L))*RTOP(I,J,L)*HTM(I,J,L)*DWDTP
     2      *EF4T
        OMGALF(I,J,L)=OMGALF(I,J,L)+RHS
        T(I,J,L)=((TTAL+TTB(I,J))*RDETA(L)+RHS)*HBM2(I,J)+T(I,J,L)
        PINT(I,J,L+1)=PRET(I,J)*(ETA(L)+ETA(L+1))*DWDTP*DT
     2               +TPM(I,J)-PINT(I,J,L)
C
        TPM(I,J)=TPMP
        TTB(I,J)=TTAL
      ENDDO
      ENDDO
C
  410 CONTINUE
c-----------------------------------------------------------------------
!$omp parallel do private (dwdtp,rhs)
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        DWDTP=DWDT(I,J,LM)
C
        RHS=-(DIV(I,J,LM1)+DIV(I,J,LM))*RTOP(I,J,LM)*HTM(I,J,LM)*DWDTP
     1      *EF4T
        OMGALF(I,J,LM)=OMGALF(I,J,LM)+RHS
        T     (I,J,LM)=(TTB(I,J)*RDETA(LM)+RHS)*HBM2(I,J)+T(I,J,LM)
        PINT(I,J,LM+1)=PRET(I,J)*(ETA(LM)+ETA(LM+1))*DWDTP*DT
     1                +TPM(I,J)-PINT(I,J,LM)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C--------------  REGENERATE THE UNINTEGRATED DIVERGENCE  ---------------
C-----------------------------------------------------------------------
      DO 425 L=LM,2,-1
!$omp parallel do
      DO J=MYJS,MYJE2
      DO I=MYIS,MYIE
        DIV(I,J,L)=DIV(I,J,L)-DIV(I,J,L-1)
      ENDDO
      ENDDO
C
  425 CONTINUE
C-----------------------------------------------------------------------
C--------------SMOOTHING VERTICAL VELOCITY ALONG BOUNDARIES-------------
C-----------------------------------------------------------------------
      IF(.NOT.HYDRO.AND.KSMUD.GT.0)THEN
C-----------------------------------------------------------------------
        NSMUD=KSMUD
C
!$omp parallel do
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          HBMS(I,J)=HBM2(I,J)
        ENDDO
        ENDDO
C
        JHL=LNSDT
        JHH=JM-JHL+1
C
        DO J=JHL,JHH
          IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
            IHL=JHL/2+1
            IHH=IM-IHL+MOD(J,2)
C
            DO I=IHL,IHH
              IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
                IX=I-MY_IS_GLB+1
                JX=J-MY_JS_GLB+1
                HBMS(IX,JX)=0.
              ENDIF
            ENDDO
C
          ENDIF
        ENDDO
C
C-----------------------------------------------------------------------
        DO KS=1,NSMUD
C-----------------------------------------------------------------------
C
!$omp parallel do private (etadtl,fne,fse)
          DO 450 L=1,LM-1
C
          DO J=MYJS_P1,MYJE1_P1
          DO I=MYIS_P1,MYIE1_P1
            FNE(I,J)=(ETADT(I+IHE(J),J+1,L)-ETADT(I,J,L))
     1               *HTM(I,J,L+1)*HTM(I+IHE(J),J+1,L+1)
          ENDDO
          ENDDO
C
          DO J=MYJS1_P1,MYJE_P1
          DO I=MYIS_P1,MYIE1_P1
            FSE(I,J)=(ETADT(I+IHE(J),J-1,L)-ETADT(I,J,L))
     1               *HTM(I+IHE(J),J-1,L+1)*HTM(I,J,L+1)
          ENDDO
          ENDDO
C
          DO J=MYJS2,MYJE2
          DO I=MYIS1,MYIE1
            ETADTL=(FNE(I,J)-FNE(I+IHW(J),J-1)
     1             +FSE(I,J)-FSE(I+IHW(J),J+1))*HBM2(I,J)
            ETADT(I,J,L)=ETADTL*HBMS(I,J)*0.125+ETADT(I,J,L)
          ENDDO
          ENDDO
C
  450     CONTINUE
C
          btim=timef()
          CALL EXCH(ETADT,LM-1,2,2)
          exch_tim=exch_tim+timef()-btim
C
        ENDDO
C
C-----------------------------------------------------------------------
      ENDIF
C-----------------------------------------------------------------------
      RETURN
      END
