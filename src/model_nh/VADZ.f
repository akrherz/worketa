C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE VADZ
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    VADZ        VERTICAL ADVECTION OF HEIGHT
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-11-17
C
C ABSTRACT:
C     VADV CALCULATES THE CONTRIBUTION OF THE VERTICAL ADVECTION
C     OF HEIGHT IN ORDER TO COMPUTE W=DZ/DT DIAGNOSTICALLY
C
C PROGRAM HISTORY LOG:
C   96-05-??  JANJIC     - ORIGINATOR
C   00-01-04  BLACK      - DISTRIBUTED MEMORY AND THREADS
C
C USAGE: CALL VADZ FROM MAIN PROGRAM
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
C                  MASKS
C                  LOOPS
C                  DYNAM
C                  VRBLS
C                  CONTIN
C                  NHYDRO
C                  INDX
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C***********************************************************************
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     & (IMJM=IM*JM-JM/2,JAM=6+2*(JM-10)
     &, LM1=LM-1,LM2=LM-2,LP1=LM+1,KSMUD=0)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      include "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
C-----------------------------------------------------------------------
      INCLUDE "CLDWTR.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
c-----------------------------------------------------------------------
                              R E A L
     & PRET  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TTB   (IDIM1:IDIM2,JDIM1:JDIM2),ALP1  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNE   (IDIM1:IDIM2,JDIM1:JDIM2),FSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ETADTL(IDIM1:IDIM2,JDIM1:JDIM2)
c-----------------------------------------------------------------------
      G=9.8
      RG=1./9.8
      RDT=1./DT
C-----------------------------------------------------------------------
!$omp parallel do
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        W(I,J,LM+1)=0.
        Z(I,J,LM+1)=0.
        IF(SIGMA)Z(I,J,LM+1)=FIS(I,J)*RG
        PRET(I,J)=PSDT(I,J)*RES(I,J)
c       PRET(I,J)=PSDT(I,J)/ETA(LMH(I,J)+1)
        ALP1(I,J)=ALOG(PINT(I,J,LM+1))
      ENDDO
      ENDDO
C***
      DO 50 L=LM,1,-1
      ZETA=DFL(L)*RG
      DTLRG=DETA(L)*RG
C
!$omp parallel do
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        ALP1P=ALOG(PINT(I,J,L))
        DZ=(Q(I,J,L)*0.608-CWM(I,J,L)+1.)*T(I,J,L)*(ALP1(I,J)-ALP1P)*R
     &    /(DWDT(I,J,L)*G)
        Z(I,J,L)=(Z(I,J,L+1)+DZ-ZETA)*HTM(I,J,L)+ZETA
        PDWDT(I,J,L)=DWDT(I,J,L)
        DWDT(I,J,L)=W(I,J,L)
        W(I,J,L)=(DZ-RTOP(I,J,L)*PDSLO(I,J)*DTLRG)*HTM(I,J,L)*HBM2(I,J)
     &         +W(I,J,L+1)
C
        ALP1(I,J)=ALP1P
      ENDDO
      ENDDO
   50 CONTINUE
c----------------------------------------------------------------------
c!$omp parallel do
c     DO L=1,LM+1
c       DO J=MYJS,MYJE
c       DO I=MYJS,MYJE
c          W(I,J,L)=(W(I,J,L)-W(I,J,1))*HTM(I,J,L)*HBM2(I,J)
c       ENDDO
c       ENDDO
c      ENDDO
c-----------------------------------------------------------------------
      DO L=1,LM
C
!$omp parallel do
        DO J=MYJS,MYJE
        DO I=MYIS,MYIE
          Z(I,J,L)=(Z(I,J,L)+Z(I,J,L+1))*0.5
          W(I,J,L)=(W(I,J,L)+W(I,J,L+1))*HTM(I,J,L)*HBM2(I,J)*0.5*RDT
        ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
C--------------SMOOTHING VERTICAL VELOCITY AT H POINTS------------------
C-----------------------------------------------------------------------
c      IF(KSMUD.GT.0)THEN
cC-----------------------------------------------------------------------
c        NSMUD=KSMUD
c        DO 90 L=1,LM1
cC
c        DO J=1,JM
c        DO I=1,IM
c          ETADT(I,J,L)=ETADT(I,J,L)*HBM2(I,J)
c        ENDDO
c        ENDDO
cC
c        DO 90 KS=1,NSMUD
c        DO J=MYJS,MYJE
c        DO I=MYIS,MYIE
c          FNE(I,J)=(ETADT(I+IHE(J),J+1,L)-ETADT(I,J,L))
c     1             *HTM(I,J,L+1)*HTM(I+IHE(J),J+1,L+1)
c        ENDDO
c        ENDDO
cC
c        DO J=MYJS1,MYJE
c        DO I=MYIS,MYIE
c          FSE(I,J)=(ETADT(I+IHE(J),J-1,L)-ETADT(I,J,L))
c     1             *HTM(I+IHE(J),J-1,L+1)*HTM(I,J,L+1)
c        ENDDO
c        ENDDO
cC
c        DO J=MYJS2,MYJE2
c        DO I=MYIS1,MYIE1
c          ETADTL(I,J)=(FNE(I,J)-FNE(I+IHW(J),J-1)
c     1                +FSE(I,J)-FSE(I+IHW(J),J+1))*HBM2(I,J)
c        ENDDO
c        ENDDO
cC
c        DO J=MYJS2,MYJE2
c        DO I=MYIS1,MYIE1
c          ETADT(I,J,L)=ETADTL(I,J)*0.125+ETADT(I,J,L)
c        ENDDO
c        ENDDO
c   90   CONTINUE
cC-----------------------------------------------------------------------
c      ENDIF
cC-----------------------------------------------------------------------
!$omp parallel do
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        TTB(I,J)=0.
      ENDDO
      ENDDO
C
      DO L=1,LM-1
!$omp parallel do
        DO J=MYJS2,MYJE2
        DO I=MYIS1,MYIE1
          TTAL=(Z(I,J,L+1)-Z(I,J,L))*ETADT(I,J,L)*0.5
          W(I,J,L)=(TTAL+TTB(I,J))*RDETA(L)+W(I,J,L)
          TTB(I,J)=TTAL
        ENDDO
        ENDDO
      ENDDO
C
!$omp parallel do
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        W(I,J,LM)=TTB(I,J)*RDETA(LM)+W(I,J,LM)
      ENDDO
      ENDDO
c-----------------------------------------------------------------------
                             RETURN
                             END
