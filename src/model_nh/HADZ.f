C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                             SUBROUTINE HADZ
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    HADZ        HORIZONTAL ADVECTION OF HEIGHT
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 96-05-??
C
C ABSTRACT:
C     HADZ CALCULATES DIAGNOSTICALLY THE CONTRIBUTION OF
C     THE HORIZONTAL ADVECTION OF HEIGHT
C
C PROGRAM HISTORY LOG:
C   96-05-??  JANJIC     - ORIGINATOR
C   00-01-04  BLACK      - DISTRIBUTED MEMORY AND THREADS
C
C USAGE: CALL HADZ FROM MAIN PROGRAM
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
C                  VRBLS
C                  CONTIN
C                  NHYDRO
C                  INDX
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C***********************************************************************
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(JAM=6+2*(JM-10)
     &,IMJM=IM*JM-JM/2,LM1=LM-1,LP1=LM+1)
                             P A R A M E T E R
     &(G=9.8,NTSHY=2)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
c-----------------------------------------------------------------------
      include "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
                             R E A L
     & HBMS  (IDIM1:IDIM2,JDIM1:JDIM2),DPDE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UDY   (IDIM1:IDIM2,JDIM1:JDIM2),VDX   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UNED  (IDIM1:IDIM2,JDIM1:JDIM2),USED  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ZEW   (IDIM1:IDIM2,JDIM1:JDIM2),ZNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ZNE   (IDIM1:IDIM2,JDIM1:JDIM2),ZSE   (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C
      IF(NTSD.LE.NTSHY.OR.HYDRO)THEN
!$omp parallel do
        DO L=1,LM
          DO J=MYJS,MYJE
          DO I=MYIS,MYIE
            W(I,J,L)=0.
          ENDDO
          ENDDO
        ENDDO
C***
        RETURN
C***
      ENDIF
C***********************************************************************
C-----------------------------------------------------------------------
!$omp parallel do
!$omp& private (dpde,ihh,ihl,ivh,ivl,ix,jx,udy,uned,used,
!$omp&          vdx,zew,zne,zns,zse)
      DO 200 L=1,LM
C-----------------------------------------------------------------------
      DO J=MYJS_P3,MYJE_P3
      DO I=MYIS_P3,MYIE_P3
        DPDE(I,J)=PDSL(I,J)*DETA(L)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C--------------MASS FLUXES AND MASS POINTS ADVECTION COMPONENTS---------
C-----------------------------------------------------------------------
      DO 125 J=2,JM-1
      IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
        JX=J-MY_JS_GLB+1
        IVL=2-MOD(J,2)
        IVH=IM-1
C
        DO 120 I=IVL,IVH
        IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
          IX=I-MY_IS_GLB+1
          UDY(IX,JX)=U(IX,JX,L)*DY
          ZEW(IX,JX)=UDY(IX,JX)
     1            *(DPDE(IX+IVW(JX),JX  )+DPDE(IX+IVE(JX),JX  ))
     2            *(Z   (IX+IVE(JX),JX,L)-Z   (IX+IVW(JX),JX,L))
          VDX(IX,JX)=V(IX,JX,L)*DX(IX,JX)
          ZNS(IX,JX)=VDX(IX,JX)
     1            *(DPDE(IX      ,JX-1  )+DPDE(IX      ,JX+1  ))
     2            *(Z   (IX      ,JX+1,L)-Z   (IX      ,JX-1,L))
          UNED(IX,JX)=UDY(IX,JX)+VDX(IX,JX)
          USED(IX,JX)=UDY(IX,JX)-VDX(IX,JX)
        ENDIF
 120    CONTINUE
      ENDIF
 125  CONTINUE
C-----------------------------------------------------------------------
C--------------DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND-------------
C-----------------------------------------------------------------------
      DO 145 J=2,JM-2
      IF(J.GE.MY_JS_GLB-JBPAD1.AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JX=J-MY_JS_GLB+1
        IHL=2-MOD(J+1,2)
        IHH=IM-2+MOD(J,2)
C
        DO 140 I=IHL,IHH
        IF(I.GE.MY_IS_GLB-ILPAD1.AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          IX=I-MY_IS_GLB+1
          ZNE(IX,JX)=(UNED(IX+IHE(JX),JX)   +UNED(IX        ,JX+1))
     1              *(DPDE(IX        ,JX)   +DPDE(IX+IHE(JX),JX+1))
     2              *(Z   (IX+IHE(JX),JX+1,L)-Z  (IX      ,JX ,L))
        ENDIF
  140   CONTINUE
      ENDIF
  145 CONTINUE
C
      DO 165 J=3,JM-1
      IF(J.GE.MY_JS_GLB-JBPAD1.AND.J.LE.MY_JE_GLB+JTPAD1)THEN
        JX=J-MY_JS_GLB+1
        IHL=2-MOD(J+1,2)
        IHH=IM-2+MOD(J,2)
C
        DO 160 I=IHL,IHH
        IF(I.GE.MY_IS_GLB-ILPAD1.AND.I.LE.MY_IE_GLB+IRPAD1)THEN
          IX=I-MY_IS_GLB+1
          ZSE(IX,JX)=(USED(IX+IHE(JX),JX   ) +USED(IX        ,JX-1  ))
     1              *(DPDE(IX        ,JX   ) +DPDE(IX+IHE(JX),JX-1  ))
     2              *(Z   (IX+IHE(JX),JX-1,L)-Z   (IX        ,JX ,L))
        ENDIF
  160   CONTINUE
      ENDIF
  165 CONTINUE
C-----------------------------------------------------------------------
C--------------ADVECTION OF Z-------------------------------------------
C-----------------------------------------------------------------------
c      DO 170 J=4,JM-3
c      IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
c        IHL=2+MOD(J,2)
c        IHH=IM-2
c        DO 171 I=IHL,IHH
c
      DO 175 J=3,JM-2
      IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
        JX=J-MY_JS_GLB+1
        IHL=2
        IHH=IM-2+MOD(J,2)
C
        DO 170 I=IHL,IHH
        IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
          IX=I-MY_IS_GLB+1
          W(IX,JX,L)=
     1          -(ZEW(IX+IHW(JX),JX  )+ZEW(IX+IHE(JX),JX  )
     1           +ZNS(IX        ,JX-1)+ZNS(IX        ,JX+1)
     2           +ZNE(IX+IHW(JX),JX-1)+ZNE(IX        ,JX  )
     2           +ZSE(IX        ,JX  )+ZSE(IX+IHW(JX),JX+1))
     3           *FAD(IX,JX)*HTM(IX,JX,L)*HBM2(IX,JX)/(DPDE(IX,JX)*DT)
     4           +W(IX,JX,L)
        ENDIF
  170   CONTINUE
      ENDIF
  175 CONTINUE
C-----------------------------------------------------------------------
  200 CONTINUE
C-----------------------------------------------------------------------
C***********************************************************************
c      NSMUD=7
c      DO J=MYJS,MYJE
c      DO I=MYIS,MYIE
c        HBMS(I,J)=HBM2(I,J)
c      ENDDO
c      ENDDO
cC
cC    JHL MUST BE ODD!!!
c      JHL=9
c      JHH=JM-JHL+1
cC
c      DO 225 J=JHL,JHH
c      IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
c        JX=J-MY_JS_GLB+1
c        IHL=JHL/2+1
c        IHH=IM-IHL+MOD(J,2)
c        DO I=IHL,IHH
c        IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
c          IX=I-MY_IS_GLB+1
c          HBMS(IX,JX)=0.
c        ENDDO
c      ENDIF
c  225 CONTINUE
cC-----------------------------------------------------------------------
c!#omp paralle do private (ihh,ihl,ix,jx,zne,zse)
c      DO 300 L=1,LM
cC-----------------------------------------------------------------------
c      DO KS=1,NSMUD
cC
c        DO J=MYJS,MYJE1
c        DO I=MYIS,MYIE1
c          ZNE(I,J)=(W(I+IHE(J),J+1,L)-W(I,J,L))
c     &             *HTM(I,J,L)*HTM(I+IHE(J),J+1,L)
c        ENDDO
c        ENDDO
cC
c        DO J=MYJS1,MYJE
c        DO I=MYIS,MYIE1
c          ZSE(I,J)=(W(I+IHE(J),J-1,L)-W(I,J,L))
c     &             *HTM(I+IHE(J),J-1,L)*HTM(I,J,L)
c        ENDDO
c        ENDDO
cC
c        DO 250 J=3,JM-2
c        IF(J.GE.MY_JS_GLB.AND.J.LE.MY_JE_GLB)THEN
c          JX=J-MY_JS_GLB+1
c          IHL=2
c          IHH=IM-2+MOD(J,2)
cC
c          DO 245 I=2,IM-2
c          IF(I.GE.MY_IS_GLB.AND.I.LE.MY_IE_GLB)THEN
c            IX=I-MY_IS_GLB+1
c            W(IX,JX,L)=(ZNE(IX,JX)-ZNE(IX+IHW(JX),JX-1)
c     &           +ZSE(IX,JX)-ZSE(IX+IHW(JX),JX+1))
c     &          *HBMS(IX,JX)*0.125+W(IX,JX,L)
c          ENDIF
c  245     CONTINUE
c        ENDIF
c  250   CONTINUE
c      ENDDO
cC----------------------------------------------------------------------
c  300 CONTINUE
cC----------------------------------------------------------------------
      RETURN
      END
