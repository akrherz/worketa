                             SUBROUTINE DIVHOA
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    DIVHOA      DIVERGENCE/HORIZONTAL OMEGA-ALPHA
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-10-28
C
C ABSTRACT:
C     DIVHOA COMPUTES THE DIVERGENCE INCLUDING THE
C     MODIFICATION PREVENTING GRAVITY WAVE GRID SEPARATION, AND
C     CALCULATES THE HORIZONTAL PART OF THE OMEGA-ALPHA TERM
C     (THE PART PROPORTIONAL TO THE ADVECTION OF MASS ALONG
C     ETA/SIGMA SURFACES).
C
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-29  BLACK      - ADDED EXTERNAL EDGE
C   97-03-17  MESINGER   - SPLIT FROM PFDHT
C   98-10-30  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C   00-10-20  BLACK      - INCORPORATED PRESSURE GRADIENT METHOD
C                          FROM MESO MODEL
C
C USAGE: CALL DIVHOA FROM MAIN PROGRAM EBU
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
C                  INDX
C
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
     & (LP1=LM+1,JAM=6+2*(JM-10))
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA
C----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
c-----------------------------------------------------------------------
      include "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
c-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
C-----------------------------------------------------------------------
                             R E A L
     & PINTLG(IDIM1:IDIM2,JDIM1:JDIM2,LM+1)
C
                             R E A L
     & FIM   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FILO  (IDIM1:IDIM2,JDIM1:JDIM2),RDPD  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADPDX (IDIM1:IDIM2,JDIM1:JDIM2),RDPDX (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADPDY (IDIM1:IDIM2,JDIM1:JDIM2),RDPDY (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ADPDNE(IDIM1:IDIM2,JDIM1:JDIM2),ADPDSE(IDIM1:IDIM2,JDIM1:JDIM2)
     &,PEW   (IDIM1:IDIM2,JDIM1:JDIM2),PNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PCEW  (IDIM1:IDIM2,JDIM1:JDIM2),PCNS  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DPFEW (IDIM1:IDIM2,JDIM1:JDIM2),DPFNS (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNS   (IDIM1:IDIM2,JDIM1:JDIM2),TNS   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,HM    (IDIM1:IDIM2,JDIM1:JDIM2),VM    (IDIM1:IDIM2,JDIM1:JDIM2)
     &,EDIV  (IDIM1:IDIM2,JDIM1:JDIM2),DIVL  (IDIM1:IDIM2,JDIM1:JDIM2)
C
                             R E A L
     & DPDE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,APEL  (IDIM1:IDIM2,JDIM1:JDIM2),PCXC  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ALP1  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DFDZ  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,UDY   (IDIM1:IDIM2,JDIM1:JDIM2),VDX   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TEW   (IDIM1:IDIM2,JDIM1:JDIM2),FEW   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,TNE   (IDIM1:IDIM2,JDIM1:JDIM2),TSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,FNE   (IDIM1:IDIM2,JDIM1:JDIM2),FSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PNE   (IDIM1:IDIM2,JDIM1:JDIM2),PSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,CNE   (IDIM1:IDIM2,JDIM1:JDIM2),CSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PPNE  (IDIM1:IDIM2,JDIM1:JDIM2),PPSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PCNE  (IDIM1:IDIM2,JDIM1:JDIM2),PCSE  (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL ZERO2(ALP1)
      CALL ZERO2(DPDE)
      CALL ZERO2(APEL)
      CALL ZERO2(PCXC)
      CALL ZERO2(DFDZ)
      CALL ZERO2(UDY)
      CALL ZERO2(VDX)
      CALL ZERO2(TEW)
      CALL ZERO2(FEW)
      CALL ZERO2(TNE)
      CALL ZERO2(TSE)
      CALL ZERO2(FNE)
      CALL ZERO2(FSE)
      CALL ZERO2(PNE)
      CALL ZERO2(PSE)
      CALL ZERO2(CNE)
      CALL ZERO2(CSE)
      CALL ZERO2(PPNE)
      CALL ZERO2(PPSE)
      CALL ZERO2(PCNE)
      CALL ZERO2(PCSE)
C-----------------------------------------------------------------------
C--------------PREPARATORY CALCULATIONS---------------------------------
C-----------------------------------------------------------------------
      IF(SIGMA)THEN
!$omp parallel do
        DO 50 J=MYJS_P5,MYJE_P5
        DO 50 I=MYIS_P5,MYIE_P5
        FILO(I,J)=FIS(I,J)
        PDSL(I,J)=PD(I,J)
   50   CONTINUE
      ELSE
!$omp parallel do
        DO 100 J=MYJS_P5,MYJE_P5
        DO 100 I=MYIS_P5,MYIE_P5
        FILO(I,J)=0.0
        PDSL(I,J)=RES(I,J)*PD(I,J)
  100   CONTINUE
      ENDIF
C
      IF(HYDRO)THEN
!$omp parallel do
        DO L=1,LM+1
          DO J=MYJS_P5,MYJE_P5
          DO I=MYIS_P5,MYIE_P5
            PINTLG(I,J,L)=ALOG(ETA(L)*PDSL(I,J)+PT)
          ENDDO
          ENDDO
        ENDDO
      ELSE
!$omp parallel do
        DO L=1,LM+1
          DO J=MYJS_P5,MYJE_P5
          DO I=MYIS_P5,MYIE_P5
            PINTLG(I,J,L)=ALOG(PINT(I,J,L))
          ENDDO
          ENDDO
        ENDDO
      ENDIF
C
!$omp parallel do
      DO L=1,LM
        DO J=MYJS_P4,MYJE_P4
        DO I=MYIS_P4,MYIE_P4
          OMGALF(I,J,L)=0.
          DIV(I,J,L)=0.
        ENDDO
        ENDDO
      ENDDO
C-----------------------------------------------------------------------
!$omp parallel do private (alp1x)
      DO J=MYJS_P5,MYJE_P5
      DO I=MYIS_P5,MYIE_P5
        ALP1X=PINTLG(I,J,LM+1)
        ALP1(I,J)=ALP1X
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C-------------------- MAIN VERTICAL INTEGRATION LOOP -------------------
C-----------------------------------------------------------------------
                             DO 400 L=LM,1,-1
C-----------------------------------------------------------------------
C***
C***  INTEGRATE THE GEOPOTENTIAL
C***
C
!$omp parallel do private (alp1p,dfi,fiupk,rdpds)
      DO 125 J=MYJS_P5,MYJE_P5
      DO 125 I=MYIS_P5,MYIE_P5
C
      ALP1P=PINTLG(I,J,L)
C
      DFI=(Q(I,J,L)*0.608+1.)*T(I,J,L)*R*(ALP1(I,J)-ALP1P)/DWDT(I,J,L)
C
      RDPDS=1./(DETA(L)*PDSL(I,J))
      RTOP(I,J,L)=RDPDS*DFI
      FIUPK=FILO(I,J)+DFI
      FIM(I,J)=FILO(I,J)+FIUPK
C
      FILO(I,J)=(FIUPK-DFL(L))*HTM(I,J,L)+DFL(L)
      ALP1(I,J)=ALP1P
  125 CONTINUE
C
C-----------------------------------------------------------------------
!$omp parallel do private (alp1p,alp1pl,alp2p,alp2pl,dfi)
      DO 205 J=MYJS_P5,MYJE_P5
      DO 205 I=MYIS_P5,MYIE_P5
      HM(I,J)=HTM(I,J,L)*HBM2(I,J)
      VM(I,J)=VTM(I,J,L)*VBM2(I,J)
C
      ALP1P =PINTLG(I,J,L)
      ALP1PL=PINTLG(I,J,L+1)
      ALP2P =ALP1P*ALP1P
      ALP2PL=ALP1PL*ALP1PL
C
      DFI=(Q(I,J,L)*0.608+1.)*T(I,J,L)*R*(ALP1PL-ALP1P)/DWDT(I,J,L)
      DFDZ(I,J)=DFI*DWDT(I,J,L)/(ALP2PL-ALP2P)
      APEL(I,J)=(ALP2PL+ALP2P)*0.5
  205 CONTINUE
C
!$omp parallel do
      DO 210 J=MYJS_P4,MYJE_P4
      DO 210 I=MYIS_P4,MYIE_P4
      DPDE(I,J)=DETA(L)*PDSL(I,J)
      DIVL(I,J)=0.
      EDIV(I,J)=0.
  210 CONTINUE
C
!$omp parallel do
      DO 215 J=MYJS_P1,MYJE_P1
      DO 215 I=MYIS_P1,MYIE_P1
      RDPD(I,J)=1./DPDE(I,J)
  215 CONTINUE
C
!$omp parallel do
      DO 220 J=MYJS1_P3,MYJE1_P3
      DO 220 I=MYIS_P3,MYIE_P3
      ADPDX(I,J)=DPDE(I+IVW(J),J)+DPDE(I+IVE(J),J)
      ADPDY(I,J)=DPDE(I,J-1)+DPDE(I,J+1)
      RDPDX(I,J)=1./ADPDX(I,J)
      RDPDY(I,J)=1./ADPDY(I,J)
  220 CONTINUE
C
C--------------DIAGONAL CONTRIBUTIONS TO PRESSURE GRADIENT FORCE--------
C
!$omp parallel do
      DO 240 J=MYJS_P4,MYJE_P4
      DO 240 I=MYIS_P4,MYIE_P4
      ADPDNE(I,J)=DPDE(I+IHE(J),J+1)+DPDE(I,J)
      PNE(I,J)=(FIM (I+IHE(J),J+1)-FIM (I,J))
     1        *(DWDT(I+IHE(J),J+1,L)+DWDT(I,J,L))
      PPNE(I,J)=PNE(I,J)*ADPDNE(I,J)
      CNE(I,J)=(DFDZ(I+IHE(J),J+1)+DFDZ(I,J))*2.
     1        *(APEL(I+IHE(J),J+1)-APEL(I,J))
      PCNE(I,J)=CNE(I,J)*ADPDNE(I,J)
  240 CONTINUE
C
!$omp parallel do
      DO 250 J=MYJS1_P4,MYJE_P4
      DO 250 I=MYIS_P4,MYIE1_P4
      ADPDSE(I,J)=DPDE(I+IHE(J),J-1)+DPDE(I,J)
      PSE(I,J)=(FIM(I+IHE(J),J-1)-FIM(I,J))
     1        *(DWDT(I+IHE(J),J-1,L)+DWDT(I,J,L))
      PPSE(I,J)=PSE(I,J)*ADPDSE(I,J)
      CSE(I,J)=(DFDZ(I+IHE(J),J-1)+DFDZ(I,J))*2.
     1        *(APEL(I+IHE(J),J-1)-APEL(I,J))
      PCSE(I,J)=CSE(I,J)*ADPDSE(I,J)
  250 CONTINUE
C
C--------------CONTINUITY EQUATION MODIFICATION-------------------------
C
!$omp parallel do
      DO 260 J=MYJS1_P1,MYJE1_P1
      DO 260 I=MYIS_P1,MYIE_P1
      PCXC(I,J)=VBM3(I,J)*VTM(I,J,L)*(PNE(I+IVW(J),J)
     1         +CNE(I+IVW(J),J)+PSE(I+IVW(J),J)+CSE(I+IVW(J),J)
     2         -PNE(I,J-1)-CNE(I,J-1)-PSE(I,J+1)-CSE(I,J+1))
  260 CONTINUE
C-----------------------------------------------------------------------
      DO 270 J=MYJS2,MYJE2
      DO 270 I=MYIS1,MYIE1
      DIV(I,J,L)=DETA(L)*WPDAR(I,J)
     1         *(PCXC(I+IHE(J),J)-PCXC(I,J+1)
     2          +PCXC(I+IHW(J),J)-PCXC(I,J-1))
  270 CONTINUE
C
C--------------LAT & LONG PRESSURE FORCE COMPONENTS---------------------
C
!$omp parallel do private (dcnek,dcsek,dpnek,dpsek)
      DO 280 J=MYJS1_P3,MYJE1_P3
      DO 280 I=MYIS_P3,MYIE_P3
      DPNEK=PNE(I+IVW(J),J)+PNE(I,J-1)
      DPSEK=PSE(I+IVW(J),J)+PSE(I,J+1)
      PEW(I,J)=DPNEK+DPSEK
      PNS(I,J)=DPNEK-DPSEK
      DCNEK=CNE(I+IVW(J),J)+CNE(I,J-1)
      DCSEK=CSE(I+IVW(J),J)+CSE(I,J+1)
      PCEW(I,J)=(DCNEK+DCSEK)*ADPDX(I,J)
      PCNS(I,J)=(DCNEK-DCSEK)*ADPDY(I,J)
  280 CONTINUE
C
C--------------LAT & LON FLUXES & OMEGA-ALPHA COMPONENTS----------------
C
!$omp parallel do
      DO 310 J=MYJS1_P3,MYJE1_P3
      DO 310 I=MYIS_P3,MYIE_P3
      UDY(I,J)=DY*U(I,J,L)
      FEW(I,J)=UDY(I,J)*ADPDX(I,J)
      TEW(I,J)=UDY(I,J)*PCEW(I,J)
      VDX(I,J)=DX(I,J)*V(I,J,L)
      FNS(I,J)=VDX(I,J)*ADPDY(I,J)
      TNS(I,J)=VDX(I,J)*PCNS(I,J)
  310 CONTINUE
C
C--------------DIAGONAL FLUXES AND DIAGONALLY AVERAGED WIND-------------
C
!$omp parallel do private (pvnek)
      DO 320 J=MYJS1_P2,MYJE2_P2
      DO 320 I=MYIS_P2,MYIE1_P2
      PVNEK=(UDY(I+IHE(J),J)+VDX(I+IHE(J),J))+(UDY(I,J+1)+VDX(I,J+1))
      FNE(I,J)=PVNEK*ADPDNE(I,J)
      TNE(I,J)=PVNEK*PCNE(I,J)*2.
  320 CONTINUE
C
!$omp parallel do private (pvsek)
      DO 330 J=MYJS2_P2,MYJE1_P2
      DO 330 I=MYIS_P2,MYIE1_P2
      PVSEK=(UDY(I+IHE(J),J)-VDX(I+IHE(J),J))+(UDY(I,J-1)-VDX(I,J-1))
      FSE(I,J)=PVSEK*ADPDSE(I,J)
      TSE(I,J)=PVSEK*PCSE(I,J)*2.
  330 CONTINUE
C
C--------------HORIZONTAL PART OF OMEGA-ALPHA & DIVERGENCE--------------
C
!$omp parallel do
      DO 340 J=MYJS2_P1,MYJE2_P1
      DO 340 I=MYIS1_P1,MYIE1_P1
      OMGALF(I,J,L)=(TEW(I+IHE(J),J)+TEW(I+IHW(J),J)+TNS(I,J+1)
     1              +TNS(I,J-1)+TNE(I,J)+TNE(I+IHW(J),J-1)+TSE(I,J)
     2              +TSE(I+IHW(J),J+1))*RDPD(I,J)*FCP(I,J)*HM(I,J)
      T(I,J,L)=OMGALF(I,J,L)+T(I,J,L)
      EDIV(I,J)=((FEW(I+IHE(J),J)+FNS(I,J+1)
     1           +FNE(I,J)+FSE(I,J))
     2          -(FEW(I+IHW(J),J)+FNS(I,J-1)
     3           +FNE(I+IHW(J),J-1)+FSE(I+IHW(J),J+1)))*FDIV(I,J)
      DIVL(I,J)=EDIV(I,J)*HBM2(I,J)
  340 CONTINUE
C
C------------------------------------------------------------------------
C
!$omp parallel do
      DO 390 J=MYJS,MYJE
      DO 390 I=MYIS,MYIE
      DIV(I,J,L)=(DIV(I,J,L)+DIVL(I,J))*HM(I,J)
  390 CONTINUE
C-----------------------------------------------------------------------
  400                        CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
