                             SUBROUTINE PGCOR
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    PGCOR       PRESSURE GRADIENT/CORIOLIS CALC
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 93-10-28
C
C ABSTRACT:
C     PGCOR CALCULATES THE PRESSURE GRADIENT FORCE, UPDATES THE
C     VELOCITY COMPONENTS DUE TO THE EFFECT OF THE PRESSURE GRADIENT
C     AND CORIOLIS FORCES.
C
C PROGRAM HISTORY LOG:
C   87-06-??  JANJIC     - ORIGINATOR
C   95-03-25  BLACK      - CONVERSION FROM 1-D TO 2-D IN HORIZONTAL
C   96-03-29  BLACK      - ADDED EXTERNAL EDGE
C   97-03-17  MESINGER   - SPLIT FROM PFDHT
C   98-10-28  BLACK      - MODIFIED FOR DISTRIBUTED MEMORY
C   00-10-20  BLACK      - INCORPORATED PRESSURE GRADIENT METHOD
C                          FROM MESO MODEL
C
C USAGE: CALL PGCOR FROM MAIN PROGRAM EBU
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
c-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
c-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
c-----------------------------------------------------------------------
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
C
                             R E A L
     & DPDE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,APEL  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ALP1  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,DFDZ  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PNE   (IDIM1:IDIM2,JDIM1:JDIM2),PSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,CNE   (IDIM1:IDIM2,JDIM1:JDIM2),CSE   (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PPNE  (IDIM1:IDIM2,JDIM1:JDIM2),PPSE  (IDIM1:IDIM2,JDIM1:JDIM2)
     &,PCNE  (IDIM1:IDIM2,JDIM1:JDIM2),PCSE  (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL ZERO2(ALP1)
      CALL ZERO2(DPDE)
      CALL ZERO2(APEL)
      CALL ZERO2(ADPDX)
      CALL ZERO2(ADPDY)
      CALL ZERO2(DFDZ)
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
C-----------------------------------------------------------------------
!$omp parallel do private (alp1p)
      DO J=MYJS_P5,MYJE_P5
      DO I=MYIS_P5,MYIE_P5
        ALP1P=PINTLG(I,J,LM+1)
        ALP1(I,J)=ALP1P
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C-------------------- MAIN VERTICAL INTEGRATION LOOP -------------------
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------

Cmp
	FIM=0.
Cmp
                             DO 400 L=LM,1,-1
C-----------------------------------------------------------------------
C***
C***  INTEGRATE THE GEOPOTENTIAL
C***
!$omp parallel do private (alp1p,dfi,fiupk,rdpds)
      DO 125 J=MYJS_P5,MYJE_P5
      DO 125 I=MYIS_P5,MYIE_P5
C
      ALP1P=PINTLG(I,J,L)
C
      DFI=(Q(I,J,L)*0.608+1.)*T(I,J,L)*R*(ALP1(I,J)-ALP1P)/DWDT(I,J,L)
C
      RDPDS=1./(DETA(L)*PDSL(I,J))
      FIUPK=FILO(I,J)+DFI
      FIM(I,J)=FILO(I,J)+FIUPK
C
      FILO(I,J)=(FIUPK-DFL(L))*HTM(I,J,L)+DFL(L)
C	if (MYPE.eq.4.and.I.eq.1.and.J.eq.2.and.L.eq.25) then
C	write(6,*) 'L,FILO, DFI-->: ', L,FILO(I,J),DFI
C	write(6,*) 'T,Q,ALP1,ALP1P,DWDT: ', T(I,J,L),Q(I,J,L),ALP1(I,J),
C     +					ALP1P,DWDT(I,J,L)
C	endif
	if (abs(FILO(I,J)) .lt. 20000000.) then
	else
	write(6,*) 'bad FILO value ', FILO(I,J),' on PE: ' ,
     +		MYPE, 'at ', I,J
	endif
      ALP1(I,J)=ALP1P
  125 CONTINUE
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
      DPDE(I,J)=DETA(L)*PDSL(I,J)
  205 CONTINUE
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
CC
CC	Having problem with global boundary here
CC
!$omp parallel do
	if (MYPE .eq. 5 .and. L .eq. LM ) then
	I=MYIE_P4
	J=MYJE_P4-2
C	write(6,*) 'bad FIM point: ',I+IHE(J),J+1,FIM (I+IHE(J),J+1)
	endif
      DO 240 J=MYJS_P4,MYJE_P4
      DO 240 I=MYIS_P4,MYIE_P4
      ADPDNE(I,J)=DPDE(I+IHE(J),J+1)+DPDE(I,J)
	if ( abs(FIM (I+IHE(J),J+1)) .lt. 2000000.) then
	else
	write(6,*) 'using FIM val: ', FIM (I+IHE(J),J+1), 'at point',
     +		I+IHE(J),J+1, 'on PE: ', MYPE
	endif
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
      PSE(I,J)=(FIM (I+IHE(J),J-1)-FIM (I,J))
     1        *(DWDT(I+IHE(J),J-1,L)+DWDT(I,J,L))
      PPSE(I,J)=PSE(I,J)*ADPDSE(I,J)
      CSE(I,J)=(DFDZ(I+IHE(J),J-1)+DFDZ(I,J))*2.
     1        *(APEL(I+IHE(J),J-1)-APEL(I,J))
      PCSE(I,J)=CSE(I,J)*ADPDSE(I,J)
  250 CONTINUE
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
C--------------UPDATE U AND V (CORIOLIS & PGF)--------------------------
C
!$omp parallel do private (dpfnek,dpfsek)
      DO 290 J=MYJS2_P3,MYJE2_P3
      DO 290 I=MYIS_P3,MYIE1_P3
      DPFNEK=((PPNE(I+IVW(J),J)+PPNE(I,J-1))
     1       +(PCNE(I+IVW(J),J)+PCNE(I,J-1)))*2.
      DPFSEK=((PPSE(I+IVW(J),J)+PPSE(I,J+1))
     1       +(PCSE(I+IVW(J),J)+PCSE(I,J+1)))*2.
      DPFEW(I,J)=DPFNEK+DPFSEK
      DPFNS(I,J)=DPFNEK-DPFSEK
  290 CONTINUE
C
!$omp parallel do private (f0k,upk,utk,vpk,vtk)
      DO 300 J=MYJS2_P2,MYJE2_P2
      DO 300 I=MYIS_P2,MYIE1_P2
      F0K=U(I,J,L)*CURV(I,J)+F(I,J)
      VM(I,J)=VTM(I,J,L)*VBM2(I,J)
      UPK=((DPFEW(I,J)+PCEW(I,J))*RDPDX(I,J)
     1       +PEW(I,J))*CPGFU(I,J)+F0K*V(I,J,L)+U(I,J,L)
      VPK=((DPFNS(I,J)+PCNS(I,J))*RDPDY(I,J)
     1       +PNS(I,J))*CPGFV-F0K*U(I,J,L)+V(I,J,L)
      UTK=U(I,J,L)
      VTK=V(I,J,L)
      U(I,J,L)=((F0K*VPK+UPK)/(F0K*F0K+1.)-UTK)
     1         *VM(I,J)+UTK
      V(I,J,L)=(VPK-F0K*U(I,J,L)-VTK)
     1         *VM(I,J)+VTK
  300 CONTINUE
C-----------------------------------------------------------------------
  400 CONTINUE
C-----------------------------------------------------------------------
                             RETURN
                             END
