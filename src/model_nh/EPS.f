!&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
                          SUBROUTINE EPS
C     ******************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    EPS
C   PRGRMMR: JANJIC          ORG: W/NP22     DATE: 9?-??-??
C
C ABSTRACT:
C     EPS COMPUTES THE VERTICAL AND HORIZONTAL ADVECTION OF DZ/DT
C
C PROGRAM HISTORY LOG:
C   9?-??-??  JANJIC     - ORIGINATOR
C   00-01-05  BLACK      - DISTRIBUTED MEMORY AND THREADS
C
C USAGE: CALL EPS FROM MAIN PROGRAM
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
C                  PVRBLS
C                  NHYDRO
C                  INDX
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C***********************************************************************
      INCLUDE "parmeta"
      INCLUDE "mpp.h"
      INCLUDE "mpif.h"
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(IM1=IM-1,JAM=6+2*(JM-10)
     &,JAMD=(JAM*2-10)*3,LP1=LM+1)
C-----------------------------------------------------------------------
      PARAMETER (EPSQ=1.E-12,EPSQ2=0.2)
      PARAMETER (FF1=0.52500,FF2=-0.64813,FF3=0.24520,FF4=-0.12189)
C-----------------------------------------------------------------------
                             P A R A M E T E R
     &(NTSHY=3,EPSFC=1./1.05,EPSN=-EPSFC,EPSP=EPSFC,ZERO=1.E-06
     &,G=9.8,CP=1004.6,CAPA=287.04/CP,GMA=-287.04*(1.-CAPA)/2.)
C-----------------------------------------------------------------------
                             L O G I C A L
     & RUN,FIRST,RESTRT,SIGMA,TOP,BOT
C-----------------------------------------------------------------------
      INCLUDE "CTLBLK.comm"
C-----------------------------------------------------------------------
      INCLUDE "LOOPS.comm"
C-----------------------------------------------------------------------
      INCLUDE "MASKS.comm"
C-----------------------------------------------------------------------
      INCLUDE "DYNAM.comm"
C-----------------------------------------------------------------------
      INCLUDE "CONTIN.comm"
C-----------------------------------------------------------------------
      INCLUDE "VRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "PVRBLS.comm"
C-----------------------------------------------------------------------
      INCLUDE "NHYDRO.comm"
C-----------------------------------------------------------------------
      INCLUDE "INDX.comm"
C-----------------------------------------------------------------------
C-------------LOCAL VARIABLES REQUIRING DECLARATION---------------------
C-----------------------------------------------------------------------
                             R E A L
     & PONE(LM+1),PSTR(LM+1),PNP1(LM+1),COFF(LM+1),DFRC(LM+1)
     &,CHI(LM+1),CHIN(LM+1),WRES(LM+1),CLIM(LM+1)
C-----------------------------------------------------------------------
      REAL W3(LM),W4(LM)
      REAL ETADTL(LM),DWL(LM),AFR(LM)
      INTEGER LA(LM)
C-----------------------------------------------------------------------
                             R E A L
     & AFP (IDIM1:IDIM2,JDIM1:JDIM2,LM),AFQ (IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,W1  (IDIM1:IDIM2,JDIM1:JDIM2,LM),DWST(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,DARE(IDIM1:IDIM2,JDIM1:JDIM2)   ,DVOL(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,EMH (IDIM1:IDIM2,JDIM1:JDIM2)
     &,ANE (IDIM1:IDIM2,JDIM1:JDIM2)   ,ASE (IDIM1:IDIM2,JDIM1:JDIM2)
C-----------------------------------------------------------------------
                            I N T E G E R
     & IFPA(IDIM1:IDIM2,JDIM1:JDIM2,LM),IFQA(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,IFPF(IDIM1:IDIM2,JDIM1:JDIM2,LM),IFQF(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,JFPA(IDIM1:IDIM2,JDIM1:JDIM2,LM),JFQA(IDIM1:IDIM2,JDIM1:JDIM2,LM)
     &,JFPF(IDIM1:IDIM2,JDIM1:JDIM2,LM),JFQF(IDIM1:IDIM2,JDIM1:JDIM2,LM)
C-----------------------------------------------------------------------
                             R E A L
     & GSUMS(2,LM),XSUMS(2,LM)
C-----------------------------------------------------------------------
C***********************************************************************
C-----------------------------------------------------------------------
      IF(NTSD.LE.NTSHY.OR.HYDRO)THEN
C***
!$omp parallel do
        DO J=MYJS_P2,MYJE_P2
        DO I=MYIS_P1,MYIE_P1
          PINT(I,J,1)=PT
        ENDDO
        ENDDO
C
        DO L=1,LM
!$omp parallel do
          DO J=MYJS_P2,MYJE_P2
          DO I=MYIS_P1,MYIE_P1
            DWDT(I,J,L)=1.
            PDWDT(I,J,L)=1.
            PINT(I,J,L+1)=PDSL(I,J)*DETA(L)+PINT(I,J,L)
          ENDDO
          ENDDO
        ENDDO
C***
        RETURN
C***
      ENDIF
C-----------------------------------------------------------------------
      ADDT=DT
      RDT=1./ADDT
C-----------------TIME TENDENCY-----------------------------------------
!$omp parallel do
      DO L=1,LM
        DO J=MYJS_P1,MYJE_P1
        DO I=MYIS_P1,MYIE_P1
          DWDT(I,J,L)=(W(I,J,L)-DWDT(I,J,L))*HTM(I,J,L)*HBM2(I,J)*RDT
        ENDDO
        ENDDO
      ENDDO
C
C-----------------------------------------------------------------------
C-----------------VERTICAL ADVECTION------------------------------------
C-----------------------------------------------------------------------
C
!$omp parallel do
!$omp& private (afr,afrp,arr,bot,d2pqw,detal,dwl,dwp,etadtl,
!$omp&          la,lap,llap,lmp,rfacw,rr,sumnw,sumpw,top,
!$omp&          w00,w3,w4,w4p,wp,wp0)
      DO 200 J=MYJS_P1,MYJE_P1
      DO 200 I=MYIS_P1,MYIE_P1
C-----------------------------------------------------------------------
      LMP=LMH(I,J)
C-----------------------------------------------------------------------
      DO L=1,LMP
        W3(L)=W(I,J,L)
        W4(L)=W3(L)
      ENDDO
C
      ETADTL(1)=ETADT(I,J,1)*0.5
C
      DO L=2,LMP-1
        ETADTL(L)=(ETADT(I,J,L-1)+ETADT(I,J,L))*0.5
      ENDDO
C
      ETADTL(LMP)=ETADT(I,J,LMP-1)*0.5
C-----------------------------------------------------------------------
      SUMPW=0.
      SUMNW=0.
C
      DO L=1,LMP
        RR=ETADTL(L)*(-ADDT)
        ARR=ABS(RR)
C
        IF(ARR.GT.0.)THEN
          LAP=RR/ARR
        ELSE
          LAP=0
        ENDIF
C
        LA(L)=LAP
        LLAP=L+LAP
C
        TOP=.FALSE.
        BOT=.FALSE.
C
        IF(LLAP.GT.0.AND.LLAP.LT.LMH(I,J)+1.AND.LAP.NE.0)THEN
          RR=ARR/ABS(AETA(LLAP)-AETA(L))
          AFR(L)=(((FF4*RR+FF3)*RR+FF2)*RR+FF1)*RR
          DWP=(W3(LLAP)-W3(L))*RR
          DWL(L)=DWP
        ELSE
          TOP=LLAP.EQ.0
          BOT=LLAP.EQ.LMH(I,J)+1
C
          RR=0.
          AFR(L)=0.
          DWL(L)=0.
        ENDIF
      ENDDO
C
      IF(TOP)THEN
        IF(LA(2).LT.0)THEN
          DWL(1)=-DWL(2)*DETA(2)/DETA(1)
        ENDIF
      ENDIF
C
      IF(BOT)THEN
        IF(LA(LMP-1).GT.0)THEN
          DWL(LMP)=-DWL(LMP-1)*DETA(LMP-1)/DETA(LMP)
        ENDIF
      ENDIF
C
      DO L=1,LMP
        DETAL=DETA(L)
        DWP=DWL(L)*DETAL
        IF(DWP.GT.0.)THEN
          SUMPW=SUMPW+DWP
        ELSE
          SUMNW=SUMNW+DWP
        ENDIF
      ENDDO
C--------------FIRST MOMENT CONSERVING FACTOR---------------------------
      IF(SUMPW.GT.1.E-9)THEN
        RFACW=-SUMNW/SUMPW
      ELSE
        RFACW=1.
      ENDIf
C
      IF(RFACW.LT.0.9.OR.RFACW.GT.1.1)RFACW=1.
C--------------IMPOSE CONSERVATION ON ADVECTION-------------------------
      IF(RFACW.LT.1.)THEN
        DO L=1,LMP
          DWP=DWL(L)
          IF(DWP.LT.0.)DWP=DWP/RFACW
          W4(L)=W3(L)+DWP
        ENDDO
      ELSE
        DO L=1,LMP
          DWP=DWL(L)
          IF(DWP.GE.0.)DWP=DWP*RFACW
          W4(L)=W3(L)+DWP
        ENDDO
      ENDIF
C--------------ANTI-FILTERING STEP--------------------------------------
      SUMPW=0.
      SUMNW=0.
C--------------ANTI-FILTERING LIMITERS----------------------------------
      DO 50 L=2,LMP-1
      DETAL=DETA(L)
C
      W4P=W4(L)
C
      LAP=LA(L)
C
      IF(LAP.NE.0)THEN
        AFRP=2.*AFR(L)*(AETA(L+LAP)-AETA(L))**2
     1                /(AETA(L+LAP)-AETA(L-LAP))
        D2PQW=((W4(L+LAP)-W4P)/(AETA(L+LAP)-AETA(L))
     1        -(W4P-W4(L-LAP))/(AETA(L)-AETA(L-LAP)))*AFRP
      ELSE
        D2PQW=0.
      ENDIF
C
      WP=W4P-D2PQW
C
      W00=W3(L)
      WP0=W3(L+LAP)
C
      WP=MAX(WP,MIN(W00,WP0))
      WP=MIN(WP,MAX(W00,WP0))
C
      DWP=WP-W4P
C
      DWL(L)=DWP
C
      DWP=DWP*DETAL
C
      IF(DWP.GT.0.)THEN
        SUMPW=SUMPW+DWP
      ELSE
        SUMNW=SUMNW+DWP
      ENDIF
C
   50 CONTINUE
C-----------------------------------------------------------------------
      DWL(1)=0.
C
      DWL(LMP)=0.
C--------------FIRST MOMENT CONSERVING FACTOR---------------------------
      IF(SUMPW.GT.1.E-9)THEN
        RFACW=-SUMNW/SUMPW
      ELSE
        RFACW=1.
      ENDIF
C
      IF(RFACW.LT.0.9.OR.RFACW.GT.1.1)RFACW=1.
C--------------IMPOSE CONSERVATION ON ANTI-FILTERING--------------------
      IF(RFACW.LT.1.)THEN
        DO L=1,LMP
          DWP=DWL(L)
          IF(DWP.GE.0.)DWP=DWP*RFACW
          DWDT(I,J,L)=DWDT(I,J,L)-(W4(L)+DWP-W3(L))*RDT
        ENDDO
      ELSE
        DO L=1,LMP
          DWP=DWL(L)
          IF(DWP.LT.0.)DWP=DWP/RFACW
          DWDT(I,J,L)=DWDT(I,J,L)-(W4(L)+DWP-W3(L))*RDT
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
  200 CONTINUE
C--------------END OF VERTICAL ADVECTION--------------------------------
C
C-----------------------------------------------------------------------
C***********************************************************************
      ENH=ADDT/(08.*DY)
C-----------------------------------------------------------------------
!$omp parallel do
      DO J=MYJS_P2,MYJE_P2
Cmp      DO I=MYIS_P1,MYIE_P1
      DO I=MYIS_P2,MYIE_P2
        EMH(I,J)=ADDT/(08.*DX(I,J))
        DARE(I,J)=HBM2(I,J)*DX(I,J)*DY
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
C---------------------HORIZONTAL ADVECTION------------------------------
C-----------------------------------------------------------------------
C***********************************************************************
!$omp parallel do
!$omp& private (dvolp,dwstij,hm,jfp,jfq,pp,qp,
!$omp&          sumnw,sumpw,tta,ttb)
      DO 300 L=1,LM
C***********************************************************************
      DO J=MYJS_P2,MYJE_P2
      DO I=MYIS_P2,MYIE_P2
        DVOL(I,J,L)=DARE(I,J)*PDSL(I,J)*DETA(L)
        HM=HTM(I,J,L)
        W1(I,J,L)=W(I,J,L)*HM
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
      SUMPW=0.
      SUMNW=0.
C
      DO 225 J=MYJS2_P2,MYJE2_P2
      DO 225 I=MYIS1_P2,MYIE1_P2
C
      DVOLP=DVOL(I,J,L)*HBM3(I,J)
      TTA=(U(I,J-1,L)+U(I+IHW(J),J,L)+U(I+IHE(J),J,L)+U(I,J+1,L))
     2   *HBM2(I,J)*EMH(I,J)
      TTB=(V(I,J-1,L)+V(I+IHW(J),J,L)+V(I+IHE(J),J,L)+V(I,J+1,L))
     2   *HBM2(I,J)*ENH
C
      PP=-TTA-TTB
      QP= TTA-TTB
C
      JFP=INT(SIGN(1.,PP))
      JFQ=INT(SIGN(1.,QP))
C
      IFPA(I,J,L)=IHE(J)+I+( JFP-1)/2
      IFQA(I,J,L)=IHE(J)+I+(-JFQ-1)/2
C
      JFPA(I,J,L)=J+JFP
      JFQA(I,J,L)=J+JFQ
C
      IFPF(I,J,L)=IHE(J)+I+(-JFP-1)/2
      IFQF(I,J,L)=IHE(J)+I+( JFQ-1)/2
C
      JFPF(I,J,L)=J-JFP
      JFQF(I,J,L)=J-JFQ
C
      PP=ABS(PP)*HTM(I,J,L)*HTM(IFPA(I,J,L),JFPA(I,J,L),L)
      QP=ABS(QP)*HTM(I,J,L)*HTM(IFQA(I,J,L),JFQA(I,J,L),L)
C
      AFP(I,J,L)=(((FF4*PP+FF3)*PP+FF2)*PP+FF1)*PP
      AFQ(I,J,L)=(((FF4*QP+FF3)*QP+FF2)*QP+FF1)*QP
C
      DWSTIJ=(W(IFPA(I,J,L),JFPA(I,J,L),L)-W(I,J,L))*PP
     2      +(W(IFQA(I,J,L),JFQA(I,J,L),L)-W(I,J,L))*QP
C
      DWST(I,J,L)=DWSTIJ
C
  225 CONTINUE
C***
C***  GLOBAL SUM FOR CONSERVATION
C***
      DO 230 J=MYJS2,MYJE2
      DO 230 I=MYIS1,MYIE1
C
      DVOLP=DVOL(I,J,L)*HBM3(I,J)
      DWSTIJ=DWST(I,J,L)*DVOLP
C
      IF(DWSTIJ.GT.0.)THEN
        SUMPW=SUMPW+DWSTIJ
      ELSE
        SUMNW=SUMNW+DWSTIJ
      ENDIF
C
  230 CONTINUE
C
C-----------------------------------------------------------------------
      XSUMS(1,L)=SUMPW
      XSUMS(2,L)=SUMNW
C
  300 CONTINUE          ! End of Vertical Loop
C-----------------------------------------------------------------------
C
C***  GLOBAL REDUCTION
C
	IF (NPES .eq. 1) IRECV=0
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,2*LM,MPI_REAL,MPI_SUM,
     1                     MPI_COMM_COMP,IRECV)
      IF(IRECV.NE.0)THEN
        PRINT*, ' RETURN CODE FROM 1st ALLREDUCE IN EPS = ',IRECV
        STOP
      ENDIF
C
C***  END OF GLOBAL REDUCTION
C
c     if(mype.eq.0)then
c       do l=1,lm
c         read(94)gsums(1,l)
c         read(94)gsums(2,l)
c       enddo
c     endif
c     call mpi_bcast(gsums,2*lm,mpi_real,0,MPI_COMM_COMP,irtn)
C-----------------------------------------------------------------------
!$omp  parallel do
!$omp& private(d2pqw,dvolp,dwstij,rfacw,rfwij,sumnw,sumpw,
!$omp&         w00,w0q,w1ij,wp0,wstij)
C-----------------------------------------------------------------------
      DO 400 L=1,LM
C-----------------------------------------------------------------------
C
      SUMPW=GSUMS(1,L)
      SUMNW=GSUMS(2,L)
C
C--------------FIRST MOMENT CONSERVING FACTOR---------------------------
      IF(SUMPW.GT.1.)THEN
        RFACW=-SUMNW/SUMPW
      ELSE
        RFACW=1.
      ENDIF
      IF(RFACW.LT.0.9.OR.RFACW.GT.1.1)RFACW=1.
C--------------IMPOSE CONSERVATION ON ADVECTION-------------------------
      IF(RFACW.LT.1.)THEN
        DO J=MYJS2_P2,MYJE2_P2
        DO I=MYIS1_P2,MYIE1_P2
          DWSTIJ=DWST(I,J,L)
          RFWIJ=HBM3(I,J)*(RFACW-1.)+1.
          IF(DWSTIJ.LT.0.)DWSTIJ=DWSTIJ/RFWIJ
          W1(I,J,L)=W(I,J,L)+DWSTIJ
        ENDDO
        ENDDO
      ELSE
        DO J=MYJS2_P2,MYJE2_P2
        DO I=MYIS1_P2,MYIE1_P2
          DWSTIJ=DWST(I,J,L)
          RFWIJ=HBM3(I,J)*(RFACW-1.)+1.
          IF(DWSTIJ.GE.0.)DWSTIJ=DWSTIJ*RFWIJ
          W1(I,J,L)=W(I,J,L)+DWSTIJ
        ENDDO
        ENDDO
      ENDIF
C--------------ANTI-FILTERING STEP--------------------------------------
      SUMPW=0.
      SUMNW=0.
C--------------ANTI-FILTERING LIMITERS----------------------------------
C
      DO 350 J=MYJS2_P1,MYJE2_P1
      DO 350 I=MYIS1_P1,MYIE1_P1
C
      DVOLP=DVOL(I,J,L)
      W1IJ =W1(I,J,L)
C
      D2PQW=((W1(IFPA(I,J,L),JFPA(I,J,L),L)-W1IJ               )
     2      -(W1IJ                   -W1(IFPF(I,J,L),JFPF(I,J,L),L))
     3      *HTM(IFPF(I,J,L),JFPF(I,J,L),L))*AFP(I,J,L)
     4     +((W1(IFQA(I,J,L),JFQA(I,J,L),L)-W1IJ               )
     5      -(W1IJ                   -W1(IFQF(I,J,L),JFQF(I,J,L),L))
     6      *HTM(IFQF(I,J,L),JFQF(I,J,L),L))*AFQ(I,J,L)
C
      WSTIJ=W1IJ-D2PQW
C
      W00=W(I        ,J        ,L)
      WP0=W(IFPA(I,J,L),JFPA(I,J,L),L)
      W0Q=W(IFQA(I,J,L),JFQA(I,J,L),L)
C
      WSTIJ=AMAX1(WSTIJ,AMIN1(W00,WP0,W0Q))
      WSTIJ=AMIN1(WSTIJ,AMAX1(W00,WP0,W0Q))
C
      DWSTIJ=WSTIJ-W1IJ
C
      DWST(I,J,L)=DWSTIJ
C
      DWSTIJ=DWSTIJ*DVOLP
C
      IF(DWSTIJ.GT.0.)THEN
        SUMPW=SUMPW+DWSTIJ
      ELSE
        SUMNW=SUMNW+DWSTIJ
      ENDIF
C
  350 CONTINUE
C-----------------------------------------------------------------------
      XSUMS(1,L)=SUMPW
      XSUMS(2,L)=SUMNW
C
  400 CONTINUE            ! End of Vertical Loop
C-----------------------------------------------------------------------
C
C***  GLOBAL REDUCTION
C
	if (NPES .eq. 1) IRECV=0
      CALL MPI_ALLREDUCE(XSUMS,GSUMS,2*LM,MPI_REAL,MPI_SUM,
     1                     MPI_COMM_COMP,IRECV)
      IF(IRECV.NE.0)THEN
         PRINT*, ' RETURN CODE FROM 2nd ALLREDUCE IN EPS = ',IRECV
         STOP
      ENDIF
C
C***  END OF GLOBAL REDUCTION
C
c     if(mype.eq.0)then
c       do l=1,lm
c         read(95)gsums(1,l)
c         read(95)gsums(2,l)
c       enddo
c     endif
c     call mpi_bcast(gsums,2*lm,mpi_real,0,MPI_COMM_COMP,irtn)
C-----------------------------------------------------------------------
C
!$omp  parallel do
!$omp& private(dwstij,rfacw,rfwij,sumnw,sumpw)
C-----------------------------------------------------------------------
      DO 425 L=1,LM
C
      SUMPW=GSUMS(1,L)
      SUMNW=GSUMS(2,L)
C
C--------------FIRST MOMENT CONSERVING FACTOR---------------------------
      IF(SUMPW.GT.1.)THEN
        RFACW=-SUMNW/SUMPW
      ELSE
        RFACW=1.
      ENDIF
      IF(RFACW.LT.0.9.OR.RFACW.GT.1.1)RFACW=1.
C--------------IMPOSE CONSERVATION ON ANTI-FILTERING--------------------
      IF(RFACW.LT.1.)THEN
        DO J=MYJS2_P2,MYJE2_P2
        DO I=MYIS1_P2,MYIE1_P2
          DWSTIJ=DWST(I,J,L)
          RFWIJ=HBM2(I,J)*(RFACW-1.)+1.
          IF(DWSTIJ.GE.0.)DWSTIJ=DWSTIJ*RFWIJ
          DWDT(I,J,L)=(DWDT(I,J,L)-(W1(I,J,L)+DWSTIJ-W(I,J,L))*RDT)
     1                *HBM3(I,J)
        ENDDO
        ENDDO
      ELSE
        DO J=MYJS2_P2,MYJE2_P2
        DO I=MYIS1_P2,MYIE1_P2
          DWSTIJ=DWST(I,J,L)
          RFWIJ=HBM2(I,J)*(RFACW-1.)+1.
          IF(DWSTIJ.LT.0.)DWSTIJ=DWSTIJ/RFWIJ
          DWDT(I,J,L)=(DWDT(I,J,L)-(W1(I,J,L)+DWSTIJ-W(I,J,L))*RDT)
     1                *HBM3(I,J)
        ENDDO
        ENDDO
      ENDIF
C-----------------------------------------------------------------------
C***********************************************************************
  425 CONTINUE
C***********************************************************************
C
C--------------RESTRICTING THE ACCELERATION ALONG THE BOUNDARIES--------
C
c     JHL=07
      JHL=0
C***
      IF(JHL.GT.1)THEN
        JHH=JM-JHL+1
C
        IHL=JHL/2+1
        IHH=IM-IHL+MOD(J,2)
C-----------------------------------------------------------------------
!$omp parallel do private (ix,jx)
        DO 450 L=1,LM
C
        DO J=1,JHL
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J-MY_JS_GLB+1
            DO I=1,IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I-MY_IS_GLB+1
                DWDT(IX,JX,L)=MAX(DWDT(IX,JX,L)*HBM3(IX,JX),-0.03)
                DWDT(IX,JX,L)=MIN(DWDT(IX,JX,L)*HBM3(IX,JX), 0.03)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        DO J=JHH,JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J-MY_JS_GLB+1
            DO I=1,IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I-MY_IS_GLB+1
                DWDT(IX,JX,L)=MAX(DWDT(IX,JX,L)*HBM3(IX,JX),-0.03)
                DWDT(IX,JX,L)=MIN(DWDT(IX,JX,L)*HBM3(IX,JX), 0.03)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        DO J=1,JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J-MY_JS_GLB+1
            DO I=1,IHL
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I-MY_IS_GLB+1
                DWDT(IX,JX,L)=MAX(DWDT(IX,JX,L)*HBM3(IX,JX),-0.03)
                DWDT(IX,JX,L)=MIN(DWDT(IX,JX,L)*HBM3(IX,JX), 0.03)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
        DO J=1,JM
          IF(J.GE.MY_JS_GLB-JBPAD2.AND.J.LE.MY_JE_GLB+JTPAD2)THEN
            JX=J-MY_JS_GLB+1
            DO I=IHH,IM
              IF(I.GE.MY_IS_GLB-ILPAD2.AND.I.LE.MY_IE_GLB+IRPAD2)THEN
                IX=I-MY_IS_GLB+1
                DWDT(IX,JX,L)=MAX(DWDT(IX,JX,L)*HBM3(IX,JX),-0.03)
                DWDT(IX,JX,L)=MIN(DWDT(IX,JX,L)*HBM3(IX,JX), 0.03)
              ENDIF
            ENDDO
          ENDIF
        ENDDO
C
  450   CONTINUE
      ENDIF
C------------------------------------------------------------------------
      WA=0.15
C
!$omp parallel do private (ane,ase)
      DO 500 L=1,LM
C
      DO J=MYJS1_P1,MYJE2_P1
      DO I=MYIS_P1,MYIE1_P1
        ANE(I,J)=(DWDT(I+IHE(J),J+1,L)-DWDT(I,J,L))
     &           *HTM(I,J,L)*HTM(I+IHE(J),J+1,L)
      ENDDO
      ENDDO
C
      DO J=MYJS2_P1,MYJE1_P1
      DO I=MYIS_P1,MYIE1_P1
        ASE(I,J)=(DWDT(I+IHE(J),J-1,L)-DWDT(I,J,L))
     &           *HTM(I+IHE(J),J-1,L)*HTM(I,J,L)
      ENDDO
      ENDDO
C
      DO J=MYJS2,MYJE2
      DO I=MYIS1,MYIE1
        DWDT(I,J,L)=(ANE(I,J)-ANE(I+IHW(J),J-1)
     1              +ASE(I,J)-ASE(I+IHW(J),J+1))*WA*HBM2(I,J)
     2              +DWDT(I,J,L)
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
  500 CONTINUE
C-----------------------------------------------------------------------
      WP=0.075
C
      KN=0
      KP=0
      DWDTMX=0.
      DWDTMN=0.
C-----------------------------------------------------------------------
!$omp parallel do
!$omp& private (dwdtmn,dwdtmx,dwdtt,hm,kn,kp)
      DO 525 L=1,LM
C-----------------------------------------------------------------------
      DO J=MYJS,MYJE
      DO I=MYIS,MYIE
        HM=HTM(I,J,L)*HBM2(I,J)
        DWDTT=DWDT(I,J,L)*HM
C
        DWDTMX=AMAX1(DWDTT,DWDTMX)
        DWDTMN=AMIN1(DWDTT,DWDTMN)
C
        IF(DWDTT.LT.EPSN)THEN
          DWDTT=EPSN
          KN=KN+1
        ENDIF
C
        IF(DWDTT.GT.EPSP)THEN
          DWDTT=EPSP
          KP=KP+1
        ENDIF
C
        DWDT(I,J,L)=(DWDTT/G+1.)*(1.-WP)+PDWDT(I,J,L)*WP
      ENDDO
      ENDDO
C-----------------------------------------------------------------------
  525 CONTINUE
C-----------------------------------------------------------------------
      GDT=G*DT
      GDT2=GDT*GDT
      WGHT=0.35
      RLX=0.90
      FCC=GMA/GDT2
C
C-----------------------------------------------------------------------
!$omp parallel do
!$omp& private (chi,chin,chmod,clim,coff,delp,dfrc,dp,dpstr,
!$omp&          dptl,dptu,dwdtt,hbm2ij,imax,inot,iter,itmx,jmax,
!$omp&          l,lmp,pdp,pnp1,pone,pp1,pstr,rdp,resdl,wil,wres)
C-----------------------------------------------------------------------
      DO 600 J=MYJS2,MYJE2
      ITMX=0
      DO 600 I=MYIS1,MYIE1
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C
      LMP=LMH(I,J)
      PDP=PDSL(I,J)
      RPD=1./PDP
C-----------------------------------------------------------------------
      PONE(1)=PT
      PSTR(1)=PT
      PNP1(1)=PT
C
      DO L=2,LMP+1
        PONE(L)=PINT(I,J,L)
        DPSTR=DWDT(I,J,L-1)*DETA(L-1)*PDP
        PSTR(L)=PSTR(L-1)+DPSTR
        PP1=PNP1(L-1)+DPSTR
        DP=(PP1-PONE(L))*WGHT
        PNP1(L)=PONE(L)+DP
      ENDDO
C
      DO L=1,LMP+1
        PONE(L)=PONE(L)*RPD
        PSTR(L)=PSTR(L)*RPD
        PNP1(L)=PNP1(L)*RPD
        CHI(L)=PNP1(L)-PSTR(L)
      ENDDO
C
      DO L=2,LMP+1
        COFF(L-1)=T(I,J,L-1)*DETA(L-1)*FCC/((PNP1(L-1)+PNP1(L))*0.5)**2
      ENDDO
C
      DO L=2,LMP
        DFRC(L)=((PSTR(L-1)+PSTR(L)-PONE(L-1)-PONE(L))*COFF(L-1)
     &          +(PSTR(L)+PSTR(L+1)-PONE(L)-PONE(L+1))*COFF(L))*0.5
        WRES(L)=RLX/((COFF(L-1)+COFF(L))*0.5-1./DETA(L)-1./DETA(L-1))
        CLIM(L)=0.1*ETA(L)*RPD
      ENDDO
C-----------------------------------------------------------------------
      DO 575 ITER=1,1000
C-----------------------------------------------------------------------
      INOT=0
C
      DO L=2,LMP
        RESDL=((CHI(L-1)+CHI(L))*COFF(L-1)
     &        +(CHI(L)+CHI(L+1))*COFF(L))*0.5
     &        +(CHI(L+1)-CHI(L))/DETA(L)-(CHI(L)-CHI(L-1))/DETA(L-1)
     &        +DFRC(L)
        CHMOD=-RESDL*WRES(L)
        CHIN(L)=CHI(L)+CHMOD
        IF(ABS(CHMOD).GT.CLIM(L))INOT=1
      ENDDO
C
      IF(INOT.EQ.0)GO TO 580
C
      CHIN(LMP+1)=CHIN(LMP)
C
      DO L=2,LMP+1
        CHI(L)=CHIN(L)
      ENDDO
C-----------------------------------------------------------------------
  575 CONTINUE    ! End of Iteration Loop
C-----------------------------------------------------------------------
  580 IF(ITER.GT.ITMX)THEN
        ITMX=ITER
        IMAX=I
        JMAX=J
      ENDIF
C-----------------------------------------------------------------------
C
C***  UNSCALE SOLUTION
C
      DO L=1,LMP+1
        PNP1(L)=(CHI(L)+PSTR(L))*PDP
        PONE(L)=PONE(L)*PDP
        PINT(I,J,L)=PNP1(L)
      ENDDO
C
      DPTU=0.
C
      HBM2IJ=HBM2(I,J)
C
      DO L=1,LMP
        DPTL=PNP1(L+1)-PONE(L+1)
        T(I,J,L)=(DPTU+DPTL)*RTOP(I,J,L)*0.5/CP*HBM2IJ+T(I,J,L)
        DELP=(PNP1(L+1)-PNP1(L))/(PDP*DETA(L))
        DWDTT=DWDT(I,J,L)
        WIL=W(I,J,L)+(DELP-DWDTT)*GDT
        DWDT(I,J,L)=DWDTT+(WIL-W(I,J,L))/GDT*HBM2IJ
        W(I,J,L)=WIL*HBM2IJ
C
        DPTU=DPTL
      ENDDO

C-----------------------------------------------------------------------
  600 CONTINUE
C-----------------------------------------------------------------------
C	write(6,*) 'for PE: ', MYPE, 'DWDT extrema: ', DWDTMN,DWDTMX
                             RETURN
                             END
