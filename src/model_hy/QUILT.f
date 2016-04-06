                  SUBROUTINE QUILT
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  QUILT     I/O SERVERS
C   PRGRMMR: TUCCILLO        ORG:  IBM       DATE: 00-01-20
C
C ABSTRACT:  I/O SERVERS
C
C PROGRAM HISTORY LOG:
C   00-01-20  TUCCILLO - ORIGINATOR
C
C USAGE:  CALL QUILT
C
C   INPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   INPUT FILES:  NONE
C
C   OUTPUT FILES:  NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C            MPI_RECV
C            MPI_BCAST
C            COLLECT
C            SLP
C            DECOAL
C
C   EXIT STATES:
C     COND =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C
C$$$
C
C     THIS CODE ASSUMES THAT NSOIL IS GE TO 4. IF THIS IS NOT TRUE,
C     THE CODE WILL STOP. THE EQUIVALENCING IS THE PROBLEM.
C
C-----------------------------------------------------------------------
      INCLUDE "parmeta"
      INCLUDE "parmsoil"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INCLUDE "PARA.comm"
      INCLUDE "BUFFER.comm"
C-----------------------------------------------------------------------
                              P A R A M E T E R
     & (LB=2*IM+JM-3)
C-----------------------------------------------------------------------
C
       REAL DUM1(IM,JM),DUM2(IM,JM),DUM3(IM,JM),DUM4(IM,JM)
       REAL DUM5(IM,JM),DUM6(IM,JM),DUM7(IM,JM)
       REAL DUMS(IM,JM,NSOIL)
       INTEGER STATUS(MPI_STATUS_SIZE)
       EQUIVALENCE ( DUM1(1,1), DUMS(1,1,1) )
       EQUIVALENCE ( DUM2(1,1), DUMS(1,1,2) )
       EQUIVALENCE ( DUM3(1,1), DUMS(1,1,3) )
       EQUIVALENCE ( DUM4(1,1), DUMS(1,1,4) )
C
C-----------------------------------------------------------------------
      REAL, ALLOCATABLE ::
     & PDOMG(:,:),RESOMG(:,:),PD(:,:),RES(:,:),FIS(:,:)
     &,RSWIN(:,:),RSWOUT(:,:),TG(:,:),Z0(:,:),AKMS(:,:)
     &,CZEN(:,:),AKHS(:,:),THS(:,:),QS(:,:),TWBS(:,:)
     &,QWBS(:,:),HBOT(:,:),CFRACL(:,:),THZ0(:,:),QZ0(:,:)
     &,UZ0(:,:),VZ0(:,:),USTAR(:,:),HTOP(:,:),CFRACM(:,:)
     &,SNO(:,:),WET(:,:),CLDEFI(:,:),RF(:,:),PSLP(:,:)
     &,CUPPT(:,:),CFRACH(:,:),SOILTB(:,:),SFCEXC(:,:)
     &,SMSTAV(:,:),SMSTOT(:,:),GRNFLX(:,:),PCTSNO(:,:)
     &,RLWIN(:,:),RADOT(:,:),CZMEAN(:,:),SIGT4(:,:)
     &,U00(:,:),SR(:,:),PREC(:,:),ACPREC(:,:),ACCLIQ(:,:)
     &,CUPREC(:,:),ACFRCV(:,:),ACFRST(:,:),SFCSHX(:,:)
     &,ACSNOW(:,:),ACSNOM(:,:),SSROFF(:,:),BGROFF(:,:)
     &,SFCLHX(:,:),SUBSHX(:,:),SNOPCX(:,:),SFCUVX(:,:)
     &,SFCEVP(:,:),POTEVP(:,:),ASWIN(:,:),ASWOUT(:,:)
     &,ASWTOA(:,:),ALWIN(:,:),ALWOUT(:,:),ALWTOA(:,:)
     &,TH10(:,:),Q10(:,:),U10(:,:),V10(:,:),TSHLTR(:,:)
     &,QSHLTR(:,:),PSHLTR(:,:),CMC(:,:),POTFLX(:,:)
     &,TLMIN(:,:),TLMAX(:,:)
C
      REAL UL(2*LM)
C
      REAL, ALLOCATABLE ::
     & OMGALF(:,:,:),T(:,:,:),Q(:,:,:),U(:,:,:)
     &,V(:,:,:),Q2(:,:,:),TTND(:,:,:),CWM(:,:,:)
     &,TRAIN(:,:,:),TCUCN(:,:,:)
     &,RSWTT(:,:,:),RLWTT(:,:,:)
C
      REAL, ALLOCATABLE ::
     & SMC(:,:,:),STC(:,:,:)
                              R E A L
     & PDB(LB,2),TB(LB,LM,2),QB(LB,LM,2),UB(LB,LM,2),VB(LB,LM,2)
C
C-----------------------------------------------------------------------
      INTEGER IDAT(3)
C
      INTEGER, ALLOCATABLE ::
     & LC(:,:),NCFRCV(:,:),NCFRST(:,:)
C-----------------------------------------------------------------------
                              L O G I C A L
     & RUN,FIRST,HYDRO,SIGMA
C-----------------------------------------------------------------------
                              C H A R A C T E R
     & RSTFIL1*50,RSTFIL2*50,RESTHR*4,LABEL*32
     &,FNAME*80,ENVAR*50,BLANK*4
      CHARACTER FINFIL*50,DONE*10
C
       LOGICAL LME
C-----------------------------------------------------------------------
      DATA LRSTRT1/21/,LRSTRT2/61/,NHB/12/,BLANK/'    '/
C-----------------------------------------------------------------------
C
      real*8 timef, ist, isp, rtc
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
      CALL MPI_FIRST
C
      IF(NSOIL.LT.4)THEN
        print *, ' NSOIL IS LESS THAN 4. CHANGE THE EQUIVALENCES'
        print *, ' STOPPING'
        stop
      ENDIF
C
      IF(ME.EQ.0)THEN
        LME=.TRUE.
      ELSE
        LME=.FALSE.
      ENDIF
C
      btim=timef()
C
      ALLOCATE(PDOMG(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(RESOMG(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(OMGALF(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(PD(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(RES(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(FIS(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(T(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(Q(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(U(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(V(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(Q2(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(TTND(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(CWM(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(TRAIN(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(TCUCN(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(RSWIN(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(RSWOUT(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(TG(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(Z0(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(AKMS(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CZEN(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(AKHS(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(THS(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(QS(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(TWBS(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(QWBS(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(HBOT(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CFRACL(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(THZ0(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(QZ0(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(UZ0(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(VZ0(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(USTAR(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(HTOP(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SNO(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(WET(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CLDEFI(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(RF(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(PSLP(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CUPPT(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CFRACH(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CFRACM(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SOILTB(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SFCEXC(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SMSTAV(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SMSTOT(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(GRNFLX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(PCTSNO(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(RLWIN(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(RADOT(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CZMEAN(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SIGT4(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(U00(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(LC(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SR(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(PREC(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ACPREC(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ACCLIQ(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(CUPREC(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ACFRCV(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(NCFRCV(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ACFRST(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(NCFRST(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ACSNOW(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ACSNOM(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SSROFF(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(BGROFF(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SFCSHX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SFCLHX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SUBSHX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SNOPCX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SFCUVX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SFCEVP(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(POTEVP(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ASWIN(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ASWOUT(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ASWTOA(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ALWIN(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ALWOUT(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(ALWTOA(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(TH10(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(Q10(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(U10(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(V10(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(TSHLTR(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(QSHLTR(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(PSHLTR(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(SMC(MY_ISD:MY_IED,MY_JSD:MY_JED,1:NSOIL))
      ALLOCATE(CMC(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(STC(MY_ISD:MY_IED,MY_JSD:MY_JED,1:NSOIL))
      ALLOCATE(POTFLX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(TLMIN(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(TLMAX(MY_ISD:MY_IED,MY_JSD:MY_JED))
      ALLOCATE(RSWTT(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
      ALLOCATE(RLWTT(MY_ISD:MY_IED,MY_JSD:MY_JED,1:LM))
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
C***
C***  LOOP OVER ALL THE OUTPUT TIMES
C***
C-----------------------------------------------------------------------
666   continue
      if ( me .eq. 0 ) then
      call mpi_recv(ihour,1,MPI_INTEGER,0,0,mpi_comm_inter,status,ier)
      print *, ' ihour in quilt = ',ihour
      end if
      call mpi_bcast(ihour,1,MPI_INTEGER,0,mpi_comm_comp,ier)
      if ( ihour .eq. -999 ) goto 667
C      ist = rtc()
C-----------------------------------------------------------------------
      DO IXXX = 1, JEND(ME) - JSTA(ME) + 1
C-----------------------------------------------------------------------
C***
      call mpi_recv(buf,ibufmax,MPI_REAL,MPI_ANY_SOURCE,ihour,
     *              mpi_comm_inter, status,ier)
      IPE = status(MPI_SOURCE)
      if ( ier .ne. 0 ) then
         print *, ' error from mpi_rec = ',ier
      end if
      is = MY_IS_GLB_A(IPE)
      ie = MY_IE_GLB_A(IPE)
      js = MY_JS_GLB_A(IPE)
      je = MY_JE_GLB_A(IPE)
      len_ch = (ie-is+1) * (je-js+1)
C     EXTRACT RECORD LENGTH - LETS KEEP THIS IN BECAUSE IT IS POTENTIALLY HANDY
      call decoal(idum,-1)
C
      call decoal(RUN,1)
      call decoal(IDAT,3)
      call decoal(IHRST,1)
      call decoal(NTSD,1)
      call decoal(LABEL,8)
      call decoal(pdomg(is:ie,js:je),len_ch)
      call decoal(resomg(is:ie,js:je),len_ch)
      do l=1,lm
         call decoal(omgalf(is:ie,js:je,l),len_ch)
      enddo
      call decoal(RUN,1)
      call decoal(IDAT,3)
      call decoal(IHRST,1)
      call decoal(NTSD,1)
      call decoal(LABEL,8)
      call decoal(FIRST,1)
      call decoal(IOUT,1)
      call decoal(NSHDE,1)
      call decoal(pd(is:ie,js:je),len_ch)
      call decoal(res(is:ie,js:je),len_ch)
      call decoal(fis(is:ie,js:je),len_ch)
      do l=1,lm
         call decoal(t(is:ie,js:je,l),len_ch)
         call decoal(q(is:ie,js:je,l),len_ch)
         call decoal(u(is:ie,js:je,l),len_ch)
         call decoal(v(is:ie,js:je,l),len_ch)
         call decoal(q2(is:ie,js:je,l),len_ch)
         call decoal(ttnd(is:ie,js:je,l),len_ch)
         call decoal(cwm(is:ie,js:je,l),len_ch)
         call decoal(train(is:ie,js:je,l),len_ch)
         call decoal(tcucn(is:ie,js:je,l),len_ch)
      enddo
      call decoal(RUN,1)
      call decoal(IDAT,3)
      call decoal(IHRST,1)
      call decoal(NTSD,1)
      call decoal(LABEL,8)
      call decoal(rswin(is:ie,js:je),len_ch)
      call decoal(rswout(is:ie,js:je),len_ch)
      call decoal(tg(is:ie,js:je),len_ch)
      call decoal(z0(is:ie,js:je),len_ch)
      call decoal(akms(is:ie,js:je),len_ch)
      call decoal(czen(is:ie,js:je),len_ch)
      call decoal(akhs(is:ie,js:je),len_ch)
      call decoal(ths(is:ie,js:je),len_ch)
      call decoal(qs(is:ie,js:je),len_ch)
      call decoal(twbs(is:ie,js:je),len_ch)
      call decoal(qwbs(is:ie,js:je),len_ch)
      call decoal(hbot(is:ie,js:je),len_ch)
      call decoal(cfracl(is:ie,js:je),len_ch)
      call decoal(thz0(is:ie,js:je),len_ch)
      call decoal(qz0(is:ie,js:je),len_ch)
      call decoal(uz0(is:ie,js:je),len_ch)
      call decoal(vz0(is:ie,js:je),len_ch)
      call decoal(ustar(is:ie,js:je),len_ch)
      call decoal(htop(is:ie,js:je),len_ch)
      call decoal(cfracm(is:ie,js:je),len_ch)
      call decoal(sno(is:ie,js:je),len_ch)
      call decoal(wet(is:ie,js:je),len_ch)
      call decoal(cldefi(is:ie,js:je),len_ch)
      call decoal(rf(is:ie,js:je),len_ch)
      call decoal(pslp(is:ie,js:je),len_ch)
      call decoal(cuppt(is:ie,js:je),len_ch)
      call decoal(cfrach(is:ie,js:je),len_ch)
      call decoal(soiltb(is:ie,js:je),len_ch)
      call decoal(sfcexc(is:ie,js:je),len_ch)
      call decoal(smstav(is:ie,js:je),len_ch)
      call decoal(smstot(is:ie,js:je),len_ch)
      call decoal(grnflx(is:ie,js:je),len_ch)
      call decoal(pctsno(is:ie,js:je),len_ch)
      call decoal(rlwin(is:ie,js:je),len_ch)
      call decoal(radot(is:ie,js:je),len_ch)
      call decoal(czmean(is:ie,js:je),len_ch)
      call decoal(sigt4(is:ie,js:je),len_ch)
      call decoal(u00(is:ie,js:je),len_ch)
      call decoal(ul,2*lm)
      call decoal(lc(is:ie,js:je),len_ch)
      call decoal(sr(is:ie,js:je),len_ch)
      call decoal(RUN,1)
      call decoal(IDAT,3)
      call decoal(IHRST,1)
      call decoal(NTSD,1)
      call decoal(LABEL,8)
      call decoal(prec(is:ie,js:je),len_ch)
      call decoal(acprec(is:ie,js:je),len_ch)
      call decoal(accliq(is:ie,js:je),len_ch)
      call decoal(cuprec(is:ie,js:je),len_ch)
      call decoal(acfrcv(is:ie,js:je),len_ch)
      call decoal(ncfrcv(is:ie,js:je),len_ch)
      call decoal(acfrst(is:ie,js:je),len_ch)
      call decoal(ncfrst(is:ie,js:je),len_ch)
      call decoal(acsnow(is:ie,js:je),len_ch)
      call decoal(acsnom(is:ie,js:je),len_ch)
      call decoal(ssroff(is:ie,js:je),len_ch)
      call decoal(bgroff(is:ie,js:je),len_ch)
      call decoal(SFCSHX(is:ie,js:je),len_ch)
      call decoal(SFCLHX(is:ie,js:je),len_ch)
      call decoal(SUBSHX(is:ie,js:je),len_ch)
      call decoal(SNOPCX(is:ie,js:je),len_ch)
      call decoal(SFCUVX(is:ie,js:je),len_ch)
      call decoal(SFCEVP(is:ie,js:je),len_ch)
      call decoal(POTEVP(is:ie,js:je),len_ch)
      call decoal(ASWIN(is:ie,js:je),len_ch)
      call decoal(ASWOUT(is:ie,js:je),len_ch)
      call decoal(ASWTOA(is:ie,js:je),len_ch)
      call decoal(ALWIN(is:ie,js:je),len_ch)
      call decoal(ALWOUT(is:ie,js:je),len_ch)
      call decoal(ALWTOA(is:ie,js:je),len_ch)
      call decoal(ARDSW,1)
      call decoal(ARDLW,1)
      call decoal(ASRFC,1)
      call decoal(AVRAIN,1)
      call decoal(AVCNVC,1)
      call decoal(TH10(is:ie,js:je),len_ch)
      call decoal(Q10(is:ie,js:je),len_ch)
      call decoal(U10(is:ie,js:je),len_ch)
      call decoal(V10(is:ie,js:je),len_ch)
      call decoal(TSHLTR(is:ie,js:je),len_ch)
      call decoal(QSHLTR(is:ie,js:je),len_ch)
      call decoal(PSHLTR(is:ie,js:je),len_ch)
      call decoal(SMC(is:ie,js:je,1:nsoil),len_ch*nsoil)
      call decoal(CMC(is:ie,js:je),len_ch)
      call decoal(STC(is:ie,js:je,1:nsoil),len_ch*nsoil)
      call decoal(POTFLX(is:ie,js:je),len_ch)
      call decoal(TLMIN(is:ie,js:je),len_ch)
      call decoal(TLMAX(is:ie,js:je),len_ch)
      call decoal(ACUTIM,1)
      call decoal(ARATIM,1)
      call decoal(APHTIM,1)
      call decoal(NHEAT,1)
      call decoal(NPHS,1)
      call decoal(NCNVC,1)
      call decoal(NPREC,1)
      call decoal(NRDSW,1)
      call decoal(NRDLW,1)
      call decoal(NSRFC,1)
      call decoal(TPH0D,1)
      call decoal(TLM0D,1)
      call decoal(RESTRT,1)
      do l=1,lm
         call decoal(RSWTT(is:ie,js:je,l),len_ch)
         call decoal(RLWTT(is:ie,js:je,l),len_ch)
      enddo
      end do
C      isp = rtc()
C      print *, ' TIME FOR RECV/ASSEMBLY = ',isp-ist
C-----------------------------------------------------------------------
C***
C*** BEFORE WRITING OUT THE RESTRT FILE, COMPUTE THE MSLP
C***
C
C      ist = rtc()
      CALL SLP(NHB,PD,RES,FIS,T,Q,NTSD,PSLP)
C      isp = rtc()
C      print *, ' time for SLP = ',isp-ist
C
C-----------------------------------------------------------------------
C***  WRITE OUT THE GLOBAL FILE.
C-----------------------------------------------------------------------
C***
C***  GENERATE THE NAME OF THE GLOBAL OUTPUT RESTRT FILE
C***
      ENVAR=' '
      CALL GETENV("RSTFNL",ENVAR)
      CALL GETENV("tmmark",RESTHR)
      KPATH = INDEX(ENVAR,' ') -1
      IF(KPATH.LE.0) KPATH = LEN(ENVAR)
C
      IF(RESTHR.EQ.'    ')THEN
        WRITE(RSTFIL2,280)IHOUR
  280   FORMAT('restrt',I2.2)
      ELSE
        WRITE(RSTFIL2,285)IHOUR,RESTHR
  285   FORMAT('restrt',I2.2,'.',a4)
      ENDIF
C
      KRST = INDEX(RSTFIL2,' ') -1
      IF(KRST.LE.0) KRST = LEN(RSTFIL2)
C***
C***  OPEN UNIT TO THE GLOBAL RESTART FILE
C***
      CLOSE(LRSTRT2)
C
C      ist = rtc()
      IF(ENVAR(1:4).EQ.BLANK) THEN
       OPEN(UNIT=LRSTRT2,FILE=RSTFIL2,FORM='UNFORMATTED',IOSTAT=IER)
      ELSE
       FNAME = ENVAR(1:KPATH)  
       OPEN(UNIT=LRSTRT2,FILE=FNAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
C-----------------------------------------------------------------------
      IF ( LME ) WRITE(LRSTRT2)RUN,IDAT,IHRST,NTSD,LABEL
      CALL COLLECT(PDOMG,DUM1)
      CALL COLLECT(RESOMG,DUM2)
      IF ( LME ) WRITE(LRSTRT2) DUM1, DUM2
      DO L=1,LM
        CALL COLLECT(OMGALF(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
      ENDDO
      IF ( LME ) WRITE(LRSTRT2)RUN,IDAT,IHRST,NTSD,LABEL,
     1              FIRST,IOUT,NSHDE
      CALL COLLECT(PD,DUM1)
      CALL COLLECT(RES,DUM2)
      CALL COLLECT(FIS,DUM3)
      IF ( LME ) WRITE(LRSTRT2) DUM1, DUM2, DUM3
      IF ( LME ) WRITE(LRSTRT2)PDB,TB,QB,UB,VB
      DO L=1,LM
        CALL COLLECT(T(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(Q(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(U(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(V(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(Q2(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(TTND(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(CWM(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(TRAIN(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(TCUCN(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
      ENDDO
      CALL COLLECT(RSWIN,DUM1)
      CALL COLLECT(RSWOUT,DUM2)
      CALL COLLECT(TG,DUM3)
      CALL COLLECT(Z0,DUM4)
      CALL COLLECT(AKMS,DUM5)
      CALL COLLECT(CZEN,DUM6)
      IF ( LME ) WRITE(LRSTRT2)RUN,IDAT,IHRST,NTSD,LABEL
     1,           DUM1,DUM2,DUM3,DUM4,DUM5,DUM6
      CALL COLLECT(AKHS,DUM1)
      CALL COLLECT(THS,DUM2)
      CALL COLLECT(QS,DUM3)
      CALL COLLECT(TWBS,DUM4)
      CALL COLLECT(QWBS,DUM5)
      CALL COLLECT(HBOT,DUM6)
      CALL COLLECT(CFRACL,DUM7)
      IF ( LME ) WRITE(LRSTRT2)DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,DUM7
      CALL COLLECT(THZ0,DUM1)
      CALL COLLECT(QZ0,DUM2)
      CALL COLLECT(UZ0,DUM3)
      CALL COLLECT(VZ0,DUM4)
      CALL COLLECT(USTAR,DUM5)
      CALL COLLECT(HTOP,DUM6)
      CALL COLLECT(CFRACM,DUM7)
      IF ( LME ) WRITE(LRSTRT2)DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,DUM7
      CALL COLLECT(SNO,DUM1)
      CALL COLLECT(WET,DUM2)
      CALL COLLECT(CLDEFI,DUM3)
      CALL COLLECT(RF,DUM4)
      CALL COLLECT(PSLP,DUM5)
      CALL COLLECT(CUPPT,DUM6)
      CALL COLLECT(CFRACH,DUM7)
      IF ( LME ) WRITE(LRSTRT2) DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,DUM7
      CALL COLLECT(SOILTB,DUM1)
      CALL COLLECT(SFCEXC,DUM2)
      CALL COLLECT(SMSTAV,DUM3)
      CALL COLLECT(SMSTOT,DUM4)
      CALL COLLECT(GRNFLX,DUM5)
      CALL COLLECT(PCTSNO,DUM6)
      IF ( LME )WRITE(LRSTRT2) DUM1,DUM2,DUM3,DUM4,DUM5,DUM6
      CALL COLLECT(RLWIN,DUM1)
      CALL COLLECT(RADOT,DUM2)
      CALL COLLECT(CZMEAN,DUM3)
      CALL COLLECT(SIGT4,DUM4)
      IF ( LME ) WRITE(LRSTRT2)DUM1,DUM2,DUM3,DUM4
      CALL COLLECT(U00,DUM1)
      CALL COLLECT(LC,DUM2)
      CALL COLLECT(SR,DUM3)
      IF ( LME ) WRITE(LRSTRT2)DUM1,UL,DUM2,DUM3
      CALL COLLECT(PREC,DUM1)
      CALL COLLECT(ACPREC,DUM2)
      CALL COLLECT(ACCLIQ,DUM3)
      CALL COLLECT(CUPREC,DUM4)
      IF ( LME ) WRITE(LRSTRT2)RUN,IDAT,IHRST,NTSD,LABEL
     1,             DUM1,DUM2,DUM3,DUM4
      CALL COLLECT(ACFRCV,DUM1)
      CALL COLLECT(NCFRCV,DUM2)
      CALL COLLECT(ACFRST,DUM3)
      CALL COLLECT(NCFRST,DUM4)
      IF ( LME ) WRITE(LRSTRT2)DUM1,DUM2,DUM3,DUM4
      CALL COLLECT(ACSNOW,DUM1)
      CALL COLLECT(ACSNOM,DUM2)
      CALL COLLECT(SSROFF,DUM3)
      CALL COLLECT(BGROFF,DUM4)
      IF ( LME ) WRITE(LRSTRT2) DUM1,DUM2,DUM3,DUM4
      CALL COLLECT(SFCSHX,DUM1)
      CALL COLLECT(SFCLHX,DUM2)
      CALL COLLECT(SUBSHX,DUM3)
      CALL COLLECT(SNOPCX,DUM4)
      CALL COLLECT(SFCUVX,DUM5)
      CALL COLLECT(SFCEVP,DUM6)
      CALL COLLECT(POTEVP,DUM7)
      IF ( LME ) WRITE(LRSTRT2)DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,DUM7
      CALL COLLECT(ASWIN,DUM1)
      CALL COLLECT(ASWOUT,DUM2)
      CALL COLLECT(ASWTOA,DUM3)
      CALL COLLECT(ALWIN,DUM4)
      CALL COLLECT(ALWOUT,DUM5)
      CALL COLLECT(ALWTOA,DUM6)
      IF ( LME )WRITE(LRSTRT2)DUM1,DUM2,DUM3,DUM4,DUM5,DUM6
      IF ( LME ) WRITE(LRSTRT2)ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
      CALL COLLECT(TH10,DUM1)
      CALL COLLECT(Q10,DUM2)
      CALL COLLECT(U10,DUM3)
      CALL COLLECT(V10,DUM4)
      CALL COLLECT(TSHLTR,DUM5)
      CALL COLLECT(QSHLTR,DUM6)
      CALL COLLECT(PSHLTR,DUM7)
      IF ( LME )WRITE(LRSTRT2)DUM1,DUM2,DUM3,DUM4,DUM5,DUM6,DUM7
      DO L = 1, NSOIL
         CALL COLLECT(SMC(:,:,L), DUMS(:,:,L))
      END DO
      IF ( LME ) WRITE(LRSTRT2) DUMS
      CALL COLLECT(CMC,DUM1)
      IF ( LME ) WRITE(LRSTRT2) DUM1
      DO L = 1, NSOIL
         CALL COLLECT(STC(:,:,L), DUMS(:,:,L))
      END DO
      IF ( LME ) WRITE(LRSTRT2) DUMS
      CALL COLLECT(POTFLX,DUM1)
      CALL COLLECT(TLMIN,DUM2)
      CALL COLLECT(TLMAX,DUM3)
      IF ( LME ) WRITE(LRSTRT2) DUM1, DUM2, DUM3
     1,             ACUTIM,ARATIM,APHTIM
     2,             NHEAT,NPHS,NCNVC,NPREC,NRDSW,NRDLW,NSRFC
     3,             TPH0D,TLM0D,RESTRT
      DO L=1,LM
        CALL COLLECT(RSWTT(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
        CALL COLLECT(RLWTT(:,:,L),DUM1)
        IF ( LME ) WRITE(LRSTRT2) DUM1
      ENDDO
      CLOSE(LRSTRT2)
C      isp = rtc()
      if ( LME ) THEN
C      print *, ' time for I/O = ',isp-ist
      end if
C-----------------------------------------------------------------------
      IF(LME)THEN
        DONE='DONE'
        ITAG = ihour
C       WRITE(FINFIL,1190)ITAG,RESTHR
 1190   FORMAT('fcstdone',I2.2,'.',A4)
C       LFINFIL=91
C       CLOSE(LFINFIL)
C       OPEN(UNIT=LFINFIL,FILE=FINFIL,FORM='UNFORMATTED',IOSTAT=IER)
C       WRITE(LFINFIL)DONE
C       CLOSE(LFINFIL)
      ENDIF
C
      GOTO 666
667   continue
      print *, ' QUILT I/O SERVER SHUTTING DOWN NOW'
      END
      subroutine decoal(a,len_ch)
      include "BUFFER.comm"
      real a(*)
      if ( len_ch .lt. 0 ) then
         ip = 0
      end if
      do i = 1, abs(len_ch)
         ip = ip + 1
         a(i) = buf(ip)
      end do
      end

