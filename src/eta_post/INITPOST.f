      SUBROUTINE INITPOST
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    INITPOST    INITIALIZE POST FOR RUN
C   PRGRMMR: RUSS TREADON    ORG: W/NP2      DATE: 93-11-10
C     
C ABSTRACT:  THIS ROUTINE INITIALIZES CONSTANTS AND
C   VARIABLES AT THE START OF AN ETA MODEL OR POST 
C   PROCESSOR RUN.
C
C   THIS ROUTINE ASSUMES THAT INTEGERS AND REALS ARE THE SAME SIZE
C   .     
C     
C PROGRAM HISTORY LOG:
C   93-11-10  RUSS TREADON - ADDED DOCBLOC
C   98-05-29  BLACK - CONVERSION OF POST CODE FROM 1-D TO 2-D
C   99-01 20  TUCCILLO - MPI VERSION
C     
C USAGE:    CALL INIT
C   INPUT ARGUMENT LIST:
C     NONE     
C
C   OUTPUT ARGUMENT LIST: 
C     NONE
C     
C   OUTPUT FILES:
C     NONE
C     
C   SUBPROGRAMS CALLED:
C     UTILITIES:
C       NONE
C     LIBRARY:
C       COMMON   - CTLBLK
C                  OUTFIL
C                  LOOPS
C                  MASKS
C                  DYNAMD
C                  PHYS2
C                  MAPOT1
C                  VRBLS
C                  PVRBLS
C                  BOCO
C                  GRIDS
C                  ACMCLD
C                  ACMCLH
C                  ACMPRE
C                  ACMRDL
C                  ACMRDS
C                  ACMSFC
C                  INDX
C
C    
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C
C
C     INCLUDE/SET PARAMETERS.
C     
      INCLUDE "parmeta"
      INCLUDE "params"
      INCLUDE "parm.tbl"
      INCLUDE "parmsoil"
C
C     DECLARE VARIABLES.
C     
      REAL DUMMY ( IM, JM )
C 
C     NOTE: SOME INTEGER VARIABLES ARE READ INTO DUMMY ( A REAL ). THIS IS OK
C     AS LONG AS REALS AND INTEGERS ARE THE SAME SIZE.
C
C     ALSO, EXTRACT IS CALLED WITH DUMMY ( A REAL ) EVEN WHEN THE NUMBERS ARE
C     INTEGERS - THIS IS OK AS LONG AS INTEGERS AND REALS ARE THE SAME SIZE.
      LOGICAL RUN,RUNB,FIRST,RESTRT,SINGLRST
     1,       SIGMA,SUBPOST,NEST,HYDRO
      LOGICAL IOOMG,IOALL
      CHARACTER*32 LABEL
      CHARACTER*40 CONTRL,FILALL,FILMST,FILTMP,FILTKE,FILUNV
     &, FILCLD,FILRAD,FILSFC
      CHARACTER*4 RESTHR
      CHARACTER FNAME*80,ENVAR*50,BLANK*4
      INTEGER IDATB(3)
      REAL ETA(LP1),DETA(LM),AETA(LM)
C     
C     INCLUDE COMMON BLOCKS.
C
      INCLUDE "IOUNIT.comm"
      INCLUDE "OMGAOT.comm"
      INCLUDE "CTLBLK.comm"
      INCLUDE "OUTFIL.comm"
      INCLUDE "LOOPS.comm"
      INCLUDE "MASKS.comm"
      INCLUDE "DYNAMD.comm"
      INCLUDE "PHYS2.comm"
      INCLUDE "MAPOT1.comm"
      INCLUDE "SRFDSP.comm"
      INCLUDE "CNVCLD.comm"
      INCLUDE "VRBLS.comm"
      INCLUDE "PVRBLS.comm"
      INCLUDE "BOCO.comm"
      INCLUDE "ACMCLD.comm"
      INCLUDE "ACMCLH.comm"
      INCLUDE "ACMPRE.comm"
      INCLUDE "ACMRDL.comm"
      INCLUDE "ACMRDS.comm"
      INCLUDE "ACMSFC.comm"
      INCLUDE "CLDWTR.comm"
      INCLUDE "SOIL.comm"
      INCLUDE "EXTRA.comm"
      INCLUDE "E2PFLG.comm"
      INCLUDE "INDX.comm"
C     INCLUDE "KFFDBK.comm"
      
C     
C     DELCARE NAMELIST.
C
      NAMELIST /FCSTDATA/
     & TSTART,TEND,TCP,RESTRT,SINGLRST
     &,SUBPOST,NMAP,TSHDE,SPL
     &,NPHS,NCNVC,NRADSH,NRADLH,NTDDMP
     &,TPREC,THEAT,TCLOD,TRDSW,TRDLW,TSRFC
     &,NEST,HYDRO,SPLINE,ICUMULUS
C
      DATA BLANK/'    '/
C
C***********************************************************************
C     START INIT HERE.
C
      WRITE(STDOUT,*)'INITPOST:  ENTER INITPOST'
      WRITE(STDOUT,*)'INITPOST:  SPLINE=', SPLINE
C
C     ***STEP 1.  READ "NHB" NFILE.
C
C     READ DATA FROM UNIT CONNECTED TO NHIBU.  THIS DATA CONSISTS
C     OF "CONSTANTS" FOR A GIVEN ETA MODEL RUN.
C
      LUNHB = 12
      LSL   = LSM
      REWIND LUNHB

Cmp
        open (unit=LUNHB,file='cnst.file',form='unformatted',
     +          access='sequential')
Cmp
C
C
      READ(LUNHB) NFCST,NBC,LIST,DT,IDTAD,SIGMA,
     X     KHLA,KHHA,KVLA,KVHA,KHL2,KHH2,KVL2,KVH2
cwas  READ(LUNHB) LMH
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,LMH)
cwas  READ(LUNHB) LMV
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,LMV)
C     NEED ALL OF HBM2 FOR MPI TASK 0
      READ(LUNHB) HBM2
C     NEED ALL OF VBM2 FOR MPI TASK 0
      READ(LUNHB) VBM2
cwas  READ(LUNHB) VBM3
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,VBM3)
cwas  READ(LUNHB) SM
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,SM)
cwas  READ(LUNHB) SICE
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,SICE)
      DO L=1,LM
cwas     READ(LUNHB)((HTM(I,J,L),I=1,IM),J=1,JM)
         READ(LUNHB)((DUMMY(I,J),I=1,IM),J=1,JM)
         CALL EXTRACT(DUMMY,HTM(1,1,L))
      END DO
      DO L=1,LM
cwas     READ(LUNHB)((VTM(I,J,L),I=1,IM),J=1,JM)
         READ(LUNHB)((DUMMY(I,J),I=1,IM),J=1,JM)
         CALL EXTRACT(DUMMY,VTM(1,1,L))
      END DO
      READ(LUNHB) DY,CPGFV,EN,ENT,R,PT,TDDAMP,F4D,F4Q,
     X     EF4T,DETAD,RDETA,AETAD,F4Q2,ETAD,DFL,EM,EMT
cwas  READ(LUNHB) DX
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,DX)
cwas  READ(LUNHB) WPDAR
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,WPDAR)
cwas  READ(LUNHB) CPGFU
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,CPGFU)
cwas  READ(LUNHB) CURV
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,CURV)
cwas  READ(LUNHB) FCP
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,FCP)
cwas  READ(LUNHB) FDIV
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,FDIV)
cwas  READ(LUNHB) FAD
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,FAD)
cwas  READ(LUNHB) F
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,F)
cwas  READ(LUNHB) DDMPU
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,DDMPU)
cwas  READ(LUNHB) DDMPV
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,DDMPV)
      READ(LUNHB) PT2,GLAT
      READ(LUNHB) GLON
      READ(LUNHB) PLQ,RDPQ,RDTHEQ,STHEQ,THE0Q
      READ(LUNHB) ROS,CS,DS,ROI,CI,DI,PL,THL,RDQ,RDTH,RDP,
     X     RDTHE,DETA2,AETA2,DFRLG,QS0,SQS,STHE,THE0
cwas  READ(LUNHB) WFK
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,WFK)
cwas  READ(LUNHB) EPSR
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,EPSR)
cwas  READ(LUNHB) TG
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,TG)
cwas  READ(LUNHB) GFFC
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,GFFC)
cwas  READ(LUNHB) SST
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,SST)
cwas  READ(LUNHB) ALB
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,ALB)
cwas  READ(LUNHB) HDAC
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,HDAC)
cwas  READ(LUNHB) HDACV
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,HDACV)
      READ(LUNHB) TTBLQ
      READ(LUNHB) PTBL,TTBL,R1,PT1,TSPH,WBD,SBD,TLM0D,TPH0D,
     X     DLMD,DPHD,CMLD,DP30,X1P,Y1P,IXM,IYM,DETA1,AETA1,
     X     ETA1
cwas  READ(LUNHB) IVGTYP
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,IVGTYP)
cwas  READ(LUNHB) ISLTYP
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,ISLTYP)
cwas  READ(LUNHB) ISLOPE
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,ISLOPE)
cwas  READ(LUNHB) VEGFRC
      READ(LUNHB) DUMMY
      CALL EXTRACT(DUMMY,VEGFRC)
      READ(LUNHB) SLDPTH
      READ(LUNHB) RTDPTH

      WRITE(STDOUT,*)'INITPOST:  READ CONSTANTS FILE NHB'
C     
C     COMPUTE DERIVED CONSTANTS FROM NHB INPUT.
C
      PTDYN = PT
      RDYN  = R
      DO L = 1,LM
         DETA(L) = DETAD(L)
         AETA(L) = AETAD(L)
         ETA(L)  = ETAD(L)
      END DO
      ETA(LP1) = ETAD(LP1)
C     
C     
C     ***STEP 2.  READ NAMELIST FCSTDATA.
C
C     READ NAMELIST FCSTDATA WHICH CONTROLS TIMESTEPS, 
C     ACCUMULATION PERIODS, AND STANDARD OUTPUT
C
      print *,"READ NAMELIST FCSTDATA"
      RESTRT = .FALSE.
      LFCSTD = 11
      REWIND LFCSTD
      READ(LFCSTD,FCSTDATA)
Cmp
	write(6,*) 'what is spline???? ', SPLINE
Cmp
C     
      WRITE(STDOUT,*)'INITPOST:  READ NAMELIST ',
     X     'FCSTDATA - CONTENTS BELOW'
      WRITE(STDOUT,*)'  TSTART,TEND  :  ',TSTART,TEND
      WRITE(STDOUT,*)'  TCP          :  ',TCP
      WRITE(STDOUT,*)'  RESTRT       :  ',RESTRT
      WRITE(STDOUT,*)'  SUBPOST      :  ',SUBPOST
      WRITE(STDOUT,*)'  NMAP,NPHS    :  ',NMAP,NPHS
      WRITE(STDOUT,*)'  NRADSH,NRADLH:  ',NRADSH,NRADLH
      WRITE(STDOUT,*)'  TPREC,THEAT  :  ',TPREC,THEAT
      WRITE(STDOUT,*)'  TCLOD,TRDSW  :  ',TCLOD,TRDSW
      WRITE(STDOUT,*)'  TRDLW,TSRFC  :  ',TRDLW,TSRFC
      WRITE(STDOUT,*)'  ICUMULUS     :  ',ICUMULUS
      WRITE(STDOUT,*)'  TSHDE (POSTED FORECAST HOURS) BELOW:  '
      WRITE(STDOUT,50) (TSHDE(K),K=1,99)
      WRITE(STDOUT,*)'  SPL (POSTED PRESSURE LEVELS) BELOW: '
      WRITE(STDOUT,51) (SPL(L),L=1,LSM)
   50 FORMAT(14(F4.1,1X))
   51 FORMAT(8(F8.1,1X))
C     
C     COMPUTE DERIVED TIME STEPPING CONSTANTS.
C
      FIRST  = .TRUE.
      NSTART = INT(TSTART*TSPH+D50)
      NTSTM  = INT(TEND  *TSPH+D50)+1
      NCP    = INT(TCP   *TSPH+D50)
      NDDAMP = INT(TDDAMP*TSPH+D50)
      NPREC  = INT(TPREC *TSPH+D50)
      NHEAT  = INT(THEAT *TSPH+D50)
      NCLOD  = INT(TCLOD *TSPH+D50)
      NRDSW  = INT(TRDSW *TSPH+D50)
      NRDLW  = INT(TRDLW *TSPH+D50)
      NSRFC  = INT(TSRFC *TSPH+D50)
      NRADS = NINT(TSPH)*NRADSH
      NRADL = NINT(TSPH)*NRADLH
      DTQ2  = NPHS * DT
      TDTQ2 = DTQ2 + DTQ2
      DTD   = D50  * DTQ2
      TDTD  = DTD  + DTD
      KTM   = INT(DTQ2/DTD+D50)
C     
      WRITE(STDOUT,*)' '
      WRITE(STDOUT,*)'DERIVED TIME STEPPING CONSTANTS'
      WRITE(STDOUT,*)' FIRST             :  ',FIRST
      WRITE(STDOUT,*)' NSTART,NSTSM,NCP  :  ',NSTART,NTSTM,NCP
      WRITE(STDOUT,*)' NDDAMP,NPREC,NHEAT:  ',NDDAMP,NPREC,NHEAT
      WRITE(STDOUT,*)' NCLOD,NRDSW,NRDLW :  ',NCLOD,NRDSW,NRDLW
      WRITE(STDOUT,*)' NSRFC             :  ',NSRFC
      WRITE(STDOUT,*)' NRADS,NRADL       :  ',NRADS,NRADL
      WRITE(STDOUT,*)' DTQ2,TDTQ2        :  ',DTQ2,TDTQ2
      WRITE(STDOUT,*)' DTD,TDTD,KTM      :  ',DTD,TDTD,KTM
C
C     COMPUTE DERIVED MAP OUTPUT CONSTANTS.
      DO L = 1,LSL
         ALSL(L) = LOG(SPL(L))
      END DO
      DO I=1,NMAP
         ISHDE(I)=INT(TSHDE(I)*TSPH+D50)+1
      END DO
C     
C     
C     
C     STEP 3.  READ MODEL RESTART FILE
C
      WRITE(STDOUT,*)'INITPOST:  READ RESTRT FILE'
C
      ENVAR=' '
      CALL GETENV("RSTFNL",ENVAR)
      CALL GETENV("tmmark",RESTHR)
      KPATH = INDEX(ENVAR,' ') -1
      IF(KPATH.LE.0) KPATH = LEN(ENVAR)
      print*,'kpath= ',kpath
C
      IF(RESTHR.EQ.'    ')THEN
        WRITE(RSTFIL,75)ITAG
   75   FORMAT('restrt',I2.2)
      ELSE
        WRITE(RSTFIL,80)ITAG,RESTHR
   80   FORMAT('restrt',I2.2,'.',a4)
      ENDIF
C
      KRST = INDEX(RSTFIL,' ') -1
      IF(KRST.LE.0) KRST = LEN(RSTFIL)
      print *,'krst= ',krst
C***
      LRSTRT = 13
      CLOSE(LRSTRT)
C     CALL ASNUNIT(LRSTRT,'-F cos -C ascii -N ibm',IER)
C     IF(IER.NE.0)
C    1     WRITE(LIST,*)'INITPOST:  ASNUNIT ERROR IER=',IER

      print *,"FNAME: " ,RSTFIL
      IF(ENVAR(1:4).EQ.BLANK) THEN
        print *,"OPENING ",LRSTRT, RSTFIL
        OPEN(LRSTRT,FILE=RSTFIL,FORM='UNFORMATTED')
      ELSE
        FNAME = ENVAR(1:KPATH) // RSTFIL(1:KRST)
        OPEN(UNIT=LRSTRT,FILE=FNAME,FORM='UNFORMATTED',IOSTAT=IER)
      ENDIF
C***
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
      print *,"LABEL",LABEL
C***
      READ(LRSTRT) PDOMG,RESOMG
C***
      DO L = 1,LM
cwas    READ(LRSTRT) ((OMGA(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT) ((DUMMY(I,J),I=1,IM),J=1,JM)
        CALL EXTRACT(DUMMY,OMGA(1,1,L))
      END DO
C***
      WRITE(STDOUT,*)'  READ ',LABEL
C
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &     FIRST,IOUT,NSHDE
C***
CWAS  READ(LRSTRT) PD,RES,FIS
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,PD)
      do i=1,im
       do j=jsta,jend
        if((pt+pd(i,j)).lt.60000.)print*,'enormal psfc',i,j,pd(i,j)+pt
       end do
      end do       
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,RES)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,FIS)
C
      READ(LRSTRT) PDB,TB,QB,UB,VB
C***
      DO L = 1,LM
cwas    READ(LRSTRT) ((T(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT) ((DUMMY(I,J),I=1,IM),J=1,JM)
        CALL EXTRACT(DUMMY,T(1,1,L))
cwas    READ(LRSTRT) ((Q(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT) ((DUMMY(I,J),I=1,IM),J=1,JM)
        CALL EXTRACT(DUMMY,Q(1,1,L))
cwas    READ(LRSTRT) ((U(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT) ((DUMMY(I,J),I=1,IM),J=1,JM)
        CALL EXTRACT(DUMMY,U(1,1,L))
cwas    READ(LRSTRT) ((V(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT) ((DUMMY(I,J),I=1,IM),J=1,JM)
        CALL EXTRACT(DUMMY,V(1,1,L))
cwas    READ(LRSTRT) ((Q2(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT) ((DUMMY(I,J),I=1,IM),J=1,JM)
        CALL EXTRACT(DUMMY,Q2(1,1,L))
        READ(LRSTRT)
c       READ(LRSTRT) ((TTND(I,J,L),I=1,IM),J=1,JM)
cwas    READ(LRSTRT) ((CWM(I,J,L),I=1,IM),J=1,JM)
        READ(LRSTRT) ((DUMMY(I,J),I=1,IM),J=1,JM)
        CALL EXTRACT(DUMMY,CWM(1,1,L))
        READ(LRSTRT)
        READ(LRSTRT)
c       READ(LRSTRT) ((TRAIN(I,J,L),I=1,IM),J=1,JM)
c       READ(LRSTRT) ((TCUCN(I,J,L),I=1,IM),J=1,JM)
      ENDDO
C***
      WRITE(STDOUT,*)'  READ ',LABEL
C
cwas  READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
cwas &,            RSWIN,RSWOUT,TG,Z0,AKMS,CZEN
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
     &,            DUMMY
      CALL EXTRACT(DUMMY,RSWIN)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
     &,            DUMMY,DUMMY
      CALL EXTRACT(DUMMY,RSWOUT)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
     &,            DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,TG)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
     &,            DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,Z0)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
     &,            DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,AKMS)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL
     &,            DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CZEN)
C***
cwas  READ(LRSTRT) AKHS,THS,QS,TWBS,QWBS,HBOT,CFRACL
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,AKHS)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,THS)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,QS)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,TWBS)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,QWBS)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,HBOT)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CFRACL)
C***
cwas  READ(LRSTRT) THZ0,QZ0,UZ0,VZ0,USTAR,HTOP,CFRACM
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,THZ0)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,QZ0)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,UZ0)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,VZ0)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,USTAR)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,HTOP)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CFRACM)
C***
CWAS  READ(LRSTRT) SNO,WET,CLDEFI,RF,PSLP,CUPPT,CFRACH
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,SNO)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,WET)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CLDEFI)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,RF)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,PSLP)
      print*,'pslp N boundary'
      do j=jend,jend-2,-1
       write(6,*) (pslp(i,j),i=15,20)
      end do
      print*,'pslp S boundary'
      do j=3,1,-1
       write(6,*) (pslp(i,j),i=15,20)
	enddo
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CUPPT)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CFRACH)
C     
C***
cwas  READ(LRSTRT) SOILTB,SFCEXC,SMSTAV,SMSTOT,GRNFLX,PCTSNO
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,SOILTB)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SFCEXC)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SMSTAV)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SMSTOT)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,GRNFLX)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,PCTSNO)
C***
CWAS  READ(LRSTRT) RLWIN,RADOT,CZMEAN,SIGT4
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,RLWIN)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,RADOT)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CZMEAN)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SIGT4)
C***
CWAS  READ(LRSTRT) U00,UL,LC,SR
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,U00)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,UL,DUMMY
      CALL EXTRACT(DUMMY,LC)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,UL,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SR)
C***
      WRITE(STDOUT,*)'  READ ',LABEL
C
CWAS  READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL,
CWAS &             PREC,ACPREC,ACCLIQ,CUPREC
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &             DUMMY
      CALL EXTRACT(DUMMY,PREC)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &             DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ACPREC)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &             DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ACCLIQ)
      BACKSPACE LRSTRT
      READ(LRSTRT) RUN,IDAT,IHRST,NTSD,LABEL,
     &             DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,CUPREC)
C***
cwas  READ(LRSTRT) ACFRCV,NCFRCV,ACFRST,NCFRST
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,ACFRCV)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,NCFRCV)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ACFRST)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,NCFRST)
C**
cwas  READ(LRSTRT) ACSNOW,ACSNOM,SSROFF,BGROFF
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,ACSNOW)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ACSNOM)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SSROFF)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,BGROFF)
C***
cwas  READ(LRSTRT) SFCSHX,SFCLHX,SUBSHX,SNOPCX
cwas 1,            SFCUVX,SFCEVP,POTEVP
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,SFCSHX)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SFCLHX)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SUBSHX)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SNOPCX)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SFCUVX)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,SFCEVP)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,POTEVP)
C***
cwas  READ(LRSTRT) ASWIN,ASWOUT,ASWTOA,ALWIN,ALWOUT,ALWTOA
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,ASWIN)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ASWOUT)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ASWTOA)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ALWIN)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ALWOUT)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,ALWTOA)
C***
      READ(LRSTRT) ARDSW,ARDLW,ASRFC,AVRAIN,AVCNVC
C***
cwas  READ(LRSTRT) TH10,Q10,U10,V10,TSHLTR,QSHLTR,PSHLTR
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,TH10)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY
      CALL EXTRACT(DUMMY,Q10)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,U10)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,V10)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,TSHLTR)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,QSHLTR)
      BACKSPACE LRSTRT
      READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
      CALL EXTRACT(DUMMY,PSHLTR)
C     BACKSPACE LRSTRT
C     READ(LRSTRT) DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY,DUMMY
C     CALL EXTRACT(DUMMY,SUMFB)

C***
cwas  READ(LRSTRT) (((SMC(I,J,N),I=1,IM),J=1,JM),N=1,NSOIL)
      DO II = 1, NSOIL
         READ(LRSTRT) (((DUMMY(I,J),I=1,IM),J=1,JM),N=1,II)
         CALL EXTRACT(DUMMY,SMC(1,1,II))
         BACKSPACE LRSTRT
      END DO
      READ(LRSTRT)
C***
cwas  READ(LRSTRT) CMC
      READ(LRSTRT) DUMMY
      CALL EXTRACT(DUMMY,CMC)
C***
cwas  READ(LRSTRT) (((STC(I,J,N),I=1,IM),J=1,JM),N=1,NSOIL)
      DO II = 1, NSOIL
         READ(LRSTRT) (((DUMMY(I,J),I=1,IM),J=1,JM),N=1,II)
         CALL EXTRACT(DUMMY,STC(1,1,II))
         BACKSPACE LRSTRT
      END DO
      READ(LRSTRT)
C***
      WRITE(STDOUT,*)'  READ ',LABEL
C     
C     END OF RESTART FILE READ.
C     
C     CLOSE THE RESTART FILE.
      CLOSE(LRSTRT)
C     
C     SET UP THESE FLAGS FOR ETA2P     
C     
      IOOMG=.FALSE.
      IOALL=.FALSE.
C***
C***  BOUND SR BY 0 AND 1
C***
      DO J=JSTA,JEND
      DO I=1,IM
        SR(I,J)=MAX(SR(I,J),0.)
        SR(I,J)=MIN(SR(I,J),1.)
      ENDDO
      ENDDO
C***
C***  FILL IN THE EXTRA-LEVEL ARRAYS WITH VALUES AT L=LM
C***  WHEREVER THERE ARE NO STEPS.
C***
      DO J=JSTA,JEND
      DO I=1,IM
        LMHK=LMH(I,J)
        IF(LMHK.EQ.LM)THEN
          PDSL(I,J)=RES(I,J)*PD(I,J)
          TH10(I,J)=T(I,J,LM)*(1.0E5/(AETA(LM)*PDSL(I,J)+PT))**CAPA
          Q10(I,J)=Q(I,J,LM)
        ENDIF
      ENDDO
      ENDDO
C
      DO J=JSTA,JEND
      DO I=1,IM
        LMVK=LMV(I,J)
        IF(LMVK.EQ.LM)THEN
          U10(I,J)=U(I,J,LM)
          V10(I,J)=V(I,J,LM)
        ENDIF
      ENDDO
      ENDDO
C
C     CLIP NEGATIVE SPECIFIC HUMIDITY.
C
      DO 100 L=1,LM
      CALL BOUNDL(Q(1,1,L),H1M12,H99999,IM,JM)
  100 CONTINUE 
      WRITE(STDOUT,*)'INITPOST:  CLIP NEGATIVE SPECIFIC HUMIDITY'
C     
C     COMPUTE PRESSURE AND LN(P) AT INTERFACES.
C     SET SURFACE VALUES.
C     
!$omp  parallel do
!$omp& private(pbi)
      DO J=JSTA,JEND
      DO I=1,IM
        PDSL(I,J)      =RES(I,J)*PD(I,J)
        PBI            =PD(I,J)+PT
        PINT(I,J,LP1)  =PBI
        ALPINT(I,J,LP1)=ALOG(PBI)
      ENDDO
      ENDDO
C     
C     COMPUTE OMEGA ON ETA LEVELS.
C
!$omp  parallel do
!$omp& private(rtopkl)
      DO 125 L=1,LM
      DO J=JSTA,JEND
      DO I=1,IM
        RTOPKL=RDYN*T(I,J,L)*(H1+D608*Q(I,J,L))/
     1         (PDSL(I,J)*AETA(L)+PT)
        IF(ABS(RTOPKL).GT.H1M12) 
     1       OMGA(I,J,L)=OMGA(I,J,L)*CP/(RTOPKL*DT)
      ENDDO
      ENDDO
  125 CONTINUE
C
C     CALCULATE AVERAGE PRESSURE DIFFERENCE BETWEEN ETA=1 AND ETA=0
C     AT VELOCITY POINTS (PDVP1) USING THE VALUES ALREADY KNOWN AT
C     HEIGHT POINTS (PDSL).
C
      CALL EXCH(PDSL)
!$omp  parallel do
      DO J=2,JM-1,2
      DO I=2,IM-1
        PDVP1(I,J)=0.25*(PDSL(I-1,J)+PDSL(I,J)
     1                  +PDSL(I,J+1)+PDSL(I,J-1))
      ENDDO
      ENDDO
!$omp  parallel do
      DO J=3,JM-1,2
      DO I=1,IM-1
        PDVP1(I,J)=0.25*(PDSL(I+1,J)+PDSL(I,J)
     1                  +PDSL(I,J+1)+PDSL(I,J-1))
      ENDDO
      ENDDO
C
!$omp  parallel do
      DO I=1,IM-1
        PDVP1(I,1)=0.5*(PDSL(I,1)+PDSL(I+1,1))
        PDVP1(I,JM)=0.5*(PDSL(I,JM)+PDSL(I+1,JM))
      ENDDO
C
!$omp  parallel do
      DO J=2,JM-1,2
        PDVP1(1,J)=0.5*(PDSL(1,J-1)+PDSL(1,J+1))
        PDVP1(IM,J)=0.5*(PDSL(IM,J-1)+PDSL(IM,J+1))
      ENDDO
C
      PDVP1(IM,JM)=PDVP1(IM-1,JM)
C     
C     FIND THE HIGHEST ETA LAYER CONTAINING MOUNTAINS.
C     
      DO 150 L=LM,1,-1
C
      DO J=JSTA,JEND
      DO I=1,IM
        IF (HTM(I,J,L).EQ.D00)GO TO 150
      ENDDO
      ENDDO
C
      LHMNT=L+1
      GO TO 155
  150 CONTINUE
  155 IF(LHMNT.EQ.LP1)THEN
        IF(.NOT.SIGMA)THEN
          GO TO 175
        ELSE
          LHMNT = LM
        ENDIF
      ENDIF
C***
C***  NOW GATHER THE ADDRESSES OF ALL THE UNDERGROUND POINTS.
C***
      DO 170 L=LHMNT,LM
      KMN=0
      KMNTM(L)=0
C
      K=0
      DO 160 J=1,JM
      IEND=IM
      IF(MOD(J,2).EQ.0)IEND=IM-1
      DO 160 I=1,IEND
      K=K+1
      IF ( J .GE. JSTA .AND. J .LE. JEND ) THEN
         KMNT(K,L)=0
         IF(HTM(I,J,L).EQ.H1)GO TO 160
         KMN=KMN+1
         KMNT(KMN,L)=K
      END IF
  160 CONTINUE
      KMNTM(L)=KMN
  170 CONTINUE
  175 CONTINUE
C     
C     COMPUTE PRESSURE VALUES ABOVE THE SURFACE.
C
!$omp  parallel do
!$omp& private(pbi)
      DO L=LM,1,-1
        DO J=JSTA,JEND
        DO I=1,IM
          PBI          =PDSL(I,J)*ETA(L)+PT
          PINT(I,J,L)  =PBI     
          ALPINT(I,J,L)=ALOG(PBI)
        ENDDO
        ENDDO
      ENDDO
C
C     COMPUTE PRESSURE VALUES BELOW THE SURFACE.
C
      KMM=KMNTM(LM)
      DO KM=1,KMM
        K=KMNT(KM,LM)
        NDROW=K/IMT
        LFTOV1=MOD(K,IMT)
        IF(LFTOV1-IM.GT.0)THEN
          I=K-NDROW*IMT-IM
          IADD=2
        ELSEIF(LFTOV1.GT.0)THEN
          I=K-NDROW*IMT
          IADD=1
        ELSEIF(LFTOV1.EQ.0)THEN
          I=IM-1
          IADD=0
        ENDIF
        J=2*NDROW+IADD
C
C       COMPUTE ALPINT AND PINT ONLY FOR THOSE POINTS THAT WE OWN
C
C       IF ( J .GE. JSTA .AND. J .LE. JEND ) THEN
C
        LMAP1=LMH(I,J)+1
        DO L=LMAP1,LM
          ALPINT(I,J,L+1)=(DFL(L)-DFL(L+1))/(R*T(I,J,L))
     1                   +ALPINT(I,J,L)
          PINT(I,J,L+1)=EXP(ALPINT(I,J,L+1))
        ENDDO
C       END IF
      ENDDO
C
C     CALCULATE THE I-INDEX EAST-WEST INCREMENTS
C
      DO J=1,JM
        IHE(J)=MOD(J+1,2)
        IHW(J)=IHE(J)-1
        IVE(J)=MOD(J,2)
        IVW(J)=IVE(J)-1
      ENDDO
C
      WRITE(STDOUT,*)'INITPOST:  COMPLETE ONE TIME CALCULATIONS'
C     
C     END OF ROUTINE.
C     
      WRITE(STDOUT,*)'INITPOST:  ALL INPUT DATA READ/PREPARED.'
      WRITE(STDOUT,*)' '
      RETURN
      END
