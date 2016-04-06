      SUBROUTINE LUTAB
C----------------------------------------------------------------------
C**  THIS SUBROUTINE IS A LOOKUP TABLE.
C**  GIVEN A SERIES OF SERIES OF SATURATION EQUIVALENT POTENTIAL 
C**  TEMPERATURES, THE TEMPERATURE IS CALCULATED.
C----------------------------------------------------------------------
      PARAMETER(KFNT=250,KFNP=220) 
C----------------------------------------------------------------------
      COMMON/KFLUT/ TTAB(KFNT,KFNP),QSTAB(KFNT,KFNP),THE0K(KFNP)
     1,             ALU(200),RDPR,RDTHK,PTOP
C----------------------------------------------------------------------
C***  EQUIVALENT POTENTIAL TEMPERATURE INCREMENT
C
      DATA DTH/1./
C----------------------------------------------------------------------
C***  MINIMUM STARTING TEMP 
C
      DATA TMIN/150./
C----------------------------------------------------------------------
C***  TOLERANCE FOR ACCURACY OF TEMPERATURE 
C
      DATA TOLER/0.001/
C----------------------------------------------------------------------
C***  TOP PRESSURE (PASCALS)
C
      PTOP=100.0
C----------------------------------------------------------------------
C***  BOTTOM PRESSURE (PASCALS)
C
      PBOT=110000.0
C----------------------------------------------------------------------
C
C***  DEFINE CONSTANTS FOR CALCULATION OF SATURATION VAPOR PRESSURE
C***  ACCORDING TO BUCK (J. APPL. METEO., DECEMBER, 1981)
C
      ALIQ=613.3
      BLIQ=17.502
      CLIQ=4780.8
      DLIQ=32.19
C----------------------------------------------------------------------
C
C***  COMPUTE PARAMETERS
C
C----------------------------------------------------------------------
C***  1./(SAT. EQUIV. THETA INCREMENT)
C
      RDTHK=1./DTH
C----------------------------------------------------------------------
C***  PRESSURE INCREMENT
C
      DPR=(PBOT-PTOP)/FLOAT(KFNP-1)
C----------------------------------------------------------------------
C***  1./(PRESSURE INCREMENT)
C
      RDPR=1./DPR
C----------------------------------------------------------------------
C***  CALCULATE THE STARTING SAT. EQUIV. THETA
C
      TEMP=TMIN 
      P=PTOP-DPR
C
      DO KP=1,KFNP
        P=P+DPR
        ES=ALIQ*EXP((BLIQ*TEMP-CLIQ)/(TEMP-DLIQ))
        QS=0.622*ES/(P-ES)
        PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
        THE0K(KP)=TEMP*PI*EXP((3374.6525/TEMP-2.5403)*QS*
     1           (1.+0.81*QS))
      ENDDO
C----------------------------------------------------------------------
C
C***  COMPUTE TEMPERATURES FOR EACH SAT. EQUIV. POTENTIAL TEMP.
C
      P=PTOP-DPR
C
      DO 60 KP=1,KFNP
      THES=THE0K(KP)-DTH
      P=P+DPR
C
      DO 50 IT=1,KFNT
C
C***  DEFINE SAT. EQUIV. POT. TEMP.
C
      THES=THES+DTH
C
C***  ITERATE TO FIND TEMPERATURE
C***   FIND INITIAL GUESS
C
      IF(IT.EQ.1)THEN
        TGUES=TMIN
      ELSE
        TGUES=TTAB(IT-1,KP)
      ENDIF
C
      ES=ALIQ*EXP((BLIQ*TGUES-CLIQ)/(TGUES-DLIQ))
      QS=0.622*ES/(P-ES)
      PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
      THGUES=TGUES*PI*EXP((3374.6525/TGUES-2.5403)*QS*
     1         (1.+0.81*QS))
      F0=THGUES-THES
      T1=TGUES-0.5*F0
      T0=TGUES
      ITCNT=0
C
C***  ITERATION LOOP
C
   30 CONTINUE
C
      ES=ALIQ*EXP((BLIQ*T1-CLIQ)/(T1-DLIQ))
      QS=0.622*ES/(P-ES)
      PI=(1.E5/P)**(0.2854*(1.-0.28*QS))
      THTGS=T1*PI*EXP((3374.6525/T1-2.5403)*QS*
     1      (1.+0.81*QS))
      F1=THTGS-THES
      IF(ABS(F1).LT.TOLER)GO TO 40
      ITCNT=ITCNT+1
C
      IF(ITCNT.GT.10)THEN
        PRINT*,' ITCNT > 10',' IT=',IT,' P=',P,' T1=',T1,
     1         ' THES=',THES
        GO TO 40
      ENDIF
C
      DT=F1*(T1-T0)/(F1-F0)
      T0=T1
      F0=F1
      T1=T1-DT
      GO TO 30
   40 CONTINUE
C
      TTAB(IT,KP)=T1 
      QSTAB(IT,KP)=QS
   50 CONTINUE
   60 CONTINUE
C----------------------------------------------------------------------
C
C***  LOOKUP TABLE FOR TLOG(EMIX/ALIQ)
C
C***  SET UP INTIAL VALUES FOR LOOKUP TABLES
C
      ASTRT=1.E-3
      AINC=0.075
c
      A1=ASTRT-AINC
C
      DO I=1,200
        A1=A1+AINC
        ALU(I)=ALOG(A1)
      ENDDO
C----------------------------------------------------------------------
      RETURN
      END
