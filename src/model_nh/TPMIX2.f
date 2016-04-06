      SUBROUTINE TPMIX2(P,THES,TU,QU,QLIQ,QICE,QNEWLQ,QNEWIC,RATIO2
     1,                 XLV1,XLV0)
C----------------------------------------------------------------------
C*************** LOOKUP TABLE VARIABLES *******************************
      PARAMETER(KFNT=250,KFNP=220)
C
      COMMON/KFLUT/ TTAB(KFNT,KFNP),QSTAB(KFNT,KFNP),THE0K(KFNP)
     1,             ALU(200),RDPR,RDTHK,PTOP 
C*************************************************************** 
C
C***********************************************************************
C***  SCALING PRESSURE & TT TABLE INDEX                         
C***********************************************************************
C
      TP=(P-PTOP)*RDPR
      QQ=TP-AINT(TP)
      IPTB=INT(TP)+1
C
C***********************************************************************
C***  BASE AND SCALING FACTOR FOR THE 
C***********************************************************************
C
C***  SCALING THE & TT TABLE INDEX                                        
C
      BTH=(THE0K(IPTB+1)-THE0K(IPTB))*QQ+THE0K(IPTB)
      TTH=(THES-BTH)*RDTHK
      PP   =TTH-AINT(TTH)
      ITHTB=INT(TTH)+1
c
      T00=TTAB(ITHTB  ,IPTB  )
      T10=TTAB(ITHTB+1,IPTB  )
      T01=TTAB(ITHTB  ,IPTB+1)
      T11=TTAB(ITHTB+1,IPTB+1)
C
      Q00=QSTAB(ITHTB  ,IPTB  )
      Q10=QSTAB(ITHTB+1,IPTB  )
      Q01=QSTAB(ITHTB  ,IPTB+1)
      Q11=QSTAB(ITHTB+1,IPTB+1)
C
C***********************************************************************
C***  PARCEL TEMPERATURE                                        
c***********************************************************************
C
      TEMP=(T00+(T10-T00)*PP+(T01-T00)*QQ
     1         +(T00-T10-T01+T11)*PP*QQ)
c
      QS=(Q00+(Q10-Q00)*PP+(Q01-Q00)*QQ
     1         +(Q00-Q10-Q01+Q11)*PP*QQ)
C
      IF(QS.LE.QU)THEN
        QNEW=QU-QS
        QU=QS
        GO TO 100
      ENDIF
C
C***  IF THE PARCEL IS SUBSATURATED, TEMPERATURE AND MIXING RATIO MUST BE
C***  ADJUSTED.  IF LIQUID WATER IS PRESENT, IT IS ALLOWED TO EVAPORATE.
C
      QNEW=0.
      DQ=QS-QU
      QTOT=QLIQ+QICE
C
C***  IF THERE IS ENOUGH LIQUID OR ICE TO SATURATE THE PARCEL, TEMP STAYS 
C***  AT ITS WET BULB VALUE, VAPOR MIXING RATIO IS AT SATURATED LEVEL, 
C***  AND THE MIXING RATIOS OF LIQUID AND ICE ARE ADJUSTED TO MAKE UP THE
C***  ORIGINAL SATURATION  DEFICIT.  OTHERWISE, ANY AVAILABLE LIQ OR ICE
C***  VAPORIZES AND APPROPRIATE ADJUSTMENTS TO PARCEL TEMP, VAPOR, LIQUID,
C***  AND ICE MIXING RATIOS ARE MADE.
C
C***  NOTE THAT THE LIQ AND ICE MAY BE PRESENT IN PROPORTIONS SLIGHTLY 
C***  DIFFERENT THAN SUGGESTED BY THE VALUE OF RATIO2.  CHECK TO MAKE SURE 
C***  THAT LIQUID AND ICE CONCENTRATIONS ARE NOT REDUCED TO BELOW ZERO WHEN 
C***  EVAPORATION/SUBLIMATION OCCURS.
C
C***  SUBSATURATED VALUES ONLY OCCUR IN CALCULATIONS INVOLVING VARIOUS 
C***  MIXTURES OF UPDRAFT AND ENVIRONMENTAL AIR FOR ESTIMATION OF
C***  ENTRAINMENT AND DETRAINMENT.  FOR THESE PURPOSES, ASSUME THAT 
C***  REASONABLE ESTIMATES CAN BE GIVEN USING LIQUID WATER SATURATION
C***  CALCULATIONS ONLY - I.E., IGNORE THE EFFECT OF THE ICE PHASE
C***  IN THIS PROCESS ONLY.
C
      IF(QTOT.GE.DQ)THEN
        QLIQ=QLIQ-DQ*QLIQ/QTOT
        QICE=QICE-DQ*QICE/QTOT
        QU=QS
        GO TO 100
      ELSE
        RLL=XLV0-XLV1*TEMP
        CP=1005.7*(1.+0.89*QU)
C
C***  IF NO LIQUID WATER OR ICE IS AVAILABLE, TEMPERATURE IS GIVEN BY:
C
        IF(QTOT.LT.1.E-10)THEN
          TEMP=TEMP+RLL*(DQ/(1.+DQ))/CP
          GO TO 100
C
C***  IF SOME LIQ WATER/ICE IS AVAILABLE, BUT NOT ENOUGH TO ACHIEVE
C***  SATURATION, THE TEMPERATURE IS GIVEN BY:
C
        ELSE
          TEMP=TEMP+RLL*((DQ-QTOT)/(1+DQ-QTOT))/CP
          QU=QU+QTOT
          QTOT=0.
        ENDIF
C
        QLIQ=0.
        QICE=0.
      ENDIF
C-------------------------------------------------------------------------
  100 TU=TEMP
      QNEWLQ=QNEW
      QNEWIC=0.
C
      RETURN
      END
