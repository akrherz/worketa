      SUBROUTINE TPMIX2DD(P,THES,TS,QS)
C-----------------------------------------------------------------------
C
C******************* LOOKUP TABLE VARIABLES ****************************
      PARAMETER(KFNT=250,KFNP=220)
C-----------------------------------------------------------------------
      COMMON/KFLUT/ TTAB(KFNT,KFNP),QSTAB(KFNT,KFNP),THE0K(KFNP)
     1,             ALU(200),RDPR,RDTHK,PTOP 
C*********************************************************************** 
C
C***********************************************************************
C***  SCALING PRESSURE & TT TABLE INDEX  
c***********************************************************************
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
C
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
C***********************************************************************
C
      TS=(T00+(T10-T00)*PP+(T01-T00)*QQ
     1       +(T00-T10-T01+T11)*PP*QQ)
C
      QS=(Q00+(Q10-Q00)*PP+(Q01-Q00)*QQ
     1         +(Q00-Q10-Q01+Q11)*PP*QQ)
C-----------------------------------------------------------------------
      RETURN
      END
