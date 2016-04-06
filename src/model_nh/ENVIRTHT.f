      SUBROUTINE ENVIRTHT(P1,T1,Q1,THT1,R1,RL,                          
     1   ALIQ,BLIQ,CLIQ,DLIQ,AICE,BICE,CICE,DICE)                       
C----------------------------------------------------------------------
      PARAMETER(KFNT=250,KFNP=220)
C----------------------------------------------------------------------
      COMMON/KFLUT/ TTAB(KFNT,KFNP),QSTAB(KFNT,KFNP),THE0K(KFNP)
     1,             ALU(200),RDPR,RDTHK,PTOP
C----------------------------------------------------------------------
      DATA T00,P00,C1,C2,C3,C4,C5/273.16,1.E5,3374.6525,2.5403,3114.834
     1,    0.278296,1.0723E-3/                                          
C----------------------------------------------------------------------
C                                                                       
C***  CALCULATE ENVIRONMENTAL EQUIVALENT POTENTIAL TEMPERATURE.
C            
      EMIX=Q1*P1/(0.622+Q1)
C
C***  FIND THE TEMPERATURE OF THE MIXTURE AT ITS LCL.
C
      ASTRT=1.E-3
      AINC=0.075
      A1=EMIX/ALIQ
      TP=(A1-ASTRT)/AINC
      INDLU=INT(TP)+1
      VALUE=(INDLU-1)*AINC+ASTRT
      AINTRP=(A1-VALUE)/AINC
      TLOG=AINTRP*ALU(INDLU+1)+(1-AINTRP)*ALU(INDLU)
      TDPT=(CLIQ-DLIQ*TLOG)/(BLIQ-TLOG)
      TLCL=TDPT-(.212+1.571E-3*(TDPT-T00)-4.36E-4*(T1-T00))*(T1-
     1     TDPT)
      TSAT=AMIN1(TLCL,T1)
      THT=T1*(P00/P1)**(0.2854*(1.-0.28*Q1))                          
      THT1=THT*EXP((C1/TSAT-C2)*Q1*(1.+0.81*Q1))                      
C----------------------------------------------------------------------
      RETURN
      END              
