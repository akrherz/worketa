      SUBROUTINE CONDLOAD(QLIQ,QICE,WTW,DZ,BOTERM,ENTERM,RATE,QNEWLQ
     1,                   QNEWIC,QLQOUT,QICOUT)
C----------------------------------------------------------------------
C***  THIS PRECIPITATION FALLOUT SCHEME IS BASED ON THE SCHEME USED
C***  BY OGURA AND CHO (1973).  LIQUID WATER FALLOUT FROM A PARCEL IS 
C***  CALCULATED USING THE EQUATION DQ=-RATE*Q*DT, BUT TO SIMULATE A 
C***  QUASI-CONTINUOUS PROCESS, AND TO ELIMINATE A DEPENDENCY ON
C***  VERTICAL RESOLUTION THIS IS EXPRESSED AS Q=Q*EXP(-RATE*DZ).                   
C----------------------------------------------------------------------
      G=9.8
C
      QTOT=QLIQ+QICE                                                    
      QNEW=QNEWLQ+QNEWIC                                                
C                                                                       
C***  ESTIMATE THE VERTICAL VELOCITY SO THAT AN AVERAGE VERTICAL VELOCITY 
C***  CAN BE CALCULATED TO ESTIMATE THE TIME REQUIRED FOR ASCENT BETWEEN
C***  MODEL LEVELS.
C                                                                       
      QEST=0.5*(QTOT+QNEW)                                              
      G1=WTW+BOTERM-ENTERM-2.*G*DZ*QEST/1.5                             
      IF(G1.LT.0.0)G1=0.                                                
      WAVG=(SQRT(WTW)+SQRT(G1))/2.                                      
      CONV=RATE*DZ/WAVG                                                 
      CONV=AMIN1(CONV,50.)
      CONV=AMAX1(CONV,-50.)
C                                                                       
C***  RATIO3 IS THE FRACTION OF LIQUID WATER IN FRESH CONDENSATE,
C***  RATIO4 IS THE FRACTION OF LIQUID WATER IN THE TOTAL AMOUNT OF
C***  CONDENSATE INVOLVED IN THE PRECIPITATION PROCESS.
C***  NOTE THAT ONLY 60% OF THE FRESH CONDENSATE IS ALLOWED
C***  TO PARTICIPATE IN THE CONVERSION PROCESS.
C                                                                       
      RATIO3=QNEWLQ/(QNEW+1.E-10)                                       
C     OLDQ=QTOT                                                         
      QTOT=QTOT+0.6*QNEW                                                
      OLDQ=QTOT                                                         
      RATIO4=(0.6*QNEWLQ+QLIQ)/(QTOT+1.E-10)                            
      QTOT=QTOT*EXP(-CONV)                                              
C
C***  DETERMINE THE AMOUNT OF PRECIPITATION THAT FALLS OUT OF THE
C***  UPDRAFT PARCEL AT THIS LEVEL.
C                                                                       
      DQ=OLDQ-QTOT                                                      
      QLQOUT=RATIO4*DQ                                                  
      QICOUT=(1.-RATIO4)*DQ                                             
C                                 
C***  ESTIMATE THE MEAN LOAD OF CONDENSATE ON THE UPDRAFT IN THE LAYER,
C***  CALCULATE VERTICAL VELOCITY.
C
      PPTDRG=0.5*(OLDQ+QTOT-0.2*QNEW)                                   
      WTW=WTW+BOTERM-ENTERM-2.*G*DZ*PPTDRG/1.5                          
C
C***  DETERMINE THE NEW LIQUID WATER AND ICE CONCENTRATIONS INCLUDING
C***  LOSSES DUE TO PRECIPITATION AND GAINS FROM CONDENSATION.
C                                                                       
      QLIQ=RATIO4*QTOT+RATIO3*0.4*QNEW                                  
      QICE=(1.-RATIO4)*QTOT+(1.-RATIO3)*0.4*QNEW                        
      QNEWLQ=0.                                                         
      QNEWIC=0.                                                         
C----------------------------------------------------------------------
      RETURN                                                            
      END
