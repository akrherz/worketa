      SUBROUTINE SWR93(FSWC,HSWC,UFSWC,DFSWC,FSWL,HSWL,UFSWL,
     &                 DFSWL,
     1                 PRESS,COSZRO,TAUDAR,RH2O,RRCO2,SSOLAR,QO3,
     2                 NCLDS,KTOPSW,KBTMSW,CAMT,CRR,CTT,
     A                 ALVB,ALNB,ALVD,ALND,GDFVB,GDFNB,GDFVD,GDFND)
C===>    *********************************************************
C --- SWR91SIB --- MODIFIED FROM SWR89-BAND12....YUTAI HOU
C
C     -SW- RADIATION CODE............................
C        INPUTS:PRESS,COSZRO,TAUDAR,RH2O,RRCO2,SSOLAR,QO3,NCLDS,
C               KTOPSW,KBTMSW,CIRAB,CIRRF,CUVRF,CAMT,
C               ALVB,ALVD,ALNB,ALND;
C        OUTPUT:FSWC,HSWC,UFSWC,DFSWC,FSWL,HSWL,UFSWL,DFSWL,
C               GDFVB,GDFVD,GDFNB,GDFND.
C --- SWR91SIB --- MODIFIED BY K. CAMPANA..06 MAR 92
C         INCLUDE HPCON,PARMC CHANGED TO HCON,RDPARM
C         6 Q..... VARIABLE NAMES RESTORED TO ORIGINAL 7,8 CHAR
C         CHANGE O3DIFF,DIFFCC TO O3DIFCTR,DIFFCTR
C --- SWR91SIB --- MODIFIED BY Y. HOU         FEB 93
C         INPUTS 12 BANDS CLD REFLECTTANCE AND TRANSMITTANCE
C         CRR,CTT TO REPLACE CIRAB,CIRRF,CUVRF
C===>    *********************************************************
C
C
        INCLUDE "parmeta"
        INCLUDE "HCON.comm" 
        INCLUDE "rdparm" 
        INCLUDE "mpp.h"


C     PARAMETER SETTINGS FOR THE LONGWAVE AND SHORTWAVE RADIATION CODE:
C          L      =  NO. VERTICAL LEVELS (ALSO LAYERS) IN MODEL
C          NB IS A SHORTWAVE PARAMETER; OTHER QUANTITIES ARE DERIVED
C                    FROM THE ABOVE PARAMETERS.
C --- VARIABLES AS IN ARGUMENT LIST
                       D I M E N S I O N
     1  FSWC  (IDIM1:IDIM2,LP1),  HSWC  (IDIM1:IDIM2,LP1),
     2  CRR   (IDIM1:IDIM2,NB,LP1)
     3, FSWL  (IDIM1:IDIM2,LP1),  HSWL  (IDIM1:IDIM2,LP1),
     4  CTT   (IDIM1:IDIM2,NB,LP1)
     5, UFSWC (IDIM1:IDIM2,LP1),  DFSWC (IDIM1:IDIM2,LP1)
     6, UFSWL (IDIM1:IDIM2,LP1),  DFSWL (IDIM1:IDIM2,LP1)
     7, PRESS (IDIM1:IDIM2,LP1),  RH2O  (IDIM1:IDIM2,L),
     8  QO3   (IDIM1:IDIM2,L)
     9, CAMT  (IDIM1:IDIM2,LP1),  KTOPSW(IDIM1:IDIM2,LP1),
     o  KBTMSW(IDIM1:IDIM2,LP1)
     1, COSZRO(IDIM1:IDIM2),      TAUDAR(IDIM1:IDIM2),
     2  NCLDS (IDIM1:IDIM2)
     3, ALVB  (IDIM1:IDIM2),  ALNB  (IDIM1:IDIM2),
     4  ALVD  (IDIM1:IDIM2),  ALND  (IDIM1:IDIM2)
     5, GDFVB (IDIM1:IDIM2),  GDFNB (IDIM1:IDIM2),  GDFVD (IDIM1:IDIM2),
     6  GDFND (IDIM1:IDIM2)
C --- LOCAL VARIABLES
                       D I M E N S I O N
     1  PP    (IDIM1:IDIM2,LP1),  DP    (IDIM1:IDIM2,LP1),
     2  PR2   (IDIM1:IDIM2,LP1)
     3, DU    (IDIM1:IDIM2,LP1),  DUCO2 (IDIM1:IDIM2,LP1),
     4  DUO3  (IDIM1:IDIM2,LP1)
     5, FF    (IDIM1:IDIM2,LP1),  FFCO2 (IDIM1:IDIM2,LP1),
     6  FFO3  (IDIM1:IDIM2,LP1)
     7, RRAY  (IDIM1:IDIM2),      DFNTOP(IDIM1:IDIM2,NB),
     8  SECZ  (IDIM1:IDIM2)
     9, REFL  (IDIM1:IDIM2),      TMP1 (IDIM1:IDIM2),
     o  REFL2 (IDIM1:IDIM2)
     1, CCMAX (IDIM1:IDIM2),      XAMT  (IDIM1:IDIM2,LP1)
                       D I M E N S I O N
     1  UD    (IDIM1:IDIM2,LP1),   UR    (IDIM1:IDIM2,LP1)
     2, UCO2  (IDIM1:IDIM2,LLP2), UDCO2 (IDIM1:IDIM2,LP1),
     3  URCO2 (IDIM1:IDIM2,LP1)
     4, UO3   (IDIM1:IDIM2,LLP2), UDO3  (IDIM1:IDIM2,LP1),
     5  URO3  (IDIM1:IDIM2,LP1)
     6, TCO2  (IDIM1:IDIM2,LLP2), TDCO2 (IDIM1:IDIM2,LP1),
     7  TUCO2 (IDIM1:IDIM2,LP1)
     8, TO3   (IDIM1:IDIM2,LLP2), TDO3  (IDIM1:IDIM2,LP1),
     9  TUO3  (IDIM1:IDIM2,LP1)
                       D I M E N S I O N
     1  DFN   (IDIM1:IDIM2,LP1),  UFN   (IDIM1:IDIM2,LP1),
     2  CR    (IDIM1:IDIM2,LP1)
     3, TTD   (IDIM1:IDIM2,LP1),  TTU   (IDIM1:IDIM2,LP1),
     4  CT    (IDIM1:IDIM2,LP1)
     5, PPTOP (IDIM1:IDIM2,LP1),  DPCLD (IDIM1:IDIM2,LP1)
C --- EQUIVALENCED LOCAL VARIABLES
                       D I M E N S I O N
     1  TTUB1 (IDIM1:IDIM2,LP1),  TUCL1 (IDIM1:IDIM2,LP1)
     2, TTDB1 (IDIM1:IDIM2,LP1),  TDCL1 (IDIM1:IDIM2,LP1),
     3  TDCL2 (IDIM1:IDIM2,LP1)
     4, UFNTRN(IDIM1:IDIM2,LP1),  UFNCLU(IDIM1:IDIM2,LP1),
     5  TCLU  (IDIM1:IDIM2,LP1)
     6, DFNTRN(IDIM1:IDIM2,LP1),  DFNCLU(IDIM1:IDIM2,LP1),
     7  TCLD  (IDIM1:IDIM2,LP1)
     8, ALFA  (IDIM1:IDIM2,LP1),  ALFAU (IDIM1:IDIM2,LP1)
                     E Q U I V A L E N C E
     1  (UDO3,UO3(IDIM1,1),DFNCLU), (URO3,UO3(IDIM1,LP2), UFNCLU)
     2, (UDCO2,UCO2(IDIM1,1),TCLD), (URCO2,UCO2(IDIM1,LP2), TCLU)
     3, (TDO3 ,TO3(IDIM1,1),DFNTRN),(TUO3,TO3(IDIM1,LP2), UFNTRN)
     4, (TDCO2,TCO2(IDIM1,1)      ),(TUCO2,TCO2(IDIM1,LP2)        )
     5, (FF   , ALFA ),   (FFCO2 , ALFAU ),   (FFO3  , TTDB1 )
     6, (DU   , TTUB1),   (DUCO2 , TUCL1 ),   (DUO3  , TDCL1 )
     7, (PR2  , TDCL2)
C
C---COMMON FOR LOCAL DATA VARIABLES---
      COMMON /SWRSAV/ ABCFF(NB),PWTS(NB),CFCO2,CFO3,REFLO3,RRAYAV
C                           D A T A
C    1  ABCFF / 2*4.0E-5, 0.002, 0.035, 0.377, 1.95, 9.40, 44.6,
C    1          190.0,    989.0, 2706.0, 39011.0 /
C    2, PWTS  / 0.5000, 0.121416, 0.0698, 0.1558, 0.0631, 0.0362,
C    2          0.0243, 0.0158, 0.0087, 0.001467, 0.002342, 0.001075 /
C    3, CFCO2, CFO3, REFLO3, RRAYAV / 508.96, 466.64, 1.9, 0.144 /
C    1  ABCFF / 2*4.0E-5, .002, .035, .377, 1.95, 9.40, 44.6, 190. /
C    2, PWTS  /.5000,.1470,.698,.1443,.0584,.0335,.0225,.0158,.0087/
C    3, CFCO2, CFO3, REFLO3, RRAYAV / 508.96, 466.64, 1.9, 0.144 /
C
C     CALCULATE SECANT OF ZENITH ANGLE (SECZ),FLUX PRESSURES(PP),
C     LAYER WIDTH (DP) AND PRESSURE SCALING FACTOR (PR2).
      DO 100 I=MYIS,MYIE
        SECZ(I) = H35E1/SQRT(H1224E3*COSZRO(I)*COSZRO(I)+ONE)
        PP(I,1)   = ZERO
        PP(I,LP1) = PRESS(I,LP1)
        TMP1(I)  = ONE/PRESS(I,LP1)
100   CONTINUE
      DO 110 K=1,LM1
      DO 110 I=MYIS,MYIE
        PP(I,K+1) = HAF*(PRESS(I,K+1)+PRESS(I,K))
110   CONTINUE
      DO 120 K=1,L
      DO 120 I=MYIS,MYIE
        DP (I,K) = PP(I,K+1)-PP(I,K)
        PR2(I,K) = HAF*(PP(I,K)+PP(I,K+1))
120   CONTINUE
      DO 130 K=1,L
      DO 130 I=MYIS,MYIE
        PR2(I,K) = PR2(I,K)*TMP1(I)
130   CONTINUE
C     CALCULATE ENTERING FLUX AT THE TOP FOR EACH BAND(IN CGS UNITS)
      DO 140 N=1,NB
      DO 140 IP=MYIS,MYIE
        DFNTOP(IP,N) = SSOLAR*H69766E5*COSZRO(IP)*TAUDAR(IP)*PWTS(N)
140   CONTINUE
C     EXECUTE THE LACIS-HANSEN REFLECTIVITY PARAMETERIZATION
C     FOR THE VISIBLE BAND
      DO 150 I=MYIS,MYIE
        RRAY(I) = HP219/(ONE+HP816*COSZRO(I))
        REFL(I) = RRAY(I) + (ONE-RRAY(I))*(ONE-RRAYAV)*ALVB(I)/
     1            (ONE-ALVD(I)*RRAYAV)
150   CONTINUE
      DO 155 I=MYIS,MYIE
        RRAY(I) = 0.104/(ONE+4.8*COSZRO(I))
        REFL2(I)= RRAY(I) + (ONE-RRAY(I))*(ONE-0.093)*ALVB(I)/
     1            (ONE-ALVD(I)*0.093)
155   CONTINUE
C     CALCULATE PRESSURE-WEIGHTED OPTICAL PATHS FOR EACH LAYER
C     IN UNITS OF CM-ATM. PRESSURE WEIGHTING IS USING PR2.
C     DU= VALUE FOR H2O;DUCO2 FOR CO2;DUO3 FOR O3.
      DO 160 K=1,L
      DO 160 I=MYIS,MYIE
        DU   (I,K) = GINV*RH2O(I,K)*DP(I,K)*PR2(I,K)
        DUCO2(I,K) = (RRCO2*GINV*CFCO2)*DP(I,K)*PR2(I,K)
        DUO3 (I,K) = (GINV*CFO3)*QO3(I,K)*DP(I,K)
160   CONTINUE
C
C                 CALCULATE CLEAR SKY SW FLUX
C
C     OBTAIN THE OPTICAL PATH FROM THE TOP OF THE ATMOSPHERE TO THE
C     FLUX PRESSURE. ANGULAR FACTORS ARE NOW INCLUDED. UD=DOWNWARD
C     PATH FOR H2O,WIGTH UR THE UPWARD PATH FOR H2O. CORRESPONDING
C     QUANTITIES FOR CO2,O3 ARE UDCO2/URCO2 AND UDO3/URO3.
      DO 200 IP=MYIS,MYIE
        UD   (IP,1) = ZERO
        UDCO2(IP,1) = ZERO
        UDO3 (IP,1) = ZERO
200   CONTINUE
      DO 210 K=2,LP1
      DO 210 I=MYIS,MYIE
        UD   (I,K) = UD   (I,K-1)+DU   (I,K-1)*SECZ(I)
        UDCO2(I,K) = UDCO2(I,K-1)+DUCO2(I,K-1)*SECZ(I)
        UDO3 (I,K) = UDO3 (I,K-1)+DUO3 (I,K-1)*SECZ(I)
210   CONTINUE
      DO 220 IP=MYIS,MYIE
        UR   (IP,LP1) = UD   (IP,LP1)
        URCO2(IP,LP1) = UDCO2(IP,LP1)
        URO3 (IP,LP1) = UDO3 (IP,LP1)
220   CONTINUE
      DO 230 K=L,1,-1
      DO 230 IP=MYIS,MYIE
        UR   (IP,K) = UR   (IP,K+1)+DU   (IP,K)*DIFFCTR
        URCO2(IP,K) = URCO2(IP,K+1)+DUCO2(IP,K)*DIFFCTR
        URO3 (IP,K) = URO3 (IP,K+1)+DUO3 (IP,K)*O3DIFCTR
230   CONTINUE
C     CALCULATE CO2 ABSORPTIONS . THEY WILL BE USED IN NEAR INFRARED
C     BANDS.SINCE THE ABSORPTION AMOUNT IS GIVEN (IN THE FORMULA USED
C     BELOW, DERIVED FROM SASAMORI) IN TERMS OF THE TOTAL SOLAR FLUX,
C     AND THE ABSORPTION IS ONLY INCLUDED IN THE NEAR IR (50 PERCENT
C     OF THE SOLAR SPECTRUM), THE ABSORPTIONS ARE MULTIPLIED BY 2.
C       SINCE CODE ACTUALLY REQUIRES TRANSMISSIONS, THESE ARE THE
C     VALUES ACTUALLY STORED IN TCO2.
      DO 240 K=1,LL
      DO 240 I=MYIS,MYIE
       TCO2(I,K+1)=ONE-TWO*(H235M3*EXP(HP26*LOG(UCO2(I,K+1)+H129M2))
     1                       -H75826M4)
240   CONTINUE
C     NOW CALCULATE OZONE ABSORPTIONS. THESE WILL BE USED IN
C     THE VISIBLE BAND.JUST AS IN THE CO2 CASE, SINCE THIS BAND IS
C     50 PERCENT OF THE SOLAR SPECTRUM,THE ABSORPTIONS ARE MULTIPLIED
C     BY 2. THE TRANSMISSIONS ARE STORED IN TO3.
      HTEMP = H1036E2*H1036E2*H1036E2
      DO 250 K=1,LL
      DO 250 I=MYIS,MYIE
        TO3(I,K+1)=ONE-TWO*UO3(I,K+1)*
     1            (H1P082*EXP(HMP805*LOG(ONE+H1386E2*UO3(I,K+1)))+
     2            H658M2/(ONE+HTEMP*UO3(I,K+1)*UO3(I,K+1)*UO3(I,K+1))+
     3            H2118M2/(ONE+UO3(I,K+1)*(H42M2+H323M4*UO3(I,K+1))))
250   CONTINUE
C   START FREQUENCY LOOP (ON N) HERE
C
C--- BAND 1 (VISIBLE) INCLUDES O3 AND H2O ABSORPTION
      DO 260 K=1,L
      DO 260 I=MYIS,MYIE
        TTD(I,K+1) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UD(I,K+1)))
        TTU(I,K) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UR(I,K)))
        DFN(I,K+1) = TTD(I,K+1)*TDO3(I,K+1)
        UFN(I,K) = TTU(I,K)*TUO3(I,K)
260   CONTINUE
      DO 270 I=MYIS,MYIE
        DFN(I,1)   = ONE
        UFN(I,LP1) = DFN(I,LP1)
270   CONTINUE
C     SCALE VISIBLE BAND FLUXES BY SOLAR FLUX AT THE TOP OF THE
C     ATMOSPHERE (DFNTOP(I,1))
C     DFSW/UFSW WILL BE THE FLUXES, SUMMED OVER ALL BANDS
      DO 280  K=1,LP1
      DO 280  I=MYIS,MYIE
        DFSWL(I,K) =         DFN(I,K)*DFNTOP(I,1)
        UFSWL(I,K) = REFL(I)*UFN(I,K)*DFNTOP(I,1)
280   CONTINUE
      DO 285 I=MYIS,MYIE
        GDFVB(I) = DFSWL(I,LP1)*EXP(-0.15746*SECZ(I))
        GDFVD(I) = ((ONE-REFL2(I))*DFSWL(I,LP1) -
     1              (ONE-ALVB(I)) *GDFVB(I)) / (ONE-ALVD(I))
        GDFNB(I) = ZERO
        GDFND(I) = ZERO
285   CONTINUE
C---NOW OBTAIN FLUXES FOR THE NEAR IR BANDS. THE METHODS ARE THE SAME
C   AS FOR THE VISIBLE BAND, EXCEPT THAT THE REFLECTION AND
C   TRANSMISSION COEFFICIENTS (OBTAINED BELOW) ARE DIFFERENT, AS
C   RAYLEIGH SCATTERING NEED NOT BE CONSIDERED.
      DO 350 N=2,NB
        IF (N.EQ.2) THEN
C   THE WATER VAPOR TRANSMISSION FUNCTION FOR BAND 2 IS EQUAL TO
C   THAT OF BAND 1 (SAVED AS TTD,TTU)
C--- BAND 2-9 (NEAR-IR) INCLUDES O3, CO2 AND H2O ABSORPTION
          DO 290 K=1,L
          DO 290 I=MYIS,MYIE
            DFN(I,K+1) = TTD(I,K+1)*TDCO2(I,K+1)
            UFN(I,K) = TTU(I,K)*TUCO2(I,K)
290       CONTINUE
        ELSE
C   CALCULATE WATER VAPOR TRANSMISSION FUNCTIONS FOR NEAR INFRARED
C   BANDS. INCLUDE CO2 TRANSMISSION (TDCO2/TUCO2), WHICH
C   IS THE SAME FOR ALL INFRARED BANDS.
          DO 300 K=1,L
          DO 300 I=MYIS,MYIE
            DFN(I,K+1)=EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UD(I,K+1)))
     1                 *TDCO2(I,K+1)
            UFN(I,K)=EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UR(I,K)))
     1               *TUCO2(I,K)
300       CONTINUE
        ENDIF
C---AT THIS POINT,INCLUDE DFN(1),UFN(LP1), NOTING THAT DFN(1)=1 FOR
C   ALL BANDS, AND THAT UFN(LP1)=DFN(LP1) FOR ALL BANDS.
        DO 310 I=MYIS,MYIE
          DFN(I,1)   = ONE
          UFN(I,LP1) = DFN(I,LP1)
310     CONTINUE
C     SCALE THE PREVIOUSLY COMPUTED FLUXES BY THE FLUX AT THE TOP
C     AND SUM OVER BANDS
        DO 320 K=1,LP1
        DO 320 I=MYIS,MYIE
          DFSWL(I,K) = DFSWL(I,K) +         DFN(I,K)*DFNTOP(I,N)
          UFSWL(I,K) = UFSWL(I,K) + ALNB(I)*UFN(I,K)*DFNTOP(I,N)
320     CONTINUE
        DO 330 I=MYIS,MYIE
          GDFNB(I) = GDFNB(I) + DFN(I,LP1)*DFNTOP(I,N)
330     CONTINUE
350   CONTINUE
      DO 360 K=1,LP1
      DO 360 I=MYIS,MYIE
        FSWL(I,K) = UFSWL(I,K)-DFSWL(I,K)
360   CONTINUE
      DO 370 K=1,L
      DO 370 I=MYIS,MYIE
        HSWL(I,K)=RADCON*(FSWL(I,K+1)-FSWL(I,K))/DP(I,K)
370   CONTINUE
C
C---END OF FREQUENCY LOOP (OVER N)
C
C                 CALCULATE CLOUDY SKY SW FLUX
C
      KCLDS=NCLDS(MYIS)
      DO 400 I=MYIS1,MYIE
        KCLDS=MAX(NCLDS(I),KCLDS)
400   CONTINUE
        DO 410 K=1,LP1
        DO 410 I=MYIS,MYIE
          DFSWC(I,K) = DFSWL(I,K)
          UFSWC(I,K) = UFSWL(I,K)
          FSWC (I,K) = FSWL (I,K)
410     CONTINUE
        DO 420 K=1,L
        DO 420 I=MYIS,MYIE
          HSWC(I,K) = HSWL(I,K)
420     CONTINUE
C*******************************************************************
      IF (KCLDS .EQ. 0)  RETURN
C*******************************************************************
      DO 430 K=1,LP1
      DO 430 I=MYIS,MYIE
        XAMT(I,K) = CAMT(I,K)
430   CONTINUE
      DO 470 I=MYIS,MYIE
        NNCLDS   = NCLDS(I)
        CCMAX(I) = ZERO
        IF (NNCLDS .LE. 0) GO TO 470
        CCMAX(I) = ONE
        DO 450 K=1,NNCLDS
          CCMAX(I) = CCMAX(I) * (ONE - CAMT(I,K+1))
450     CONTINUE
        CCMAX(I) = ONE - CCMAX(I)
        IF (CCMAX(I) .GT. ZERO) THEN
          DO 460 K=1,NNCLDS
            XAMT(I,K+1) = CAMT(I,K+1)/CCMAX(I)
460       CONTINUE
        END IF
470   CONTINUE
      DO 480 K=1,LP1
      DO 480 I=MYIS,MYIE
        FF   (I,K) = DIFFCTR
        FFCO2(I,K) = DIFFCTR
        FFO3 (I,K) = O3DIFCTR
480   CONTINUE
      DO 490 IP=MYIS,MYIE
        JTOP = KTOPSW(IP,NCLDS(IP)+1)
      DO 490 K=1,JTOP
        FF   (IP,K) = SECZ(IP)
        FFCO2(IP,K) = SECZ(IP)
        FFO3 (IP,K) = SECZ(IP)
490   CONTINUE
      DO 500 I=MYIS,MYIE
        RRAY(I) = HP219/(ONE+HP816*COSZRO(I))
        REFL(I) = RRAY(I) + (ONE-RRAY(I))*(ONE-RRAYAV)*ALVD(I)/
     1            (ONE-ALVD(I)*RRAYAV)
500   CONTINUE
      DO 510 IP=MYIS,MYIE
        UD   (IP,1) = ZERO
        UDCO2(IP,1) = ZERO
        UDO3 (IP,1) = ZERO
510   CONTINUE
      DO 520 K=2,LP1
      DO 520 I=MYIS,MYIE
        UD   (I,K) = UD   (I,K-1)+DU   (I,K-1)*FF   (I,K)
        UDCO2(I,K) = UDCO2(I,K-1)+DUCO2(I,K-1)*FFCO2(I,K)
        UDO3 (I,K) = UDO3 (I,K-1)+DUO3 (I,K-1)*FFO3 (I,K)
520   CONTINUE
      DO 530 IP=MYIS,MYIE
        UR   (IP,LP1) = UD   (IP,LP1)
        URCO2(IP,LP1) = UDCO2(IP,LP1)
        URO3 (IP,LP1) = UDO3 (IP,LP1)
530   CONTINUE
      DO 540 K=L,1,-1
      DO 540 IP=MYIS,MYIE
        UR   (IP,K) = UR   (IP,K+1)+DU   (IP,K)*DIFFCTR
        URCO2(IP,K) = URCO2(IP,K+1)+DUCO2(IP,K)*DIFFCTR
        URO3 (IP,K) = URO3 (IP,K+1)+DUO3 (IP,K)*O3DIFCTR
540   CONTINUE
      DO 550 K=1,LL
      DO 550 I=MYIS,MYIE
        TCO2(I,K+1)=ONE-TWO*(H235M3*EXP(HP26*LOG(UCO2(I,K+1)+H129M2))
     1                        -H75826M4)
550   CONTINUE
      DO 560 K=1,LL
      DO 560 I=MYIS,MYIE
        TO3(I,K+1)=ONE-TWO*UO3(I,K+1)*
     1           (H1P082*EXP(HMP805*LOG(ONE+H1386E2*UO3(I,K+1)))+
     2          H658M2/(ONE+HTEMP*UO3(I,K+1)*UO3(I,K+1)*UO3(I,K+1))+
     3          H2118M2/(ONE+UO3(I,K+1)*(H42M2+H323M4*UO3(I,K+1))))
560   CONTINUE
C********************************************************************
C---THE FIRST CLOUD IS THE GROUND; ITS PROPERTIES ARE GIVEN
C   BY REFL (THE TRANSMISSION (0) IS IRRELEVANT FOR NOW!).
C********************************************************************
      DO 570 I=MYIS,MYIE
        CR(I,1) = REFL(I)
570   CONTINUE
C***OBTAIN CLOUD REFLECTION AND TRANSMISSION COEFFICIENTS FOR
C   REMAINING CLOUDS (IF ANY) IN THE VISIBLE BAND
C---THE MAXIMUM NO OF CLOUDS IN THE ROW (KCLDS) IS USED. THIS CREATES
C   EXTRA WORK (MAY BE REMOVED IN A SUBSEQUENT UPDATE).
      DO 581 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 581
      DO 580 KK=2,KCLDS+1
        CR(I,KK) = CRR(I,1,KK)*XAMT(I,KK)
        CT(I,KK) = ONE - (ONE-CTT(I,1,KK))*XAMT(I,KK)
580   CONTINUE
581   CONTINUE
C---OBTAIN THE PRESSURE AT THE TOP,BOTTOM AND THE THICKNESS OF
C   "THICK" CLOUDS (THOSE AT LEAST 2 LAYERS THICK). THIS IS USED
C   LATER IS OBTAINING FLUXES INSIDE THE THICK CLOUDS, FOR ALL
C   FREQUENCY BANDS.
      DO 591 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 591
      DO 590 KK=1,KCLDS
        IF ((KBTMSW(I,KK+1)-1).GT.KTOPSW(I,KK+1)) THEN
           PPTOP(I,KK)=PP(I,KTOPSW(I,KK+1))
           DPCLD(I,KK)=ONE/(PPTOP(I,KK)-PP(I,KBTMSW(I,KK+1)))
        ENDIF
590   CONTINUE
591   CONTINUE
      DO 600 K=1,L
      DO 600 I=MYIS,MYIE
        TTDB1(I,K+1) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UD(I,K+1)))
        TTUB1(I,K) = EXP(HM1EZ*MIN(FIFTY,ABCFF(1)*UR(I,K)))
        TTD  (I,K+1) = TTDB1(I,K+1)*TDO3(I,K+1)
        TTU  (I,K) = TTUB1(I,K)*TUO3(I,K)
600   CONTINUE
      DO 610 I=MYIS,MYIE
        TTD(I,1)   = ONE
        TTU(I,LP1) = TTD(I,LP1)
610   CONTINUE
C***FOR EXECUTION OF THE CLOUD LOOP, IT IS NECESSARY TO SEPARATE OUT
C   TRANSMISSION FCTNS AT THE TOP AND BOTTOM OF THE CLOUDS, FOR
C   EACH BAND N. THE REQUIRED QUANTITIES ARE:
C      TTD(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
C      TTU(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
C      TTD(I,KBTMSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
C      AND INVERSES OF THE FIRST TWO. THE ABOVE QUANTITIES ARE
C      STORED IN TDCL1,TUCL1,TDCL2, AND DFNTRN,UFNTRN, RESPECTIVELY,
C      AS THEY HAVE MULTIPLE USE IN THE PGM.
C---FOR FIRST CLOUD LAYER (GROUND) TDCL1,TUCL1 ARE KNOWN:
      DO 620 I=MYIS,MYIE
        TDCL1 (I,1) = TTD(I,LP1)
        TUCL1 (I,1) = TTU(I,LP1)
        TDCL2 (I,1) = TDCL1(I,1)
        DFNTRN(I,1) = ONE/TDCL1(I,1)
        UFNTRN(I,1) = DFNTRN(I,1)
620   CONTINUE
      DO 631 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 631
      DO 630 KK=2,KCLDS+1
        TDCL1(I,KK) = TTD(I,KTOPSW(I,KK))
        TUCL1(I,KK) = TTU(I,KTOPSW(I,KK))
        TDCL2(I,KK) = TTD(I,KBTMSW(I,KK))
630   CONTINUE
631   CONTINUE
C---COMPUTE INVERSES
      DO 641 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 641
      DO 640 KK=2,KCLDS
        DFNTRN(I,KK) = ONE/TDCL1(I,KK)
        UFNTRN(I,KK) = ONE/TUCL1(I,KK)
640   CONTINUE
641   CONTINUE
C---COMPUTE THE TRANSMISSIVITY FROM THE TOP OF CLOUD (K+1) TO THE
C   TOP OF CLOUD (K). THE CLOUD TRANSMISSION (CT) IS INCLUDED. THIS
C   QUANTITY IS CALLED TCLU (INDEX K). ALSO, OBTAIN THE TRANSMISSIVITY
C   FROM THE BOTTOM OF CLOUD (K+1) TO THE TOP OF CLOUD (K)(A PATH
C   ENTIRELY OUTSIDE CLOUDS). THIS QUANTITY IS CALLED TCLD (INDEX K).
      DO 651 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 651
      DO 650 KK=1,KCLDS
        TCLU(I,KK) = TDCL1(I,KK)*DFNTRN(I,KK+1)*CT(I,KK+1)
        TCLD(I,KK) = TDCL1(I,KK)/TDCL2(I,KK+1)
650   CONTINUE
651   CONTINUE
C***THE FOLLOWING IS THE RECURSION RELATION FOR ALFA: THE REFLECTION
C   COEFFICIENT FOR A SYSTEM INCLUDING THE CLOUD IN QUESTION AND THE
C   FLUX COMING OUT OF THE CLOUD SYSTEM INCLUDING ALL CLOUDS BELOW
C   THE CLOUD IN QUESTION.
C---ALFAU IS ALFA WITHOUT THE REFLECTION OF THE CLOUD IN QUESTION
      DO 660 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 660
        ALFA (I,1)=CR(I,1)
        ALFAU(I,1)=ZERO
660   CONTINUE
C---AGAIN,EXCESSIVE CALCULATIONS-MAY BE CHANGED LATER!
      DO 671 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 671
      DO 670 KK=2,KCLDS+1
        ALFAU(I,KK)= TCLU(I,KK-1)*TCLU(I,KK-1)*ALFA(I,KK-1)/
     1        (ONE - TCLD(I,KK-1)*TCLD(I,KK-1)*ALFA(I,KK-1)*CR(I,KK))
        ALFA (I,KK)= ALFAU(I,KK)+CR(I,KK)
670   CONTINUE
671   CONTINUE
C     CALCULATE UFN AT CLOUD TOPS AND DFN AT CLOUD BOTTOMS
C---NOTE THAT UFNCLU(I,KCLDS+1) GIVES THE UPWARD FLUX AT THE TOP
C   OF THE HIGHEST REAL CLOUD (IF NCLDS(I)=KCLDS). IT GIVES THE FLUX
C   AT THE TOP OF THE ATMOSPHERE IF NCLDS(I) < KCLDS. IN THE FIRST
C   CASE, TDCL1 EQUALS THE TRANSMISSION FCTN TO THE TOP OF THE
C   HIGHEST CLOUD, AS WE WANT. IN THE SECOND CASE, TDCL1=1, SO UFNCLU
C   EQUALS ALFA. THIS IS ALSO CORRECT.
      DO 680 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 680
        UFNCLU(I,KCLDS+1) = ALFA(I,KCLDS+1)*TDCL1(I,KCLDS+1)
        DFNCLU(I,KCLDS+1) = TDCL1(I,KCLDS+1)
680   CONTINUE
C---THIS CALCULATION IS THE REVERSE OF THE RECURSION RELATION USED
C  ABOVE
      DO 691 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 691
      DO 690 KK=KCLDS,1,-1
        UFNCLU(I,KK) = UFNCLU(I,KK+1)*ALFAU(I,KK+1)/(ALFA(I,KK+1)*
     1                 TCLU(I,KK))
        DFNCLU(I,KK) = UFNCLU(I,KK)/ALFA(I,KK)
690   CONTINUE
691   CONTINUE
      DO 701 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 701
      DO 700 KK=1,KCLDS+1
        UFNTRN(I,KK) = UFNCLU(I,KK)*UFNTRN(I,KK)
        DFNTRN(I,KK) = DFNCLU(I,KK)*DFNTRN(I,KK)
700   CONTINUE
701   CONTINUE
C---CASE OF KK=1( FROM THE GROUND TO THE BOTTOM OF THE LOWEST CLOUD)
      DO 720 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 720
        J2=KBTMSW(I,2)
        DO 710 K=J2,LP1
          UFN(I,K) = UFNTRN(I,1)*TTU(I,K)
          DFN(I,K) = DFNTRN(I,1)*TTD(I,K)
710     CONTINUE
720   CONTINUE
C---REMAINING LEVELS (IF ANY!)
      DO 760 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 760
      DO 755 KK=2,KCLDS+1
        J1=KTOPSW(I,KK)
        J2=KBTMSW(I,KK+1)
        IF (J1.EQ.1) GO TO 755
        DO 730 K=J2,J1
          UFN(I,K) = UFNTRN(I,KK)*TTU(I,K)
          DFN(I,K) = DFNTRN(I,KK)*TTD(I,K)
730     CONTINUE
C---FOR THE THICK CLOUDS, THE FLUX DIVERGENCE THROUGH THE CLOUD
C   LAYER IS ASSUMED TO BE CONSTANT. THE FLUX DERIVATIVE IS GIVEN BY
C   TEMPF (FOR THE UPWARD FLUX) AND TEMPG (FOR THE DOWNWARD FLUX).
        J3=KBTMSW(I,KK)
        IF ((J3-J1).GT.1) THEN
          TEMPF = (UFNCLU(I,KK)-UFN(I,J3))*DPCLD(I,KK-1)
          TEMPG = (DFNCLU(I,KK)-DFN(I,J3))*DPCLD(I,KK-1)
          DO 740 K=J1+1,J3-1
            UFN(I,K) = UFNCLU(I,KK)+TEMPF*(PP(I,K)-PPTOP(I,KK-1))
            DFN(I,K) = DFNCLU(I,KK)+TEMPG*(PP(I,K)-PPTOP(I,KK-1))
740       CONTINUE
        ENDIF
755   CONTINUE
760   CONTINUE
      DO 770 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 770
      DO 771 K=1,LP1
        DFSWC(I,K) = DFN(I,K)*DFNTOP(I,1)
        UFSWC(I,K) = UFN(I,K)*DFNTOP(I,1)
771   CONTINUE
770   CONTINUE
      DO 780 I=MYIS,MYIE
      KCLDS=NCLDS(I)
      IF(KCLDS.EQ.0) GO TO 780
        TMP1(I) = ONE - CCMAX(I)
        GDFVB(I) = TMP1(I)*GDFVB(I)
        GDFNB(I) = TMP1(I)*GDFNB(I)
        GDFVD(I) = TMP1(I)*GDFVD(I) + CCMAX(I)*DFSWC(I,LP1)
780   CONTINUE
C---NOW OBTAIN FLUXES FOR THE NEAR IR BANDS. THE METHODS ARE THE SAME
C   AS FOR THE VISIBLE BAND, EXCEPT THAT THE REFLECTION AND
C   TRANSMISSION COEFFICIENTS ARE DIFFERENT, AS
C   RAYLEIGH SCATTERING NEED NOT BE CONSIDERED.
C
      DO 1000 N=2,NB
CYH93
        DO 791 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 791
        DO 790 K=1,KCLDS+1
          CR(I,K) = CRR(I,N,K)*XAMT(I,K)
          CT(I,K) = ONE - (ONE-CTT(I,N,K))*XAMT(I,K)
790     CONTINUE
791     CONTINUE
CYH93
        IF (N.EQ.2) THEN
C   THE WATER VAPOR TRANSMISSION FUNCTION FOR BAND 2 IS EQUAL TO
C   THAT OF BAND 1 (SAVED AS TTDB1,TTUB1)
          DO 800 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 800
        DO 801 KK=2,LP1
            TTD(I,KK) = TTDB1(I,KK)*TDCO2(I,KK)
801     CONTINUE
        DO 802 KK=1,L
            TTU(I,KK) = TTUB1(I,KK)*TUCO2(I,KK)
802     CONTINUE
800       CONTINUE
        ELSE
          DO 810 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 810
        DO 811 KK=2,LP1
            TTD(I,KK) = EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UD(I,KK)))
     1               * TDCO2(I,KK)
811     CONTINUE
        DO 812 KK=1,L
            TTU(I,KK) = EXP(HM1EZ*MIN(FIFTY,ABCFF(N)*UR(I,KK)))
     1               * TUCO2(I,KK)
812     CONTINUE
810       CONTINUE
        ENDIF
C---AT THIS POINT,INCLUDE TTD(1),TTU(LP1), NOTING THAT TTD(1)=1 FOR
C   ALL BANDS, AND THAT TTU(LP1)=TTD(LP1) FOR ALL BANDS.
        DO 820 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 820
          TTU(I,LP1) = TTD(I,LP1)
          TTD(I,1)   = ONE
820     CONTINUE
C***FOR EXECUTION OF THE CLOUD LOOP, IT IS NECESSARY TO SEPARATE OUT
C   TRANSMISSION FCTNS AT THE TOP AND BOTTOM OF THE CLOUDS, FOR
C   EACH BAND N. THE REQUIRED QUANTITIES ARE:
C      TTD(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
C      TTD(I,KBTMSW(I,K),N)  K RUNS FROM 2 TO NCLDS(I)+1:
C      TTU(I,KTOPSW(I,K),N)  K RUNS FROM 1 TO NCLDS(I)+1:
C      AND INVERSES OF THE ABOVE. THE ABOVE QUANTITIES ARE STORED
C      IN TDCL1,TDCL2,TUCL1,AND DFNTRN,UFNTRN,RESPECTIVELY, AS
C      THEY HAVE MULTIPLE USE IN THE PGM.
C---FOR FIRST CLOUD LAYER (GROUND) TDCL1,TUCL1 ARE KNOWN:
        DO 830 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 830
          TDCL1 (I,1) = TTD(I,LP1)
          TUCL1 (I,1) = TTU(I,LP1)
          TDCL2 (I,1) = TDCL1(I,1)
          DFNTRN(I,1) = ONE/TDCL1(I,1)
          UFNTRN(I,1) = DFNTRN(I,1)
830     CONTINUE
        DO 841 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 841
        DO 840 KK=2,KCLDS+1
          TDCL1(I,KK) = TTD(I,KTOPSW(I,KK))
          TUCL1(I,KK) = TTU(I,KTOPSW(I,KK))
          TDCL2(I,KK) = TTD(I,KBTMSW(I,KK))
840     CONTINUE
841     CONTINUE
        DO 851 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 851
        DO 850 KK=2,KCLDS+1
          DFNTRN(I,KK) = ONE/TDCL1(I,KK)
          UFNTRN(I,KK) = ONE/TUCL1(I,KK)
850     CONTINUE
851     CONTINUE
        DO 861 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 861
        DO 860 KK=1,KCLDS
          TCLU(I,KK) = TDCL1(I,KK)*DFNTRN(I,KK+1)*CT(I,KK+1)
          TCLD(I,KK) = TDCL1(I,KK)/TDCL2(I,KK+1)
860     CONTINUE
861     CONTINUE
C***THE FOLLOWING IS THE RECURSION RELATION FOR ALFA: THE REFLECTION
C   COEFFICIENT FOR A SYSTEM INCLUDING THE CLOUD IN QUESTION AND THE
C   FLUX COMING OUT OF THE CLOUD SYSTEM INCLUDING ALL CLOUDS BELOW
C   THE CLOUD IN QUESTION.
        DO 870 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 870
          ALFA (I,1) = CR(I,1)
          ALFAU(I,1) = ZERO
870     CONTINUE
C---AGAIN,EXCESSIVE CALCULATIONS-MAY BE CHANGED LATER!
        DO 881 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 881
        DO 880 KK=2,KCLDS+1
          ALFAU(I,KK) = TCLU(I,KK-1)*TCLU(I,KK-1)*ALFA(I,KK-1)/(ONE -
     1             TCLD(I,KK-1)*TCLD(I,KK-1)*ALFA(I,KK-1)*CR(I,KK))
          ALFA (I,KK) = ALFAU(I,KK)+CR(I,KK)
880     CONTINUE
881     CONTINUE
C     CALCULATE UFN AT CLOUD TOPS AND DFN AT CLOUD BOTTOMS
C---NOTE THAT UFNCLU(I,KCLDS+1) GIVES THE UPWARD FLUX AT THE TOP
C   OF THE HIGHEST REAL CLOUD (IF NCLDS(I)=KCLDS). IT GIVES THE FLUX
C   AT THE TOP OF THE ATMOSPHERE IF NCLDS(I) < KCLDS. IT THE FIRST
C   CASE, TDCL1 EQUALS THE TRANSMISSION FCTN TO THE TOP OF THE
C   HIGHEST CLOUD, AS WE WANT. IN THE SECOND CASE, TDCL1=1, SO UFNCLU
C   EQUALS ALFA. THIS IS ALSO CORRECT.
        DO 890 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 890
          UFNCLU(I,KCLDS+1) = ALFA(I,KCLDS+1)*TDCL1(I,KCLDS+1)
          DFNCLU(I,KCLDS+1) = TDCL1(I,KCLDS+1)
890     CONTINUE
        DO 901 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 901
        DO 900 KK=KCLDS,1,-1
          UFNCLU(I,KK)=UFNCLU(I,KK+1)*ALFAU(I,KK+1)/(ALFA(I,KK+1)*
     1                 TCLU(I,KK))
          DFNCLU(I,KK) = UFNCLU(I,KK)/ALFA(I,KK)
900     CONTINUE
901     CONTINUE
C     NOW OBTAIN DFN AND UFN FOR LEVELS BETWEEN THE CLOUDS
        DO 911 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 911
        DO 910 KK=1,KCLDS+1
          UFNTRN(I,KK) = UFNCLU(I,KK)*UFNTRN(I,KK)
          DFNTRN(I,KK) = DFNCLU(I,KK)*DFNTRN(I,KK)
910     CONTINUE
911     CONTINUE
        DO 930 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 930
          J2=KBTMSW(I,2)
          DO 920 K=J2,LP1
            UFN(I,K) = UFNTRN(I,1)*TTU(I,K)
            DFN(I,K) = DFNTRN(I,1)*TTD(I,K)
920       CONTINUE
930     CONTINUE
        DO 970  I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 970
        DO 965  KK=2,KCLDS+1
          J1 = KTOPSW(I,KK)
          J2 = KBTMSW(I,KK+1)
          IF (J1.EQ.1) GO TO 965
          DO 940 K=J2,J1
            UFN(I,K) = UFNTRN(I,KK)*TTU(I,K)
            DFN(I,K) = DFNTRN(I,KK)*TTD(I,K)
940       CONTINUE
          J3 = KBTMSW(I,KK)
          IF ((J3-J1).GT.1) THEN
            TEMPF = (UFNCLU(I,KK)-UFN(I,J3))*DPCLD(I,KK-1)
            TEMPG = (DFNCLU(I,KK)-DFN(I,J3))*DPCLD(I,KK-1)
            DO 950 K=J1+1,J3-1
              UFN(I,K) = UFNCLU(I,KK)+TEMPF*(PP(I,K)-PPTOP(I,KK-1))
              DFN(I,K) = DFNCLU(I,KK)+TEMPG*(PP(I,K)-PPTOP(I,KK-1))
950         CONTINUE
          ENDIF
965     CONTINUE
970     CONTINUE
        DO 980 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 980
        DO 981 K=1,LP1
          DFSWC(I,K) = DFSWC(I,K) + DFN(I,K)*DFNTOP(I,N)
          UFSWC(I,K) = UFSWC(I,K) + UFN(I,K)*DFNTOP(I,N)
981     CONTINUE
980     CONTINUE
        DO 990 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 990
          GDFND(I) = GDFND(I) + CCMAX(I)*DFN(I,LP1)*DFNTOP(I,N)
990     CONTINUE
1000  CONTINUE
      DO 1100 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1100
      DO 1101 K=1,LP1
        DFSWC(I,K) = TMP1(I)*DFSWL(I,K) + CCMAX(I)*DFSWC(I,K)
        UFSWC(I,K) = TMP1(I)*UFSWL(I,K) + CCMAX(I)*UFSWC(I,K)
1101  CONTINUE
1100  CONTINUE
      DO 1200 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1200
        DO 1201 KK=1,LP1
        FSWC(I,KK) = UFSWC(I,KK)-DFSWC(I,KK)
1201    CONTINUE
1200  CONTINUE
      DO 1250 I=MYIS,MYIE
        KCLDS=NCLDS(I)
        IF(KCLDS.EQ.0) GO TO 1250
        DO 1251 KK=1, L
        HSWC(I,KK) = RADCON*(FSWC(I,KK+1)-FSWC(I,KK))/DP(I,KK)
1251    CONTINUE
1250  CONTINUE
      RETURN
      END
