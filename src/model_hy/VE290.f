      SUBROUTINE E290(EMISSB,EMISS,AVEPHI,KLEN,FXOE2,DTE2)
C
C     SUBROUTINE E290 COMPUTES THE EXCHANGE TERMS IN THE FLUX EQUATION
C  FOR LONGWAVE RADIATION FOR ALL TERMS EXCEPT THE EXCHANGE WITH THE
C  TOP OF THE ATMOSPHERE. THE METHOD IS A TABLE LOOKUP ON A PRE-
C  COMPUTED E2 FUNCTION (DEFINED IN REF. (4)).
C     CALCULATIONS ARE DONE IN THE FREQUENCY RANGE:
C       1) 0-560,1200-2200 CM-1   FOR Q(APPROX)
C  MOTIVATION FOR THESE CALCULATIONS IS IN REFERENCES (1) AND (4).
C       INPUTS:                    (COMMON BLOCKS)
C     TABLE1,TABLE2,TABLE3,            TABCOM
C     AVEPHI                           TFCOM
C     FXOE2,DTE2,KLEN           ARGUMENT LIST
C       OUTPUTS:
C     EMISS,EMISSB                     TFCOM
C
C        CALLED BY :     FST88
C        CALLS     :
C
C
      COMMON/PHYCON/AMOLWT,CSUBP,DIFFCTR,G,GRAVDR,O3DIFCTR,P0,
     *            P0XZP2,P0XZP8,P0X2,RADCON,RGAS,RGASSP,SECPDA
      COMMON/PHYCON/RATCO2MW,RATH2OMW
      COMMON/PHYCON/RADCON1
      COMMON/PHYCON/GINV,P0INV,GP0INV
      COMMON/HCON/HUNDRED,HNINETY,SIXTY,FIFTY,TEN,EIGHT,FIVE,
     *            FOUR,THREE,TWO,ONE,HAF,QUARTR,ZERO
      COMMON/HCON/H83E26,H71E26,H1E15,H1E13,H1E11,H1E8,H4E5,
     *            H165E5,H5725E4,H488E4,H1E4,H24E3,H20788E3,
     *            H2075E3,H1224E3,H5E2,H3082E2,H3E2,H2945E2,
     *            H23E2,H15E2,H35E1,H3P6,H181E1,H18E1,H2P9,H2P8,
     *            H2P5,H1P8,H1P4387,H1P4,H1P25892,HP8,HP518,
     *            HP369,HP1
      COMMON/HCON/H44871M2,H559M3,H1M3,H987M4,H285M4,H1M4,
     *            H6938M5,H394M5,H37412M5,H1439M5,H128M5,H1M5,
     *            H7M6,H4999M6,H25452M6,H1M6,H391M7,H1174M7,
     *            H8725M8,H327M8,H257M8,H1M8,H23M10,H14M10,
     *            H11M10,H1M10,H83M11,H82M11,H8M11,H77M11,
     *            H72M11,H53M11,H48M11,H44M11,H42M11,H37M11,
     *            H35M11,H32M11,H3M11,H28M11,H24M11,H23M11,
     *            H2M11,H18M11,H15M11,H14M11,H114M11,H11M11,
     *            H1M11,H96M12,H93M12,H77M12,H74M12,H65M12,
     *            H62M12,H6M12,H45M12,H44M12,H4M12,H38M12,
     *            H37M12,H3M12,H29M12,H28M12,H24M12,H21M12,
     *            H16M12,H14M12,H12M12,H8M13,H46M13,H36M13,
     *            H135M13,H12M13,H1M13,H3M14,H15M14,H14M14,
     *            H1M17,H1M18,H1M19,H1M20,H1M21,H1M22,H1M23,
     *            H1M24,H26M30,H14M30,H25M31,H21M31,H12M31,
     *            H9M32,H55M32,H45M32,H4M33,H62M34,H1M60
      COMMON/HCON/HMP575,HM13EZ,HM19EZ,HM1E1,HM181E1,HM1E2
      COMMON/HCON/H1E6,H2E6,H1M2,HMP66667,HM6666M2,HP166666,
     *            H41666M2,HMP5,HM2M2,H29316E2,H1226E1,H3116E1,
     *            H9P94,HP6,H625M2,HP228,HP60241,HM1797E1,
     *            H8121E1,H2E2,HM1EZ,H26E2,H44194M2,H1P41819
      COMMON/HCON/HP219,HP144,HP816,H69766E5,H235M3,HP26,
     *            H129M2,H75826M4,H1P082,HP805,H1386E2,
     *            H658M2,H1036E2,H2118M2,H42M2,H323M4,
     *            H67390E2,HP3795,HP5048,H102M5,H451M6
      COMMON/HCON/H16E1,HM161E1,H161E1,H3M3,H101M16,
     *            HM1597E1,H25E2,HP118666,H15M5,H3P5,H18E3,
     *            H6P08108,HMP805,HP602409,HP526315,
     *            H28571M2,H1M16
      COMMON/HCON/H3M4
      COMMON/HCON/HM8E1
      COMMON/HCON/H28E1
C-----------------------------------------------------------------------
       INCLUDE "parmeta"
       INCLUDE "mpp.h"
 
 
C-----------------------------------------------------------------------
C     PARAMETER SETTINGS FOR THE LONGWAVE AND SHORTWAVE RADIATION CODE:
C          IMAX   =  NO. POINTS ALONG THE LAT. CIRCLE USED IN CALCS.
C          L      =  NO. VERTICAL LEVELS (ALSO LAYERS) IN MODEL
C***NOTE: THE USER NORMALLY WILL MODIFY ONLY THE IMAX AND L PARAMETERS
C          NBLW   =  NO. FREQ. BANDS FOR APPROX COMPUTATIONS. SEE
C                      BANDTA FOR DEFINITION
C          NBLX   =  NO. FREQ BANDS FOR APPROX CTS COMPUTATIONS
C          NBLY   =  NO. FREQ. BANDS FOR EXACT CTS COMPUTATIONS. SEE
C                      BDCOMB FOR DEFINITION
C          INLTE  =  NO. LEVELS USED FOR NLTE CALCS.
C          NNLTE  =  INDEX NO. OF FREQ. BAND IN NLTE CALCS.
C          NB,KO2 ARE SHORTWAVE PARAMETERS; OTHER QUANTITIES ARE DERIVED
C                    FROM THE ABOVE PARAMETERS.
      PARAMETER (L=LM)
      PARAMETER (IMAX=IM,NCOL=IMAX)
      PARAMETER (NBLW=163,NBLX=47,NBLY=15)
      PARAMETER (NBLM=NBLY-1)
      PARAMETER (LP1=L+1,LP2=L+2,LP3=L+3)
      PARAMETER (LM1=L-1,LM2=L-2,LM3=L-3)
      PARAMETER (LL=2*L,LLP1=LL+1,LLP2=LL+2,LLP3=LL+3)
      PARAMETER (LLM1=LL-1,LLM2=LL-2,LLM3=LL-3)
      PARAMETER (LP1M=LP1*LP1,LP1M1=LP1M-1)
      PARAMETER (LP1V=LP1*(1+2*L/2))
      PARAMETER (LP121=LP1*NBLY)
      PARAMETER (LL3P=3*L+2)
      PARAMETER (NB=12)
      PARAMETER (INLTE=3,INLTEP=INLTE+1,NNLTE=56)
      PARAMETER (LP1I=IMAX*LP1,LLP1I=IMAX*LLP1,LL3PI=IMAX*LL3P)
      PARAMETER (NB1=NB-1)
      PARAMETER (KO2=12)
      PARAMETER (KO21=KO2+1,KO2M=KO2-1)
C     PARAMETER SETTINGS FOR THE LONGWAVE AND SHORTWAVE RADIATION CODE:
C          IMAX   =  NO. POINTS SENT TO RADFS
C          L      =  NO. VERTICAL LEVELS (ALSO LAYERS) IN MODEL
C***NOTE: THE USER NORMALLY WILL MODIFY ONLY THE IMAX AND L PARAMETERS
C          NBLW   =  NO. FREQ. BANDS FOR APPROX COMPUTATIONS. SEE
C                      BANDTA FOR DEFINITION
C          NBLX   =  NO. FREQ BANDS FOR APPROX CTS COMPUTATIONS
C          NBLY   =  NO. FREQ. BANDS FOR EXACT CTS COMPUTATIONS. SEE
C                      BDCOMB FOR DEFINITION
C          INLTE  =  NO. LEVELS USED FOR NLTE CALCS.
C          NNLTE  =  INDEX NO. OF FREQ. BAND IN NLTE CALCS.
C          NB,KO2 ARE SHORTWAVE PARAMETERS; OTHER QUANTITIES ARE DERIVED
C                    FROM THE ABOVE PARAMETERS.
C     COMMON BLOCK TABCOM CONTAINS QUANTITIES PRECOMPUTED IN SUBROUTINE
C     TABLE FOR USE IN THE LONGWAVE RADIATION PROGRAM:
C          EM1     =  E1 FUNCTION, EVALUATED OVER THE 0-560 AND
C                     1200-2200 CM-1 INTERVALS
C          EM1WDE  =  E1 FUNCTION, EVALUATED OVER THE 160-560 CM-1
C                     INTERVAL
C          TABLE1  =  E2 FUNCTION, EVALUATED OVER THE 0-560 AND
C                     1200-2200 CM-1 INTERVALS
C          TABLE2  =  TEMPERATURE DERIVATIVE OF TABLE1
C          TABLE3  =  MASS DERIVATIVE OF TABLE1
C          EM3     =  E3 FUNCTION, EVALUATED OVER THE 0-560 AND
C                     1200-2200 CM-1 INTERVALS
C          SOURCE  =  PLANCK FUNCTION, EVALUATED AT SPECIFIED TEMPS. FOR
C                     BANDS USED IN CTS CALCULATIONS
C          DSRCE   =  TEMPERATURE DERIVATIVE OF SOURCE
C          IND     =  INDEX, WITH VALUE IND(I)=I. USED IN FST88
C          INDX2   =  INDEX VALUES USED IN OBTAINING "LOWER TRIANGLE"
C                     ELEMENTS OF AVEPHI,ETC.,IN FST88
C          KMAXV   =  INDEX VALUES USED IN OBTAINING "UPPER TRIANGLE"
C                     ELEMENTS OF AVEPHI,ETC.,IN FST88
C          KMAXVM  =  KMAXV(L),USED FOR DO LOOP INDICES
C
      COMMON / TABCOM / IND(IMAX),INDX2(LP1V),KMAXV(LP1),
     1     KMAXVM
      COMMON/TABCOM/EM1(28,180),EM1WDE(28,180),TABLE1(28,180),
     1 TABLE2(28,180),TABLE3(28,180),EM3(28,180),SOURCE(28,NBLY),
     2 DSRCE(28,NBLY)
C
      DIMENSION EMISSB(IDIM1:IDIM2,LP1),EMISS(IDIM1:IDIM2,LP1),
     & AVEPHI(IDIM1:IDIM2,LP1)
      DIMENSION IVAL(IDIM1:IDIM2,LP1),
     1  DT(IDIM1:IDIM2,LP1),FYO(IDIM1:IDIM2,LP1),DU(IDIM1:IDIM2,LP1)
C---TMP3 MAY BE EQUIVALENCED TO DT IN VTEMP
      DIMENSION TMP3(IDIM1:IDIM2,LP1)
C---VARIABLES EQUIVALENCED TO COMMON BLOCK VARIABLES
      DIMENSION T1(5040),T2(5040),T4(5040)
C---VARIABLES IN THE ARGUMENT LIST
      DIMENSION FXOE2(IDIM1:IDIM2,LP1),DTE2(IDIM1:IDIM2,LP1)
C
      EQUIVALENCE (TMP3,DT)
      EQUIVALENCE (T1(1),TABLE1(1,1)),(T2(1),TABLE2(1,1)),
     1 (T4(1),TABLE3(1,1))
C---FIRST WE OBTAIN THE EMISSIVITIES AS A FUNCTION OF TEMPERATURE
C   (INDEX FXO) AND WATER AMOUNT (INDEX FYO). THIS PART OF THE CODE
C   THUS GENERATES THE E2 FUNCTION.
C
C---CALCULATIONS FOR VARYING KP (FROM KP=K+1 TO LP1, INCLUDING SPECIAL
C   CASE: RESULTS ARE IN EMISS
      DO 132 K=1,LP2-KLEN
      DO 132 I=MYIS,MYIE
      TMP3(I,K)=LOG10(AVEPHI(I,KLEN+K-1))+H16E1
      FYO(I,K)=AINT(TMP3(I,K)*TEN)
      DU(I,K)=TMP3(I,K)-HP1*FYO(I,K)
      FYO(I,K)=H28E1*FYO(I,K)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,KLEN+K-1)
      EMISS(I,KLEN+K-1)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K))
     1                           +DTE2(I,KLEN+K-1)*T4(IVAL(I,K))
132   CONTINUE
C---THE SPECIAL CASE EMISS(I,L) (LAYER KP) IS OBTAINED NOW
C   BY AVERAGING THE VALUES FOR L AND LP1:
      DO 1344 I=MYIS,MYIE
      EMISS(I,L)=HAF*(EMISS(I,L)+EMISS(I,LP1))
1344  CONTINUE
C---NOTE THAT EMISS(I,LP1) IS NOT USEFUL AFTER THIS POINT.
C
C---CALCULATIONS FOR KP=KLEN AND VARYING K; RESULTS ARE IN EMISSB.
C  IN THIS CASE, THE TEMPERATURE INDEX IS UNCHANGED, ALWAYS BEING
C  FXO(I,KLEN-1); THE WATER INDEX CHANGES, BUT IS SYMMETRICAL WITH
C  THAT FOR THE VARYING KP CASE.NOTE THAT THE SPECIAL CASE IS NOT
C  INVOLVED HERE.
C     (FIXED LEVEL) K VARIES FROM (KLEN+1) TO LP1; RESULTS ARE IN
C   EMISSB(I,(KLEN) TO L)
      DO 142 K=1,LP1-KLEN
      DO 142 I=MYIS,MYIE
      DT(I,K)=DTE2(I,KLEN-1)
      IVAL(I,K)=FYO(I,K)+FXOE2(I,KLEN-1)
142   CONTINUE
C
      DO 234 K=1,LP1-KLEN
      DO 234 I=MYIS,MYIE
      EMISSB(I,KLEN+K-1)=T1(IVAL(I,K))+DU(I,K)*T2(IVAL(I,K))
     1                                +DT(I,K)*T4(IVAL(I,K))
234   CONTINUE
      RETURN
      END
