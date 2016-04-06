      SUBROUTINE TABLE
C     SUBROUTINE TABLE COMPUTES TABLE ENTRIES USED IN THE LONGWAVE RADIA
C     PROGRAM. ALSO CALCULATED ARE INDICES USED IN STRIP-MINING AND FOR
C     SOME PRE-COMPUTABLE FUNCTIONS.
C         INPUTS:
C         OUTPUTS:
C       EM1,EM1WDE,TABLE1,TABLE2,TABLE3         TABCOM
C       EM3,SOURCE,DSRCE,IND,INDX2,KMAXV        TABCOM
C       KMAXVM,                                 TABCOM
C       AO3RND,BO3RND,AB15                      BANDTA
C       AB15WD,SKC1R,SKO3R,SKO2D                BDWIDE
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
C    COMMON BLOCK BANDTA CONTAINS RANDOM BAND PARAMETERS FOR THE LW
C    CALCULATIONS USING 10 CM-1 WIDE BANDS.THE 15 UM CO2 COMPLEX
C    IS 2 BANDS,560-670 AND 670-800 CM-1. OZONE COEFFICIENTS ARE
C    IN 3 BANDS,670-800 (14.1 UM),990-1070 AND 1070-1200 (9.6 UM).
C    THE  (NBLW) BANDS NOW INCLUDE:
C                56 BANDS, 10  CM-1 WIDE    0  -   560  CM-1
C                 2 BANDS, 15 UM COMPLEX  560  -   670  CM-1
C                                         670  -   800  CM-1
C                 3 "CONTINUUM" BANDS     800  -   900  CM-1
C                                         900  -   990  CM-1
C                                        1070  -   1200 CM-1
C                 1 BAND FOR 9.6 UM BAND  990  -   1070 CM-1
C               100 BANDS, 10 CM-1 WIDE  1200  -   2200 CM-1
C                 1 BAND FOR 4.3 UM SRC  2270  -   2380 CM-1
C    THUS NBLW PRESENTLY EQUALS    163
C    ALL BANDS ARE ARRANGED IN ORDER OF INCREASING WAVENUMBER
C
C        ARNDM   =   RANDOM "A" PARAMETER FOR (NBLW) BANDS
C        BRNDM   =   RANDOM "B" PARAMETER FOR (NBLW) BANDS
C        BETAD   =   CONTINUUM COEFFICIENTS FOR (NBLW) BANDS
C        AP,BP   =   CAPPHI COEFFICIENTS FOR (NBLW) BANDS
C        ATP,BTP =   CAPPSI COEFFICIENTS FOR (NBLW) BANDS
C        BANDLO  =   LOWEST FREQUENCY IN EACH OF (NBLW) FREQ. BANDS
C        BANDHI  =   HIGHEST FREQUENCY IN EACH OF (NBLW) FREQ. BANDS
C        AO3RND  =   RANDOM "A" PARAMETER FOR OZONE IN (3) OZONE
C                    BANDS
C        BO3RND  =   RANDOM "B" PARAMETER FOR OZONE IN (3) OZONE
C                    BANDS
C        AB15    =   THE PRODUCT ARNDM*BRNDM FOR THE TWO BANDS
C                    REPRESENTING THE 15 UM BAND COMPLEX OF CO2
C     DATA FOR ARNDM,BRNDM,AP,BP,ATP,BTP,AO3RND,BO3RND ARE OBTAINED BY
C     USING THE AFGL 1982 CATALOG. CONTINUUM COEFFICIENTS ARE FROM
C     ROBERTS (1976).
      COMMON / BANDTA / ARNDM(NBLW),BRNDM(NBLW),BETAD(NBLW),AP(NBLW),
     1                  BP(NBLW),ATP(NBLW),BTP(NBLW),BANDLO(NBLW),
     2                  BANDHI(NBLW),AO3RND(3),BO3RND(3),AB15(2)
C
C    COMMON BLOCK BDWIDE CONTAINS RANDOM BAND PARAMETERS FOR SPECIFIC
C    WIDE BANDS. AT PRESENT,THE INFORMATION CONSISTS OF 1) RANDOM
C    MODEL PARAMETERS FOR THE 15 UM BAND,560-800 CM-1; 2) THE
C    CONTINUUM COEFFICIENT FOR THE 800-990,1070-1200 CM-1 BAND
C        SPECIFICALLY:
C        AWIDE       =   RANDOM "A" PARAMETER FOR  BAND
C        BWIDE       =   RANDOM "B" PARAMETER FOR  BAND
C        BETAWD      =   CONTINUUM COEFFICIENTS FOR BAND
C        APWD,BPWD   =   CAPPHI COEFFICIENTS FOR  BAND
C        ATPWD,BTPWD =   CAPPSI COEFFICIENTS FOR BAND
C        BDLOWD      =   LOWEST FREQUENCY IN EACH  FREQ  BAND
C        BDHIWD      =   HIGHEST FREQUENCY IN EACH FREQ  BAND
C        AB15WD      =   THE PRODUCT ARNDM*BRNDM FOR THE ONE BAND
C                        REPRESENTING THE 15 UM BAND COMPLEX OF CO2
C        BETINW      =   CONT.COEFFICIENT FOR A SPECIFIED WIDE
C                        FREQ.BAND (800-990 AND 1070-1200 CM-1).
C        SKO2D       =   1./BETINW, USED IN SPA88 FOR CONT. COEFFS
C        SKC1R       =   BETAWD/BETINW, USED FOR CONT. COEFF. FOR
C                        15 UM BAND IN FST88
C        SKO3R       =   RATIO OF CONT. COEFF. FOR 9.9 UM BAND TO
C                        BETINW, USED FOR 9.6 UM CONT COEFF IN FST88
C     DATA FOR AWIDE,BWIDE,APWD,BPWD,ATPWD,BTPWD,AO3WD,BO3WD ARE
C     OBTAINED BY USING THE AFGL 1982 CATALOG. CONTINUUM COEFFICIENTS
C     ARE FROM ROBERTS (1976).
      COMMON / BDWIDE / AWIDE,BWIDE,BETAWD,
     1                  APWD,BPWD,ATPWD,BTPWD,
     2                  BDLOWD,BDHIWD,BETINW,
     3                  AB15WD,SKO2D,SKC1R,SKO3R
C
C    COMMON BLOCK BDCOMB CONTAINS RANDOM BAND PARAMETERS FOR THE LW
C    CALCULATIONS USING COMBINED WIDE FREQUENCY BANDS BETWEEN 160 AND
C    1200 CM-1,AS WELL AS THE 2270-2380 BAND FOR SOURCE CALC.
C        BANDS 1-8: COMBINED WIDE FREQUENCY BANDS FOR 160-560 CM-1
C        BANDS 9-14: FREQUENCY BANDS,AS IN BANDTA (NARROW BANDS)
C                    FOR 560-1200 CM-1
C        BAND  15:  FREQUENCY BAND 2270-2380 CM-1,USED FOR SOURCE
C                   CALCULATION ONLY
C        THUS NBLY PRESENTLY EQUALS   15
C
C        BANDS ARE ARRANGED IN ORDER OF INCREASING WAVENUMBER
C        ACOMB       =   RANDOM "A" PARAMETER FOR (NBLY) BANDS
C        BCOMB       =   RANDOM "B" PARAMETER FOR (NBLY) BANDS
C        BETACM      =   CONTINUUM COEFFICIENTS FOR (NBLY) BANDS
C        APCM,BPCM   =   CAPPHI COEFFICIENTS FOR (NBLY) BANDS
C        ATPCM,BTPCM =   CAPPSI COEFFICIENTS FOR (NBLY) BANDS
C        BDLOCM      =   LOWEST FREQUENCY IN EACH OF (NBLY) FREQ. BANDS
C        BDHICM      =   HIGHEST FREQUENCY IN EACH OF (NBLY) FREQ. BANDS
C        AO3CM       =   RANDOM "A" PARAMETER FOR OZONE IN (3) OZONE
C                        BANDS
C        BO3CM       =   RANDOM "B" PARAMETER FOR OZONE IN (3) OZONE
C                        BANDS
C        AB15CM      =   THE PRODUCT ARNDM*BRNDM FOR THE TWO BANDS
C                        REPRESENTING THE 15 UM BAND COMPLEX OF CO2
C        BETINC      =   CONT.COEFFICIENT FOR A SPECIFIED WIDE
C                        FREQ.BAND (800-990 AND 1070-1200 CM-1).
C        IBAND       =   INDEX NO OF THE 40 WIDE BANDS USED IN
C                        COMBINED WIDE BAND CALCULATIONS. IN OTHER
C                        WORDS,INDEX TELLING WHICH OF THE 40 WIDE
C                        BANDS BETWEEN 160-560 CM-1 ARE INCLUDED IN
C                        EACH OF THE FIRST 8 COMBINED WIDE BANDS
C     DATA FOR ACOMB,BCOMB,APCM,BPCM,ATPCM,BTPCM,AO3CM,BO3CM ARE
C     OBTAINED BY USING THE AFGL 1982 CATALOG. CONTINUUM COEFFICIENTS
C     ARE FROM ROBERTS (1976). IBAND INDEX VALUES ARE OBTAINED BY
C     EXPERIMENTATION.
      COMMON / BDCOMB / IBAND(40),ACOMB(NBLY),BCOMB(NBLY),
     1                  BETACM(NBLY),APCM(NBLY),BPCM(NBLY),ATPCM(NBLY),
     2                  BTPCM(NBLY),BDLOCM(NBLY),BDHICM(NBLY),BETINC,
     3                  AO3CM(3),BO3CM(3),AB15CM(2)
C
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
C% #NPADL = #PAGE*#NPAGE -  4*28*180  -  2*181 - 7*28 - 180 ;
C% #NPADL = #NPADL       -  11*28  - 2*180 - 2*30 ;
C     PARAMETER (NPADL = #NPADL - 28*NBLX - 2*28*NBLW - 7*NBLW)
      COMMON /SCRTCH/ SUM(28,180),PERTSM(28,180),SUM3(28,180),
     1 SUMWDE(28,180),SRCWD(28,NBLX),SRC1NB(28,NBLW),DBDTNB(28,NBLW)
      COMMON /SCRTCH/ ZMASS(181),ZROOT(181),SC(28),DSC(28),XTEMV(28),
     1 TFOUR(28),FORTCU(28),X(28),X1(28),X2(180),SRCS(28),
     2 SUM4(28),SUM6(28),SUM7(28),SUM8(28),SUM4WD(28),
     3 R1(28),R2(28),S2(28),T3(28),R1WD(28)
      COMMON /SCRTCH/ EXPO(180),FAC(180)
      COMMON/SCRTCH/CNUSB(30),DNUSB(30)
      COMMON/SCRTCH/ALFANB(NBLW),AROTNB(NBLW)
      COMMON/SCRTCH/ANB(NBLW),BNB(NBLW),CENTNB(NBLW),DELNB(NBLW),
     1          BETANB(NBLW)
C     COMMON/SCRTCH/PADLOC(NPADL)
      COMMON/TBLTMP/ DELCM(NBLY)
C****************************************
C***COMPUTE LOCAL QUANTITIES AND AO3,BO3,AB15
C....FOR NARROW-BANDS...
      DO 101 N=1,NBLW
      ANB(N)=ARNDM(N)
      BNB(N)=BRNDM(N)
      CENTNB(N)=HAF*(BANDLO(N)+BANDHI(N))
      DELNB(N)=BANDHI(N)-BANDLO(N)
      BETANB(N)=BETAD(N)
101   CONTINUE
      AB15(1)=ANB(57)*BNB(57)
      AB15(2)=ANB(58)*BNB(58)
C....FOR WIDE BANDS...
      AB15WD=AWIDE*BWIDE
C
C***COMPUTE INDICES: IND,INDX2,KMAXV
      DO 111 I=1,IMAX
      IND(I)=I
111   CONTINUE
      ICNT=0
      DO 113 I1=1,L
        I2E=LP1-I1
        DO 115 I2=1,I2E
          ICNT=ICNT+1
          INDX2(ICNT)=LP1*(I2-1)+LP2*I1
115     CONTINUE
113   CONTINUE
      KMAXV(1)=1
      DO 117 I=2,L
      KMAXV(I)=KMAXV(I-1)+(LP2-I)
117   CONTINUE
      KMAXVM=KMAXV(L)
C***COMPUTE RATIOS OF CONT. COEFFS
      SKC1R=BETAWD/BETINW
      SKO3R=BETAD(61)/BETINW
      SKO2D=ONE/BETINW
C
C****BEGIN TABLE COMPUTATIONS HERE***
C***COMPUTE TEMPS, MASSES FOR TABLE ENTRIES
C---NOTE: THE DIMENSIONING AND INITIALIZATION OF XTEMV AND OTHER ARRAYS
C   WITH DIMENSION OF 28 IMPLY A RESTRICTION OF MODEL TEMPERATURES FROM
C   100K TO 370K.
C---THE DIMENSIONING OF ZMASS,ZROOT AND OTHER ARRAYS WITH DIMENSION OF
C   180 IMPLY A RESTRICTION OF MODEL H2O AMOUNTS SUCH THAT OPTICAL PATHS
C   ARE BETWEEN 10**-16 AND 10**2, IN CGS UNITS.
      ZMASS(1)=H1M16
      DO 201 J=1,180
      JP=J+1
      ZROOT(J)=SQRT(ZMASS(J))
      ZMASS(JP)=ZMASS(J)*H1P25892
201   CONTINUE
      DO 203 I=1,28
      XTEMV(I)=HNINETY+TEN*I
      TFOUR(I)=XTEMV(I)*XTEMV(I)*XTEMV(I)*XTEMV(I)
      FORTCU(I)=FOUR*XTEMV(I)*XTEMV(I)*XTEMV(I)
203   CONTINUE
C******THE COMPUTATION OF SOURCE,DSRCE IS  NEEDED ONLY
C   FOR THE COMBINED WIDE-BAND CASE.TO OBTAIN THEM,THE SOURCE
C   MUST BE COMPUTED FOR EACH OF THE (NBLX) WIDE BANDS(=SRCWD)
C   THEN COMBINED (USING IBAND) INTO SOURCE.
      DO 205 N=1,NBLY
      DO 205 I=1,28
      SOURCE(I,N)=ZERO
205   CONTINUE
      DO 207 N=1,NBLX
      DO 207 I=1,28
      SRCWD(I,N)=ZERO
207   CONTINUE
C---BEGIN FREQ. LOOP (ON N)
      DO 211 N=1,NBLX
        IF (N.LE.46) THEN
C***THE 160-1200 BAND CASES
          CENT=CENTNB(N+16)
          DEL=DELNB(N+16)
          BDLO=BANDLO(N+16)
          BDHI=BANDHI(N+16)
        ENDIF
        IF (N.EQ.NBLX) THEN
C***THE 2270-2380 BAND CASE
          CENT=CENTNB(NBLW)
          DEL=DELNB(NBLW)
          BDLO=BANDLO(NBLW)
          BDHI=BANDHI(NBLW)
        ENDIF
C***FOR PURPOSES OF ACCURACY, ALL EVALUATIONS OF PLANCK FCTNS ARE MADE
C  ON 10 CM-1 INTERVALS, THEN SUMMED INTO THE (NBLX) WIDE BANDS.
      NSUBDS=(DEL-H1M3)/10+1
      DO 213 NSB=1,NSUBDS
      IF (NSB.NE.NSUBDS) THEN
        CNUSB(NSB)=TEN*(NSB-1)+BDLO+FIVE
        DNUSB(NSB)=TEN
      ELSE
        CNUSB(NSB)=HAF*(TEN*(NSB-1)+BDLO+BDHI)
        DNUSB(NSB)=BDHI-(TEN*(NSB-1)+BDLO)
      ENDIF
      C1=(H37412M5)*CNUSB(NSB)**3
C---BEGIN TEMP. LOOP (ON I)
      DO 215 I=1,28
      X(I)=H1P4387*CNUSB(NSB)/XTEMV(I)
      X1(I)=EXP(X(I))
      SRCS(I)=C1/(X1(I)-ONE)
      SRCWD(I,N)=SRCWD(I,N)+SRCS(I)*DNUSB(NSB)
215   CONTINUE
213   CONTINUE
211   CONTINUE
C***THE FOLLOWING LOOPS CREATE THE COMBINED WIDE BAND QUANTITIES SOURCE
C   AND DSRCE
      DO 221 N=1,40
      DO 221 I=1,28
      SOURCE(I,IBAND(N))=SOURCE(I,IBAND(N))+SRCWD(I,N)
221   CONTINUE
      DO 223 N=9,NBLY
      DO 223 I=1,28
      SOURCE(I,N)=SRCWD(I,N+32)
223   CONTINUE
      DO 225 N=1,NBLY
      DO 225 I=1,27
      DSRCE(I,N)=(SOURCE(I+1,N)-SOURCE(I,N))*HP1
225   CONTINUE
      DO 231 N=1,NBLW
      ALFANB(N)=BNB(N)*ANB(N)
      AROTNB(N)=SQRT(ALFANB(N))
231   CONTINUE
C***FIRST COMPUTE PLANCK FCTNS (SRC1NB) AND DERIVATIVES (DBDTNB) FOR
C   USE IN TABLE EVALUATIONS. THESE ARE DIFFERENT FROM SOURCE,DSRCE
C   BECAUSE DIFFERENT FREQUENCY PTS ARE USED IN EVALUATION, THE FREQ.
C   RANGES ARE DIFFERENT, AND THE DERIVATIVE ALGORITHM IS DIFFERENT.
C
      DO 301 N=1,NBLW
      CENT=CENTNB(N)
      DEL=DELNB(N)
C---NOTE: AT PRESENT, THE IA LOOP IS ONLY USED FOR IA=2. THE LOOP STRUCT
C   IS KEPT SO THAT IN THE FUTURE, WE MAY USE A QUADRATURE SCHEME FOR
C   THE PLANCK FCTN EVALUATION, RATHER THAN USE THE MID-BAND FREQUENCY.
      DO 303 IA=1,3
      ANU=CENT+HAF*(IA-2)*DEL
      C1=(H37412M5)*ANU*ANU*ANU+H1M20
C---TEMPERATURE LOOP---
      DO 305 I=1,28
         X(I)=H1P4387*ANU/XTEMV(I)
         X1(I)=EXP(X(I))
         SC(I)=C1/((X1(I)-ONE)+H1M20)
         DSC(I)=SC(I)*SC(I)*X(I)*X1(I)/(XTEMV(I)*C1)
305      CONTINUE
      IF (IA.EQ.2) THEN
         DO 307 I=1,28
         SRC1NB(I,N)=DEL*SC(I)
         DBDTNB(I,N)=DEL*DSC(I)
307      CONTINUE
      ENDIF
303   CONTINUE
301   CONTINUE
C***NEXT COMPUTE R1,R2,S2,AND T3- COEFFICIENTS USED FOR E3 FUNCTION
C   WHEN THE OPTICAL PATH IS LESS THAN 10-4. IN THIS CASE, WE ASSUME A
C   DIFFERENT DEPENDENCE ON (ZMASS).
C---ALSO OBTAIN R1WD, WHICH IS R1 SUMMED OVER THE 160-560 CM-1 RANGE
      DO 311 I=1,28
      SUM4(I)=ZERO
      SUM6(I)=ZERO
      SUM7(I)=ZERO
      SUM8(I)=ZERO
      SUM4WD(I)=ZERO
311   CONTINUE
      DO 313 N=1,NBLW
      CENT=CENTNB(N)
C***PERFORM SUMMATIONS FOR FREQ. RANGES OF 0-560,1200-2200 CM-1 FOR SUM4
C   SUM6,SUM7,SUM8
      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
         DO 315 I=1,28
         SUM4(I)=SUM4(I)+SRC1NB(I,N)
         SUM6(I)=SUM6(I)+DBDTNB(I,N)
         SUM7(I)=SUM7(I)+DBDTNB(I,N)*AROTNB(N)
         SUM8(I)=SUM8(I)+DBDTNB(I,N)*ALFANB(N)
315      CONTINUE
      ENDIF
C***PERFORM SUMMATIONS OVER 160-560 CM-1 FREQ RANGE FOR E1 CALCS (SUM4WD
      IF (CENT.GT.160. .AND. CENT.LT.560.) THEN
         DO 316 I=1,28
         SUM4WD(I)=SUM4WD(I)+SRC1NB(I,N)
316      CONTINUE
      ENDIF
313   CONTINUE
      DO 317 I=1,28
      R1(I)=SUM4(I)/TFOUR(I)
      R2(I)=SUM6(I)/FORTCU(I)
      S2(I)=SUM7(I)/FORTCU(I)
      T3(I)=SUM8(I)/FORTCU(I)
      R1WD(I)=SUM4WD(I)/TFOUR(I)
317   CONTINUE
      DO 401 J=1,180
      DO 401 I=1,28
      SUM(I,J)=ZERO
      PERTSM(I,J)=ZERO
      SUM3(I,J)=ZERO
      SUMWDE(I,J)=ZERO
401   CONTINUE
C---FREQUENCY LOOP BEGINS---
      DO 411 N=1,NBLW
      CENT=CENTNB(N)
C***PERFORM CALCULATIONS FOR FREQ. RANGES OF 0-560,1200-2200 CM-1
      IF (CENT.LT.560. .OR. CENT.GT.1200..AND.CENT.LE.2200.) THEN
         DO 413 J=1,180
         X2(J)=AROTNB(N)*ZROOT(J)
         IF ( X2(J).GT.70.0 ) X2(J) = 70.0
         EXPO(J)=EXP(-X2(J))
413      CONTINUE
         DO 415 J=1,180
         IF (X2(J).GE.HUNDRED) THEN
              EXPO(J)=ZERO
         ENDIF
415      CONTINUE
         DO 417 J=121,180
         FAC(J)=ZMASS(J)*(ONE-(ONE+X2(J))*EXPO(J))/(X2(J)*X2(J))
417      CONTINUE
         DO 419 J=1,180
         DO 419 I=1,28
         SUM(I,J)=SUM(I,J)+SRC1NB(I,N)*EXPO(J)
         PERTSM(I,J)=PERTSM(I,J)+DBDTNB(I,N)*EXPO(J)
419      CONTINUE
         DO 421 J=121,180
         DO 421 I=1,28
         SUM3(I,J)=SUM3(I,J)+DBDTNB(I,N)*FAC(J)
421      CONTINUE
      ENDIF
C---COMPUTE SUM OVER 160-560 CM-1 RANGE FOR USE IN E1 CALCS (SUMWDE)
      IF (CENT.GT.160. .AND. CENT.LT.560.) THEN
         DO 420 J=1,180
         DO 420 I=1,28
         SUMWDE(I,J)=SUMWDE(I,J)+SRC1NB(I,N)*EXPO(J)
420      CONTINUE
      ENDIF
411   CONTINUE
      DO 431 J=1,180
      DO 431 I=1,28
      EM1(I,J)=SUM(I,J)/TFOUR(I)
      TABLE1(I,J)=PERTSM(I,J)/FORTCU(I)
431   CONTINUE
      DO 433 J=121,180
      DO 433 I=1,28
      EM3(I,J)=SUM3(I,J)/FORTCU(I)
433   CONTINUE
      DO 441 J=1,179
      DO 441 I=1,28
      TABLE2(I,J)=(TABLE1(I,J+1)-TABLE1(I,J))*TEN
441   CONTINUE
      DO 443 J=1,180
      DO 443 I=1,27
      TABLE3(I,J)=(TABLE1(I+1,J)-TABLE1(I,J))*HP1
443   CONTINUE
      DO 445 I=1,28
      TABLE2(I,180)=ZERO
445   CONTINUE
      DO 447 J=1,180
      TABLE3(28,J)=ZERO
447   CONTINUE
      DO 449 J=1,2
      DO 449 I=1,28
      EM1(I,J)=R1(I)
449   CONTINUE
      DO 451 J=1,120
      DO 451 I=1,28
      EM3(I,J)=R2(I)/TWO-S2(I)*SQRT(ZMASS(J))/THREE+T3(I)*ZMASS(J)/EIGHT
451   CONTINUE
      DO 453 J=121,180
      DO 453 I=1,28
      EM3(I,J)=EM3(I,J)/ZMASS(J)
453   CONTINUE
C***NOW COMPUTE E1 TABLES FOR 160-560 CM-1 BANDS ONLY.
C   WE USE R1WD AND SUMWDE OBTAINED ABOVE.
      DO 501 J=1,180
      DO 501 I=1,28
      EM1WDE(I,J)=SUMWDE(I,J)/TFOUR(I)
501   CONTINUE
      DO 503 J=1,2
      DO 503 I=1,28
      EM1WDE(I,J)=R1WD(I)
503   CONTINUE
      RETURN
      END
