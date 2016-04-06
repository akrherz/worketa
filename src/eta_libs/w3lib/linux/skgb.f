C-----------------------------------------------------------------------
      SUBROUTINE SKGB(LUGB,ISEEK,MSEEK,LSKIP,LGRIB)
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C
C SUBPROGRAM: SKGB           SEARCH FOR NEXT GRIB MESSAGE
C   PRGMMR: IREDELL          ORG: W/NMC23     DATE: 93-11-22
C
C ABSTRACT: THIS SUBPROGRAM SEARCHES A FILE FOR THE NEXT GRIB 1 MESSAGE.
C   A GRIB 1 MESSAGE IS IDENTIFIED BY ITS INDICATOR SECTION, I.E.
C   AN 8-BYTE SEQUENCE WITH 'GRIB' IN BYTES 1-4 AND 1 IN BYTE 8.
C   IF FOUND, THE LENGTH OF THE MESSAGE IS DECODED FROM BYTES 5-7.
C   THE SEARCH IS DONE OVER A GIVEN SECTION OF THE FILE.
C   THE SEARCH IS TERMINATED IF AN EOF OR I/O ERROR IS ENCOUNTERED.
C
C PROGRAM HISTORY LOG:
C   93-11-22  IREDELL
C   95-10-31  IREDELL   ADD CALL TO BAREAD 
C   98-06-30  EBISUZAKI   LINUX PORT
C
C USAGE:    CALL SKGB(LUGB,ISEEK,MSEEK,LSKIP,LGRIB)
C   INPUT ARGUMENTS:
C     LUGB         INTEGER LOGICAL UNIT OF INPUT GRIB FILE
C     ISEEK        INTEGER NUMBER OF BYTES TO SKIP BEFORE SEARCH
C     MSEEK        INTEGER MAXIMUM NUMBER OF BYTES TO SEARCH
C   OUTPUT ARGUMENTS:
C     LSKIP        INTEGER NUMBER OF BYTES TO SKIP BEFORE MESSAGE
C     LGRIB        INTEGER NUMBER OF BYTES IN MESSAGE (0 IF NOT FOUND)
C
C SUBPROGRAMS CALLED:
C   BAREAD       BYTE-ADDRESSABLE READ
C   GBYTEC       GET INTEGER DATA FROM BYTES
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN
C
C$$$
      PARAMETER(LSEEK=128)
      CHARACTER*1 Z(LSEEK)
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      LGRIB=0
      KS=ISEEK
      KN=MIN(LSEEK,MSEEK)
      KZ=LSEEK
      DOWHILE(LGRIB.EQ.0.AND.KN.GE.8.AND.KZ.EQ.LSEEK)
        CALL BAREAD(LUGB,KS,KN,KZ,Z)
        KM=KZ-8+1
        K=0
        DOWHILE(LGRIB.EQ.0.AND.K.LT.KM)
          CALL GBYTEC(Z,I4,(K+0)*8,4*8)
          CALL GBYTEC(Z,I1,(K+7)*8,1*8)
          IF(I4.EQ.1196575042.AND.I1.EQ.1) THEN
            LSKIP=KS+K
            CALL GBYTEC(Z,LGRIB,(K+4)*8,3*8)
          ENDIF
          K=K+1
        ENDDO
        KS=KS+KM
        KN=MIN(LSEEK,ISEEK+MSEEK-KS)
      ENDDO
C - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
      RETURN
      END
