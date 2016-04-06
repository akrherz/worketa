      SUBROUTINE GD2EGK(IMOT,JMOT)
C
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .     
C SUBPROGRAM:    GD2EGK      MAP OUTPUT GRID TO E-GRID
C   PRGRMMR: TREADON         ORG: W/NP2      DATE: 92-12-23
C     
C ABSTRACT:
C     THIS ROUTINE LOCATES THE (I,J) INDICES ON A SPECIFIED
C     OUTPUT GRID NEAREST TO E-GRID MASS (H) POINTS.
C   .     
C     
C PROGRAM HISTORY LOG:
C   ??-??-??  ???
C   92-12-23  RUSS TREADON - GENERALIZED CODE.
C   93-06-09  RUSS TREADON - EXPANDED COMMENTS.
C   98-06-04  BLACK - CONVERSION TO 2-D
C     
C USAGE:    GD2EGK(IMOT,JMOT)
C   INPUT ARGUMENT LIST:
C     IMOT     - FIRST DIMENSION OF OUTPUT GRID.
C     JMOT     - SECOND DIMENSION OF OUTPUT GRID.
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
C       COMMON   - LLGRDS
C                  IOUNIT
C     
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : CRAY C-90
C$$$  
C     
C     INCLUDE E- AND OTUPUT GRID DIMENSIONS.
C     COMPUTE DEPENDENT PARAMETERS.
C------------------------------------------------------------------------     
      INCLUDE "parmeta"
      INCLUDE "parmout"
C------------------------------------------------------------------------     
      PARAMETER (IMT=2*IM-1, JMT=JM)
C------------------------------------------------------------------------     
      INCLUDE "LLGRDS.comm"
      INCLUDE "IOUNIT.comm"
C     
C************************************************************************
C     START GD2EGK.
C     
      WRITE(STDOUT,*)'GD2EGK:  COMPUTE E-GRID (I,J) FOR CETLIH'
C     
C     INITIALIZE TO MINUS ONE (A FLAG) THE ARRAYS WHICH WILL CONTAIN 
C     THE OUTPUT GRID (I,J) NEAREST TO EACH E-GRID MASS POINT.
C
      DO J=1,JM
      DO I=1,IM
        IEGRDK(I,J)=-1
        JEGRDK(I,J)=-1
      ENDDO
      ENDDO
C     
C     THE MAPPING EXCLUDES THE OUTERMOST TWO ROWS AND COLUMNS ON
C     THE OUTPUT GRID.  SET UPPER INDEX LIMITS ACCORDINGLY.
C
      JMOT2=JMOT-2
      IMOT2=IMOT-2
C     
C     LOOP OVER THE E-GRID.  THE LOOP ON J MOVES US SOUTH TO NORTH
C     ON THE E-GRID.  THE LOOP ON K MOVES US WEST TO EAST ACROSS 
C     THE J-TH ROW ON THE E-GRID.
C
      DO 175 J=3,JM-2
      IEND=IM-1-MOD(J+1,2)
C
      DO 175 I=2,IEND
C     
C     SET THE MASS POINT TRANSFORMED LATITUDE AND LONGITUDE
C     FOR THE K-TH E-GRID MASS POINT ON ROW J.
C
      CLON=HTLON(I,J)
      CLAT=HTLAT(I,J)
      DLAT=0.
C     
C           LOOP OVER THE OUTPUT GRID.  FOR THE CURRENT 
C           (I.E., K-TH) E-GRID MASS POINT WE WANT TO 
C           LOCATE THE NEAREST OUTPUT GRID (I,J) POINT.
C           NOTE THAT THE (LAT,LON) OF THE OUTPUT GRID HAVE 
C           BEEN TRANSFORMED TO THE E-GRID REFERENCE FRAME.
C     
 110  DO 130 JJ=2,JMOT2
      DO 120 II=2,IMOT2
      IF(CLON.GE.GDTLON(II+1,JJ))  GOTO 120
      IF(CLON.LT.GDTLON(II  ,JJ))  GOTO 120
C
C     WE HAVE THE I INDEX (IP) OF THE OUTPUT GRID POINT
C     CLOSEST TO THE K-TH E-GRID MASS POINT.  SAVE THIS
C     VALUE AND EXIT LOOP 120.
C
      IP=II
      IEGRDK(I,J)=IP
      GOTO 125
 120  CONTINUE
C     
C     REACHING THIS LINE MEANS THAT IP HAS NOT BEEN IDENTIFIED
C     ON THIS ROW OF THE OUTPUT GRID SO MOVE TO THE NEXT ROW OF
C     THE OUTPUT GRID AND REPEAT THE PROCESS.
C   
      GOTO 130
C
C     REACHING THIS POINT MEANS THAT WE HAVE THE I INDEX.
C     CHECK TO SEE IF THE LATITUDE OF THE JJ-TH OUTPUT GRID
C     POINT IN THE IP-TH COLUMN IS WITHIN DLAT OF THE LATITUDE
C     OF THE K-TH E-GRID MASS POINT.  IF SO, SAVE THE J INDEX
C     (JP) AND MOVE ON THE THE NEXT E-GRID MASS POINT.
C     OTHERWISE, MOVE ON THE THE NEXT ROW OF THE OUTPUT GRID
C     AND REPEAT THE SEARCH PROCESS.
C
  125 CONTINUE
C
      IF(CLAT.GE.(GDTLAT(IP,JJ+1)+DLAT))  GOTO 130
      IF(CLAT.LT.(GDTLAT(IP,JJ  )-DLAT))  GOTO 130
      JP=JJ
      JEGRDK(I,J)=JP
      GOTO 175
  130 CONTINUE
C     
C     REACHING THIS BLOCK OF CODE MEANS THAT NO NEAREST NEIGHBOR
C     OUTPUT GRID POINT HAS BEEN IDENTIFIED.  THIS COULD HAVE 
C     HAPPENED BECAUSE OF
C     (A) OBLIQUENESS OF TRANSFORMED GRID LINES, OR
C     (B) ETA GRID POINT BEING OUTSIDE THE OUTPUT GRID AREA.
C     WE ALLOW FOR (A) BY INCREASING THE TOLERANCE DLAT (UP TO
C     A LIMIT OF 0.1) AND REPEATING THE SEARCH.
C     
      IF(DLAT.LT.0.1)THEN
        DLAT = DLAT + 0.005
        GOTO 110
      ENDIF
C     
C     REACHING THIS LAST BLOCK OF CODE MEANS THAT WE CAN'T FIND
C     A NEAREST NEIGHBOR OUTPUT GRID POINT FOR THE K-TH E-GRID
C     MASS POINT.  SET I- AND J-EGRDK ARRAY TO -1 FOR THIS K.
C     (GRANTED, THIS IS OVERKILL, SINCE WE INITIALIZED THESE 
C     ARRAYS TO -1.)
C     
      IEGRDK(I,J)=-1
      JEGRDK(I,J)=-1
C     
C     REPEAT SEARCH FOR NEXT E-GRID MASS POINT.
C
 175  CONTINUE
C
C     END OF ROUTINE.
C
      RETURN
      END
