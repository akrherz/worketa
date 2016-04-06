C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C
      SUBROUTINE SSTHIRES (SST,SM,GLAT,GLON,IDAT,LIST,DTR)
C
      IMPLICIT REAL (A-H, O-Z)
C
      INCLUDE "parmeta"
C

Chires
	REAL INCR,ILAT1,ILON1,ILAT2,ILON2
	PARAMETER(INCR=0.5)
Chires

      PARAMETER  (H90=90.0,H360=360.0,D5=5.E-1,D00=0.0,H1=1.0)
C
      INTEGER IDATE(4),IDAT(3),MONTH(12)
      DIMENSION SSTLL(721,360),SALTLK(12),SALTLA(2),SALTLO(2)
C
      DIMENSION  SST(IM,JM), SM(IM,JM), GLAT(IM,JM), GLON(IM,JM)
C
      DATA   INSST/39/
      DATA   INDXST/0/

      DATA MONTH/31,28,31,30,31,30,31,31,30,31,30,31/
C
      DATA SALTLK/273.38,274.27,278.50,283.01,287.33,293.41
     1,           297.13,297.73,294.97,289.58,282.31,275.67/
C
C     CORNERS OF SALT LAKE LATITUDE/LONGITUDE BOX
C     in degrees---> 40.0     42.0            111.0    114.0
      DATA SALTLA/0.698132,0.733038/,SALTLO/1.937315,1.989675/
C
      IOUTUPRT = LIST
      CALL GRIBHRST(INSST,INDXST,SSTLL,IERR)
      IF (IERR.NE.0) GOTO 4500
C
C----  INTERPOLATE 1-DEG GLOBAL SST TO ETA GRID  -------
C
C-CP NOTE:  THIS SUBROUTINE AND INTERPOLATION ALGORITHM ASSUME
C-CP A 1-DEG GLOBAL SST FIELD IN THE FOLLOWING FORMAT:  
C-CP
C-CP  I=1 AT 0.5 E,  I=2 AT 1.5 E, ... , I=360 at 0.5W
C-CP  J=1 AT 89.5S, J=2 AT 88.5 S, ..., J=180 at 89.5N
C
C
C	NEW 0.5 degree data
C
C	I=1 at 0.25 E, I=720 at 0.25 W, I=721 at 0.25 E
C	J=1 at 89.75 S, J=360 at 39.75 N 
C
C
C-CP  
C-CP In the interpolation algorithm below, glon is positive westward,
C-CP from 0 to 360, with 0 at the greenwich meridian.  Elon is positive 
C-CP eastward, thus the need to subtract glon from 360 to get the index
C-CP of the correct oisst point.  If your input 1 deg SST field is in
C-CP a different indexing scheme, you will need to change the algorithm
C-CP below - see "grdeta.oldoi"
C-CP
      DO J=1,JM
      DO I=1,IM
      ELAT=H90+GLAT(I,J)/DTR
      ELON=H360-GLON(I,J)/DTR
      IF(ELON.GT.H360)ELON=ELON-H360

	DIF=ELON-INT(ELON)
	IF (DIF .ge. 0.75) ILON1=INT(ELON)+0.75
	IF (DIF .lt. 0.25) ILON1=INT(ELON)-0.25
	IF (DIF .ge. 0.25 .and. DIF .lt. 0.75) ILON1=INT(ELON)+0.25

      IF(ILON1.EQ.D00)ILON1=360.
      ILON2=ILON1+INCR

C
C-MP	New approach sets ILAT1, ILON1 to point on SST grid that is
C-MP	SW of the Eta Grid point.
C
C
        DIF=ELAT-INT(ELAT)
        IF (DIF .ge. 0.75) ILAT1=INT(ELAT)+0.75
        IF (DIF .lt. 0.25) ILAT1=INT(ELAT)-0.25
        IF (DIF .ge. 0.25 .and. DIF .lt. 0.75) ILAT1=INT(ELAT)+0.25


C      DIF=ELAT-ILAT1
Chires      IF(DIF.GT.D5)ILAT1=MIN(ILAT1+1,179)
Ctst      IF(DIF.GT.INCR/2.)ILAT1=AMIN1((ILAT1+INCR),179.5)


C     IF(ILAT1.EQ.180.OR.ILAT1.EQ.0)THEN
C       WRITE(6,6788)I,J,GLAT(I,J),GLON(I,J),ELAT,ELON
C6788   FORMAT(' I,J=',2I4,' GLAT=',E12.5,' GLON=',E12.5,
C    1   ' ELAT=',E12.5,' ELON=',E12.5)
C       STOP 333
C     ENDIF

      ILAT2=ILAT1+INCR

Chires,notsure      W1=ELON-ILON1+D5
      W1=ELON-ILON1+INCR/2.
      IF(W1.LT.D00)W1=W1+H360
Chires,notsure      W2=ELAT-ILAT1+D5
      W2=ELAT-ILAT1+INCR/2.
      AR1=W1*W2
      AR2=W1*(H1-W2)
      AR3=(H1-W1)*(H1-W2)
      AR4=(H1-W1)*W2
C	LON1INDX=2*ILON1+1
C	LON2INDX=2*ILON2+1
C	LAT1INDX=2*ILAT1+1
C	LAT2INDX=2*ILAT2+1
	LON1INDX=2*(ILON1+INCR/2.)
	LON2INDX=2*(ILON2+INCR/2.)
	LAT1INDX=2*(ILAT1+INCR/2.)
	LAT2INDX=2*(ILAT2+INCR/2.)
	if (mod (I,20) .eq. 0 .and. mod(J,20) .eq. 0) then
	write(6,*) 'weights: ',AR1,AR2,AR3,AR4
	write(6,*) 'ILAT1,ILON1,ELAT,ELON: ', ILAT1,ILON1,ELAT,ELON
	write(6,*) '------------------------------------------'
C	write(6,*) 'corresponding indices: ', LAT1INDX,LON1INDX,
C     +					      LAT2INDX,LON2INDX
	endif
C      SST(I,J) = AR1*SSTLL(ILON2,ILAT2)+AR2*SSTLL(ILON2,ILAT1)+
C     1            AR3*SSTLL(ILON1,ILAT1)+AR4*SSTLL(ILON1,ILAT2)
	SST(I,J)=AR1*SSTLL(LON2INDX,LAT2INDX)+
     +	 	 AR2*SSTLL(LON2INDX,LAT1INDX)+
     +	 	 AR3*SSTLL(LON1INDX,LAT1INDX)+
     +	 	 AR4*SSTLL(LON1INDX,LAT2INDX)
      ENDDO
      ENDDO
C***
C***  INSERT TEMPERATURES FOR THE GREAT SALT LAKE
C***
      ID1=IDAT(1)
      ID2=IDAT(2)
      MARG0=ID1-1
      IF(MARG0.LT.1)MARG0=12
      MNTH0=MONTH(MARG0)
      MNTH1=MONTH(ID1)
      IF(ID2.LT.15)THEN
        NUMER=ID2+MNTH0-15
        DENOM=MNTH0
        IARG1=MARG0
        IARG2=ID1
      ELSE
        NUMER=ID2-15
        DENOM=MNTH1
        IARG1=ID1
        IARG2=ID1+1
        IF(IARG2.GT.12)IARG2=1
      ENDIF
      FRAC=NUMER/DENOM
      DO J=1,JM
      DO I=1,IM
        IF(GLAT(I,J).GT.SALTLA(1).AND.GLAT(I,J).LT.SALTLA(2))THEN
          IF(GLON(I,J).GT.SALTLO(1).AND.GLON(I,J).LT.SALTLO(2))THEN
            IF(SM(I,J).GT.0.5)
     1        SST(I,J)=SALTLK(IARG1)+
     2                (SALTLK(IARG2)-SALTLK(IARG1))*FRAC
          ENDIF
        ENDIF
      ENDDO
      ENDDO
C
      RETURN
C
 4500 CONTINUE
C              ERROR OCCURRED WHEN INPUTING SST FROM GRIB.
      WRITE (IOUTUPRT, 4550) INSST
 4550 FORMAT ('0', 'ERROR OCCURRED WHEN READING IN SST        ',
     1             'ON UNIT', I3, ' GRIB ' /
     2        ' ', 'EXECUTION TERMINATING.')
C
      STOP 222
C
      END
