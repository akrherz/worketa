      MODULE EXCHM
      CONTAINS
      SUBROUTINE EXCH0(ARR1,LL1,IHALO,JHALO)
C*************************************************************************
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    EXCHxxx     FUNDAMENTAL EXCHANGE ROUTINES
C   PRGRMMR: BLACK           ORG: W/NP2      DATE: 97-06-25
C
C ABSTRACT:
C     SUBROUTINE EXCH0 IS USED TO EXCHANGE HALOS BETWEEN PROCESSORS
C
C     CURRENTLY SUPPORTED INTERFACES
C
C     0,00,01,011,0001111
C     1,11,111,1111,11111,111111
C
C     WHERE 0 REFERS TO A 2-D ARRAY AND 1 REFERS TO A 3-D ARRAY
C
C PROGRAM HISTORY LOG:
C   97-05-??  MEYS       - ORIGINATOR
C   97-06-25  BLACK      - CONVERTED FROM SHMEM TO MPI
C   98-??-??  TUCCILLO   - REMOVED EXPLICIT EXCHANGES OF CORNERS
C   98-??-??  TUCCILLO   - REWROTE TO USE NON_BLOCKING MPI ROUTINES
C   99-??-??  BLACK      - ADDED VARIABLE HALO SIZES
C   00-03-10  TUCCILLO   - CHANGED TO USE MODULE PROCDURES FOR
C                          INCREASED MESSAGE SIZES AND A UNIFORM
C                          INTERFACE FOR ALL CALLS
C
C USAGE: CALL EXCH FROM SUBROUTINE GOSSIP
C   INPUT ARGUMENT LIST:
C       ARR - THE ARRAY TO BE EXCHANGED
C       LL  - THE VERTICAL DIMENSION OF ARR
C     IHALO - THE NUMBER OF POINTS IN THE X DIRECTION TO EXCHANGE
C             IN THE HALO
C     JHALO - THE NUMBER OF POINTS IN THE Y DIRECTION TO EXCHANGE
C             IN THE HALO
C
C   OUTPUT ARGUMENT LIST:
C     NONE
C
C   OUTPUT FILES:
C     NONE
C
C   SUBPROGRAMS CALLED:
C
C     UNIQUE: NONE
C
C     LIBRARY: NONE
C
C   COMMON BLOCKS: NONE
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C$$$
C***********************************************************************
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = 1
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH1(ARR1,LL1,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = LL1
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH01(ARR1,LL1,ARR2,LL2,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = 1 + LL2
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+1)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+1)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1,K)=BUF1(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1,K)=BUF0(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF0(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF1(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH00(ARR1,LL1,ARR2,LL2,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = 2
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,MYJE-J)
          BUF2(I,J,2)=ARR2(I,MYJE-J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,MYJS+J)
          BUF3(I,J,2)=ARR2(I,MYJS+J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1)=BUF1(I,J,1)
          ARR2(I,MYJS-J-1)=BUF1(I,J,2)
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1)=BUF0(I,J,1)
          ARR2(I,MYJE+J+1)=BUF0(I,J,2)
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,J)
          BUF2(I,J,2)=ARR2(I,J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,J)
          BUF3(I,J,2)=ARR2(I,J)
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF0(I,J,1)
          ARR2(I,J)=BUF0(I,J,2)
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF1(I,J,1)
          ARR2(I,J)=BUF1(I,J,2)
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH11(ARR1,LL1,ARR2,LL2,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = LL1 + LL2
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH111(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = LL1 + LL2 + LL3
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH1111(ARR1,LL1,ARR2,LL2,ARR3,LL3,
     *                     ARR4,LL4,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = LL1 + LL2 + LL3 + LL4
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH11111(ARR1,LL1,ARR2,LL2,ARR3,LL3,
     *                      ARR4,LL4,ARR5,LL5,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR5(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = LL1 + LL2 + LL3 + LL4 + LL5
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR5(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR5(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR5(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR5(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH111111(ARR1,LL1,ARR2,LL2,ARR3,LL3,ARR4,LL4
     1,                     ARR5,LL5,ARR6,LL6,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR5(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR6(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = LL1 + LL2 + LL3 + LL4 + LL5 + LL6
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR5(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR6(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR5(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR6(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K)=ARR1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF0(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF0(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF0(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR5(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR6(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO K=1,LL1
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J,K)=BUF1(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF1(I,J,K+LL1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF1(I,J,K+LL1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR5(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR6(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH011(ARR1,LL1,ARR2,LL2,ARR3,LL3,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = 1 + LL2 + LL3
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+1)=ARR2(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+1+LL2)=ARR3(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+1)=ARR2(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+1+LL2)=ARR3(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1,K)=BUF1(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJS-J-1,K)=BUF1(I,J,K+1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1,K)=BUF0(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJE+J+1,K)=BUF0(I,J,K+1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+1)=ARR2(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+1+LL2)=ARR3(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF0(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF0(I,J,K+1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DO K=1,LL2
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J,K)=BUF1(I,J,K+1)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL3
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J,K)=BUF1(I,J,K+1+LL2)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE IEXCH(ARR1,LL1,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      INTEGER ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      INTEGER,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_INTEGER
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = 1
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
c     call mpi_barrier(MPI_COMM_COMP,ierr)
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
c     call mpi_barrier(MPI_COMM_COMP,ierr)
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      SUBROUTINE EXCH0001111(ARR1,LL1,ARR2,LL2,ARR3,LL3,
     *                        ARR4,LL4,ARR5,LL5,
     *                        ARR6,LL6,ARR7,LL7,IHALO,JHALO)
      INCLUDE "parmeta"
      INCLUDE "mpif.h"
      INCLUDE "mpp.h"
C-----------------------------------------------------------------------
      INTEGER ISTAT(MPI_STATUS_SIZE),STATUS_ARRAY(MPI_STATUS_SIZE,4)
      INTEGER IHANDLE(4)
      REAL ARR1(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR2(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR3(IDIM1:IDIM2,JDIM1:JDIM2)
      REAL ARR4(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR5(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR6(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL ARR7(IDIM1:IDIM2,JDIM1:JDIM2,*)
      REAL,ALLOCATABLE,DIMENSION(:,:,:) :: BUF0,BUF1,BUF2,BUF3
C***********************************************************************
C
      ITYPE=MPI_REAL
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  NORTH/SOUTH
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM NORTH
C--------------------------------------------------------------------
C
      LL = LL1 + LL2 + LL3 + LL4 + LL5 + LL6 + LL7
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF0(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*JHALO*LL
        ALLOCATE(BUF1(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        NEBPE=MY_NEB(1)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF2(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,MYJE-J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,2)=ARR2(I,MYJE-J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,3)=ARR3(I,MYJE-J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)=ARR7(I,MYJE-J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        NEBPE=MY_NEB(3)
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        ALLOCATE(BUF3(IBEG:IEND,0:JHALO-1,LL),STAT=I)
        KNT=(IEND-IBEG+1)*JHALO*LL
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,MYJS+J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,2)=ARR2(I,MYJS+J)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,3)=ARR3(I,MYJS+J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)=ARR7(I,MYJS+J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE RESULTS FROM SOUTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(3).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJS-J-1)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJS-J-1)=BUF1(I,J,2)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJS-J-1)=BUF1(I,J,3)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR5(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR6(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR7(I,MYJS-J-1,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM NORTH
C--------------------------------------------------------------------
C
      IF(MY_NEB(1).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR1(I,MYJE+J+1)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR2(I,MYJE+J+1)=BUF0(I,J,2)
        ENDDO
        ENDDO
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR3(I,MYJE+J+1)=BUF0(I,J,3)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR4(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR5(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR6(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=0,JHALO-1
        DO I=IBEG,IEND
          ARR7(I,MYJE+J+1,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
      IF(MY_NEB(1).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
      IF(MY_NEB(3).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C***
C***  EAST/WEST
C***
C--------------------------------------------------------------------
C--------------------------------------------------------------------
C
C--------------------------------------------------------------------
C     RECEIVE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS-IHALO
        IEND=IBEG+IHALO-1
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF0(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF0,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(1),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     RECEIVE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        ISIZE=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF1(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
        CALL MPI_IRECV(BUF1,ISIZE,ITYPE,NEBPE,NEBPE
     1,               MPI_COMM_COMP,IHANDLE(2),IRECV)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        NEBPE=MY_NEB(2)
        IBEG=MYIE-IHALO+1
        IEND=MYIE
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF2(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,2)=ARR2(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,3)=ARR3(I,J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF2(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        CALL MPI_ISEND(BUF2,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(3),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     SEND TO WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        NEBPE=MY_NEB(4)
        IBEG=MYIS
        IEND=MYIS+IHALO-1
        KNT=(IEND-IBEG+1)*(MYJE+JHALO-MYJS+JHALO+1)*LL
        ALLOCATE(BUF3(IBEG:IEND,MYJS-JHALO:MYJE+JHALO,LL),STAT=I)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,1)=ARR1(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,2)=ARR2(I,J)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,3)=ARR3(I,J)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3)=ARR4(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4)=ARR5(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4+LL5)=ARR6(I,J,K)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          BUF3(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)=ARR7(I,J,K)
        ENDDO
        ENDDO
        ENDDO
       CALL MPI_ISEND(BUF3,KNT,ITYPE,NEBPE,MYPE
     1,               MPI_COMM_COMP,IHANDLE(4),ISEND)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM WEST
C--------------------------------------------------------------------
C
      IF(MY_NEB(4).GE.0)THEN
        IBEG=MYIS-IHALO
        IEND=MYIS-1
        CALL MPI_WAIT(IHANDLE(1),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF0(I,J,1)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J)=BUF0(I,J,2)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J)=BUF0(I,J,3)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR5(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR6(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR7(I,J,K)=BUF0(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF0,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
C     STORE FROM EAST
C--------------------------------------------------------------------
C
      IF(MY_NEB(2).GE.0)THEN
        IBEG=MYIE+1
        IEND=MYIE+IHALO
        CALL MPI_WAIT(IHANDLE(2),ISTAT,IERR)
!$omp parallel do
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR1(I,J)=BUF1(I,J,1)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR2(I,J)=BUF1(I,J,2)
        ENDDO
        ENDDO
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR3(I,J)=BUF1(I,J,3)
        ENDDO
        ENDDO
        DO K=1,LL4
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR4(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL5
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR5(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL6
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR6(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4+LL5)
        ENDDO
        ENDDO
        ENDDO
        DO K=1,LL7
        DO J=MYJS-JHALO,MYJE+JHALO
        DO I=IBEG,IEND
          ARR7(I,J,K)=BUF1(I,J,K+LL1+LL2+LL3+LL4+LL5+LL6)
        ENDDO
        ENDDO
        ENDDO
        DEALLOCATE(BUF1,STAT=IER)
      ENDIF
C
      IF(MY_NEB(4).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(4),ISTAT,IERR)
        DEALLOCATE(BUF3,STAT=IER)
      ENDIF
C
      IF(MY_NEB(2).GE.0)THEN
        CALL MPI_WAIT(IHANDLE(3),ISTAT,IERR)
        DEALLOCATE(BUF2,STAT=IER)
      ENDIF
C
C--------------------------------------------------------------------
      END SUBROUTINE
      END MODULE
