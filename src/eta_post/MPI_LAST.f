      SUBROUTINE MPI_LAST
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C SUBPROGRAM:    MPI_LAST    SHUTS DOWN MPI
C   PRGRMMR: TUCCILLO        ORG: IBM
C
C ABSTRACT:
C     SHUTS DOWN MPI
C   .
C
C PROGRAM HISTORY LOG:
C   00-01-06  TUCCILLO - ORIGINAL
C
C USAGE:    CALL COLLECT(A)
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   OUTPUT FILES:
C     STDOUT  - RUN TIME STANDARD OUT.
C
C   SUBPROGRAMS CALLED:
C       MPI_FINALIZE
C     UTILITIES:
C       NONE
C     LIBRARY:
C       NONE
C
C   ATTRIBUTES:
C     LANGUAGE: FORTRAN
C     MACHINE : IBM RS/6000 SP
C$$$

      call MPI_FINALIZE(ierr)
      end
