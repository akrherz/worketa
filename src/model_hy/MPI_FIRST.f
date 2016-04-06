      SUBROUTINE MPI_FIRST
C$$$  SUBPROGRAM DOCUMENTATION BLOCK
C                .      .    .
C   SUBROUTINE:  MPI_FIRST   INTIALIZES MPI STUFF
C   PRGRMMR: TUCCILLO        ORG:  IBM       DATE: 00-01-20
C
C ABSTRACT:   INTIALIZES MPI STUFF
C
C PROGRAM HISTORY LOG:
C   00-01-20  TUCCILLO - ORIGINATOR
C
C USAGE:  CALL MPI_FIRST
C
C   INPUT ARGUMENT LIST:
C
C   OUTPUT ARGUMENT LIST:
C
C   INPUT FILES:  NONE
C
C   OUTPUT FILES:  NONE
C
C   SUBPROGRAMS CALLED:
C     UNIQUE:
C            MPI_INIT
C            MPI_COMM_SIZE
C            MPI_COMM_RANK
C            PARA_RANGE
C
C   EXIT STATES:
C     COND =   0 - NORMAL EXIT
C
C ATTRIBUTES:
C   LANGUAGE: FORTRAN 90
C   MACHINE : IBM SP
C
C$$$
c
      include "parmeta"
      include "PARA.comm"
      include 'mpif.h'
      include "mpp.h"
c
C
      integer ierr
       LNIP = IM/INPES
       LNJP=JM/JNPES
c
      call mpi_comm_size(mpi_comm_comp,num_procs, ierr )
      call mpi_comm_rank(mpi_comm_comp,me, ierr )
c
c     if ( me .eq. 0 ) then
         print *, ' num_procs = ',num_procs
c     end if

      if ( num_procs .gt. npes ) then
         print *, ' too many MPI tasks, max is ',npes, ', stopping'
         stop
      end if
c
      if ( num_procs .gt. 1024 ) then
         print *, ' too many MPI tasks, max is 1024, stopping'
         stop
      end if
c
      do i = 0, num_procs - 1
         call para_range(0,npes-1,num_procs,i,jsta(i),jend(i))
         if ( me .eq. 0 ) then
c           print *, ' task id, jsta, end = ',i,jsta(i),jend(i)
         end if
      end do
c
c     locations
C
C***  PARAMETER LNIP (LNJP) IS SMALLEST THAT THE I (J) EXTENT
C***  OF EACH SUBDOMAIN CAN BE.
C***  IRMND (JRMND) IS THE NUMBER OF "REMAINDER" I (J) POINTS
C***  THAT WILL BE GIVEN TO THE LEFTMOST (LOWERMOST) PEs.

      IRMND=MOD(IM,INPES)
      JRMND=MOD(JM,JNPES)

      DO IPE = 0, NPES - 1
C
      IPOSN=MOD(IPE,INPES)+1
      JPOSN=IPE/INPES+1
C
C***  GLOBAL LIMITS OF THIS PEs SUBDOMAIN
C
      MY_IS_GLB_A(IPE)=(IPOSN-1)*LNIP+MIN(IRMND,IPOSN-1)+1
      MY_IE_GLB_A(IPE)=MY_IS_GLB_A(IPE)+LNIP-1
      IF(IPOSN.LE.IRMND)MY_IE_GLB_A(IPE)=MY_IE_GLB_A(IPE)+1
C
      MY_JS_GLB_A(IPE)=(JPOSN-1)*LNJP+MIN(JRMND,JPOSN-1)+1
      MY_JE_GLB_A(IPE)=MY_JS_GLB_A(IPE)+LNJP-1
      IF(JPOSN.LE.JRMND)MY_JE_GLB_A(IPE)=MY_JE_GLB_A(IPE)+1
c
      if ( me .eq. 0 ) then
c        print *, ' ipe,  MY_IS_GLB,MY_IE_GLB,MY_JS_GLB, MY_JE_GLB =',
c    * ipe, MY_IS_GLB_A(IPE),MY_IE_GLB_A(IPE),
c    * MY_JS_GLB_A(IPE),MY_JE_GLB_A(IPE)
      end if
      END DO
c
      MY_ISD = IM
      MY_IED = 1
      MY_JSD = JM
      MY_JED = 1

      DO I = JSTA(ME), JEND(ME)
         MY_ISD = MIN(MY_IS_GLB_A(I),MY_ISD)
         MY_IED = MAX(MY_IE_GLB_A(I),MY_IED)
         MY_JSD = MIN(MY_JS_GLB_A(I),MY_JSD)
         MY_JED = MAX(MY_JE_GLB_A(I),MY_JED)
      END DO
C
c     print *, ' ME, MY_ISD,MY_IED,MY_JSD,MY_JED = ',
c    & me, MY_ISD,MY_IED,MY_JSD,MY_JED

      end
