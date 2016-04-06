       SUBROUTINE SWAP32(A,N)
C
C      REVERSE ORDER OF BYTES IN INTEGER*4 WORD, or REAL*4
C
       INTEGER*4   A(N)
C
       CHARACTER*1 JTEMP(4)
       CHARACTER*1 KTEMP
C
       SAVE
C
       EQUIVALENCE (JTEMP(1),ITEMP)
C
       DO 10 I = 1,N
         ITEMP    = A(I)
         KTEMP    = JTEMP(4)
         JTEMP(4) = JTEMP(1)
         JTEMP(1) = KTEMP
         KTEMP    = JTEMP(3)
         JTEMP(3) = JTEMP(2)
         JTEMP(2) = KTEMP
         A(I)     = ITEMP
 10    CONTINUE
       RETURN
       END
