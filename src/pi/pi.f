C     ******************************************************************
C
C     SPIGOT ALGORITHM FOR THE DIGITS OF PI.
C
C     ******************************************************************
      PROGRAM PI
      INTEGER NDIGIT, M
      PARAMETER (NDIGIT=2**14 - 1, M=(NDIGIT/4 + 1) * 14)
      INTEGER      I, J
      INTEGER      A, D, E, L
      INTEGER      B(M + 1)
      CHARACTER*14 FMT

      DATA B /M * 20,0/

      E = 0
      L = 2

      DO 20 I = M, 14, -7
      D = 0
      A = I * 2 - 1

      DO 30 J = I, 1, -1
      D = D * J + B(J) * 100
      B(J) = MOD(D, A)
      D = D / A
      A = A - 2
   30 CONTINUE

      IF (D .EQ. 99) THEN
        E = E * 100 + D
        L = L + 2
      ELSE IF (I .EQ. M) THEN
        PRINT 100, (D / 100) / 10.0
        E = MOD(D, 100)
      ELSE
        WRITE (FMT, 200) L, L
        PRINT FMT, E + D / 100
        E = MOD(D, 100)
        L = 2
      END IF
   20 CONTINUE

  100 FORMAT (F3.1,$)
  200 FORMAT ('(I',I4.4,'.',I4.4,',$)')
      END
