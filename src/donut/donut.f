C     ******************************************************************
C
C     SPINNING DONUT.
C
C     ******************************************************************
      PROGRAM DONUT
      INTEGER IW, IH
      REAL    SCALE, SPEED, TAU
      PARAMETER (SCALE=1.0, SPEED=0.5, TAU=6.28318548)
      PARAMETER (IH=INT(80*SCALE), IW=INT(22*SCALE))
      CHARACTER*12 ATABLE
      CHARACTER    SCREEN(IW*IH)
      INTEGER      I, N, O, X, Y
      REAL         Z(IW*IH)
      REAL         A, B, C, D, MESS, T
      REAL         SINA, SINB, SINC, SIND
      REAL         COSA, COSB, COSC, COSD, COSD2

      DATA ATABLE /'.,-~:;=!*#$@'/
      A = 0.0
      B = 0.0
      PRINT 100, CHAR(27)

   10 CONTINUE
      DO 20 I = 1, IW * IH
      SCREEN(I) = ' '
      Z(I) = 0.0
   20 CONTINUE

      D = 0.0

   30 CONTINUE
      C = 0.0

   40 CONTINUE
      SINA = SIN(A)
      SINB = SIN(B)
      SINC = SIN(C)
      SIND = SIN(D)
      COSA = COS(A)
      COSB = COS(B)
      COSC = COS(C)
      COSD = COS(D)
      COSD2 = COSD + 2

      MESS = 1.0 / (SINC * COSD2 * SINA + SIND * COSA + 5)

      T = SINC * COSD2 * COSA - SIND * SINA
      X = INT(40 * SCALE + SCALE * 30 * MESS *
     &     (COSC * COSD2 * COSB - T * SINB))
      Y = INT(12 * SCALE + SCALE * 15 * MESS *
     &     (COSC * COSD2 * SINB + T * COSB))
      O = MIN(IW * IH, X + (IH * Y))
      N = INT(8 * ((SIND * SINA - SINC * COSD * COSA) * COSB -
     &  SINC * COSD * SINA - SIND * COSA - COSC * COSD * SINB))

      IF (Y .GT. 0 .AND. Y .LT. IW .AND. X .GT. 0 .AND. X .LT. IH .AND.
     &    MESS .GT. Z(O)) THEN
        Z(O) = MESS
        IF (N .GT. 0) THEN
          SCREEN(O) = ATABLE(N:N)
        ELSE
          SCREEN(O) = '.'
        END IF
      END IF

      C = C + 0.02
      IF (C .LT. TAU) GOTO 40

      D = D + 0.07
      IF (D .LT. TAU) GOTO 30

      PRINT 200, CHAR(27)

      DO 50 I = 0, (IW * IH) + 1
      IF (MOD(I, IH) == 0) THEN
        PRINT 300, CHAR(10)
      ELSE
        PRINT 300, SCREEN(I)
      END IF
   50 CONTINUE

      A = A + 0.04 * SPEED
      B = B + 0.04 * SPEED
      GOTO 10

  100 FORMAT (A1,'[2J',$)
  200 FORMAT (A1,'[d',$)
  300 FORMAT (A1,$)
      END
