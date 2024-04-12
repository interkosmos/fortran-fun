C     ******************************************************************
C
C     RUSSIAN ROULETTE
C
C     ORIGINAL BASIC VERSION WRITTEN BY TOM ADAMETX, MODIFIED AND
C     PUBLISHED BY DAVID H. AHL. CONVERTED TO FORTRAN BY PHILIPP ENGEL.
C
C     ******************************************************************
      PROGRAM RUSSIA
      INTEGER N, INPUT, ISTAT

      CALL SRAND(TIME())
      PRINT 100
C
C     THE MAIN LOOP.
C
   10 CONTINUE
      N = 0
      PRINT 200
C
C     USER INPUT LOOP.
C
   20 CONTINUE
      READ (*, 300, IOSTAT=ISTAT) INPUT
      IF (ISTAT .NE. 0 .OR. (INPUT .NE. 1 .AND. INPUT .NE. 2)) THEN
        PRINT 400
        GOTO 20
      END IF

      IF (INPUT .EQ. 1) THEN
        N = N + 1
C
C       UHH-OHH ...
C
        IF (RAND(0) .LE. 1.0 / 6) THEN
          PRINT 500
          PRINT 900
          GOTO 10
        END IF
C
C       PLAYER HAS WON.
C
        IF (N .GT. 10) THEN
          PRINT 600
          GOTO 10
        END IF
C
C       PLAYER GOT LUCKY.
C
        PRINT 700
        GOTO 20
      ELSE IF (INPUT .EQ. 2) THEN
        PRINT 800
        PRINT 900
        GOTO 10
      END IF

  100 FORMAT (27X,' RUSSIAN ROULETTE',/,
     &26X,' CREATIVE COMPUTING',/,24X,' MORRISTOWN, NEW JERSEY',/,/,
     &' THIS IS A GAME OF RUSSIAN ROULETTE. HERE IS A REVOLVER.')
  200 FORMAT (/,' TYPE "1" TO SPIN THE CHAMBER AND PULL THE TRIGGER.',/,
     &' TYPE "2" TO GIVE UP.',/,' GO!')
  300 FORMAT (I1)
  400 FORMAT (' INVALID INPUT. TRY AGAIN:')
  500 FORMAT (' BANG!! YOU ARE DEAD!',/,
     &' CONDOLENCES WILL BE SENT TO YOUR RELATIVES.')
  600 FORMAT (' YOU WIN!',/,' LET SOMEONE ELSE BLOW HIS BRAINS OUT.')
  700 FORMAT (' - CLICK -')
  800 FORMAT (' CHICKEN!!')
  900 FORMAT (/,' ... NEXT VICTIM ...')
      END
