C     ******************************************************************
C
C     JACKPOT - SLOT MACHINE GAME
C
C     PROGRAM WAS ORIGINALLY WRITTEN IN BASIC, AND PUBLISHED IN:
C
C       J. VICTOR NAHIGIAN & WILLIAM S. HODGES: COMPUTER GAMES FOR
C       BUSINESS, SCHOOL, AND HOME. FOR TRS-80 LEVEL II BASIC. WINTHROP,
C       CAMBRIDGE, MA, 1980
C
C     PORTED TO FORTRAN 77 BY PHILIPP ENGEL (2021).
C
C     ******************************************************************
      PROGRAM JACKPOT
      EXTERNAL  PAYOFF
      INTEGER   IFIRST
      PARAMETER (IFIRST=15)
      CHARACTER A
      INTEGER   IBANK, ISTAT
C
C     SET THE BANK ROLL AND INITIALISE THE RANDOM NUMBER GENERATOR.
C
      IBANK = IFIRST
      CALL SRAND(TIME())
C
C     OUTPUT TITLE AND SHOW THE LIST OF PAYOFFS.
C
      PRINT 100
      CALL PAYOFF()
C
C     GAME LOOP.
C
   10 CONTINUE
      PRINT 200, IBANK
      READ (*, 900, IOSTAT=ISTAT) A
C
C     PULL THE HANDLE.
C
      CALL PULL(IBANK)
C
C     ASK PLAYER TO PLAY AGAIN.
C
      PRINT 300
      A = ' '
      READ (*, 900, IOSTAT=ISTAT) A
      IF (A .EQ. ' ' .OR. A .EQ. 'Y' .OR. A .EQ. 'y') GOTO 10
C
C     SHOW RESULTS.
C
      IF (IBANK .LT. 0) THEN
        PRINT 400, ABS(IBANK)
      ELSE IF (IBANK .GT. IFIRST) THEN
        PRINT 500, IBANK - IFIRST
      ELSE
        PRINT 600, IFIRST - IBANK
      END IF

  100 FORMAT (16X,' JACKPOT GAME',/)
  200 FORMAT (/,' YOU HAVE $',I3,'.',/,/,' HIT RETURN TO PULL HANDLE.')
  300 FORMAT (/,' AGAIN? ',$)
  400 FORMAT (/,' YOU HAVE 10 DAYS TO PAY ME $',I3,'. AFTER THAT',/,
     &' IT IS OUT OF MY HANDS AS TO WHAT HAPPENS TO YOU!')
  500 FORMAT (/,' CONGRATULATIONS, YOU WON $',I3,'.')
  600 FORMAT (/,' TOO BAD, YOU LOST $',I3,'.')
  900 FORMAT (A)
      END
C     ******************************************************************
      SUBROUTINE PAYOFF()
C
C     SHOWS THE LIST OF PAYOFFS.
C
      CHARACTER A
      INTEGER   ISTAT

      PRINT 100
      READ (*, 900, IOSTAT=ISTAT) A
      IF (ISTAT .NE. 0) RETURN
      IF (A .NE. 'Y' .AND. A .NE. 'y') RETURN
      PRINT 200

  100 FORMAT (' DO YOU WANT A LIST OF THE PAYOFFS? ',$)
  200 FORMAT (/,' REEL 1      REEL 2      REEL 3      PAYOFF',/,/,
     &' CHERRY      ANYTHING    ANYTHING      3 DOLLARS',/,
     &' CHERRY      CHERRY      ANYTHING      5 DOLLARS',/,
     &' ORANGE      ORANGE      BAR           6 DOLLARS',/,
     &' BELL        BELL        ORANGE        8 DOLLARS',/,
     &' PLUM        PLUM        PLUM         10 DOLLARS',/,
     &' APPLE       APPLE       ANYTHING     15 DOLLARS',/,
     &' ORANGE      ORANGE      ORANGE       18 DOLLARS',/,
     &' APPLE       APPLE       APPLE        20 DOLLARS',/,
     &' BELL        BELL        BELL         30 DOLLARS',/,
     &' BAR         BAR         BAR         200 DOLLARS')
  900 FORMAT (A)
      END
C     ******************************************************************
      SUBROUTINE PULL(IBANK)
C
C     PULLS THE HANDLE OF THE SLOT MACHINE.
C
      INTEGER     IBANK
      CHARACTER*6 REELY(6)
      INTEGER     NEXTY(3, 20)
      INTEGER     I, IR(3), IWIN, IX

      DATA REELY /'CHERRY','ORANGE','BAR','BELL','PLUM','APPLE'/
      DATA NEXTY /2,4,2,5,1,4,2,2,5,6,4,6,1,1,2,4,2,6,5,4,
     &5,6,1,4,4,5,3,5,5,5,1,1,2,2,2,4,4,4,6,1,
     &3,2,3,4,2,2,1,4,6,2,5,1,4,2,4,6,4,2,1,2/
C
C     INSERT COIN.
C
      IBANK = IBANK - 1
C
C     SPIN THE REELS.
C
      DO 10 I = 1, 3
      IX = 1 + INT(RAND(0) * 20)
      IR(I) = NEXTY(I, IX)
   10 CONTINUE
C
C     DETERMINE THE WIN.
C
      IWIN = 0
      IF (IR(1) .EQ. 3 .AND. IR(2) .EQ. 3 .AND. IR(3) .EQ. 3) THEN
        IWIN = 200
      ELSE IF (IR(1) .EQ. 4 .AND. IR(2) .EQ. 4 .AND. IR(3) .EQ. 4) THEN
        IWIN = 30
      ELSE IF (IR(1) .EQ. 6 .AND. IR(2) .EQ. 6 .AND. IR(3) .EQ. 6) THEN
        IWIN = 20
      ELSE IF (IR(1) .EQ. 2 .AND. IR(2) .EQ. 2 .AND. IR(3) .EQ. 2) THEN
        IWIN = 20
      ELSE IF (IR(1) .EQ. 6 .AND. IR(2) .EQ. 6) THEN
        IWIN = 15
      ELSE IF (IR(1) .EQ. 5 .AND. IR(2) .EQ. 5 .AND. IR(3) .EQ. 5) THEN
        IWIN = 10
      ELSE IF (IR(1) .EQ. 4 .AND. IR(2) .EQ. 4 .AND. IR(3) .EQ. 2) THEN
        IWIN = 8
      ELSE IF (IR(1) .EQ. 2 .AND. IR(2) .EQ. 2 .AND. IR(3) .EQ. 3) THEN
        IWIN = 6
      ELSE IF (IR(1) .EQ. 1 .AND. IR(2) .EQ. 1) THEN
        IWIN = 5
      ELSE IF (IR(1) .EQ. 1) THEN
        IWIN = 3
      END IF
C
C     SHOW RESULTS AND TRANSFER WIN.
C
      PRINT 100, REELY(IR(1)), REELY(IR(2)), REELY(IR(3)), IWIN
      IBANK = IBANK + IWIN
      IF (IWIN .EQ. 200) PRINT 200

  100 FORMAT (1X,3(A,6X),'YOU WIN $',I3)
  200 FORMAT (' *** JACKPOT ***')
      END
