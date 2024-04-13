C     *****************************************************************
C
C     ACEY DUCEY CARD GAME. ORIGINALLY WRITTEN IN BASIC BY BILL PALMBY,
C     AND ADAPTED BY DAVID H. AHL. PORTED TO FORTRAN BY PHILIPP ENGEL.
C
C     *****************************************************************
      PROGRAM ACEY
      EXTERNAL  PLAY
      CHARACTER ANS
      INTEGER   ISTAT
      LOGICAL   DONE
C
C     INITIALISE PRNG.
C
      CALL SRAND(TIME())
C
C     DISPLAY TITLE AND INSTRUCTIONS, THEN START THE GAME.
C
      DONE = .FALSE.
   10 CONTINUE
      PRINT 100
      CALL PLAY()
C
C     PLAY AGAIN?
C
      PRINT 110
      READ (*, 200, IOSTAT=ISTAT) ANS
      IF (ANS .NE. 'Y' .AND. ANS .NE. 'y') DONE = .TRUE.
      IF (.NOT. DONE) GOTO 10
      PRINT 120

  100 FORMAT (11X,' ACEY DUCEY CARD GAME',/,
     &12X,' CREATIVE COMPUTING',/,
     &10X,' MORRISTOWN, NEW JERSEY',/,/,
     &' ACEY-DUCEY IS PLAYED IN THE FOLLOWING MANNER.',/,
     &' THE DEALER (COMPUTER) DEALS TWO CARDS FACE UP.',/,
     &' YOU HAVE AN OPTION TO BET OR NOT BET DEPENDING',/,
     &' ON WHETHER OR NOT YOU FEEL THE CARD WILL HAVE',/,
     &' A VALUE BETWEEN THE FIRST TWO.',/,
     &' IF YOU DO NOT WANT TO BET, BET A 0.')
  110 FORMAT (/,' SORRY, MY FRIEND BUT YOU BLEW YOUR WAD.',/,
     &' TRY AGAIN? (Y/N)')
  120 FORMAT (/,' OK. HOPE YOU HAD FUN.')
  200 FORMAT (A)
      END
C     *****************************************************************
      INTEGER FUNCTION DEAL()
C
C     DEALS CARD. RETURNS RANDOM INTEGER IN RANGE [1, 13].
C
      DEAL = 1 + NINT(RAND() * 12)
      END
C     *****************************************************************
      SUBROUTINE PLAY()
C
C     THE GAME STARTS HERE.
C
      EXTERNAL OUTPUT
      INTEGER  DEAL

      INTEGER IBANKR, IBET, IDEAL1, IDEAL2, ISTAT, IPLAYR, ISWAP
      LOGICAL VALID

      INTEGER     JBANKR
      CHARACTER*5 CARDS(13)
      COMMON /GLOBAL/ JBANKR, CARDS

      IBANKR = JBANKR
      CALL OUTPUT(IBANKR)
C
C     MAIN LOOP, RUNS UNTIL PLAYER IS BROKE.
C
   10 CONTINUE
      PRINT 100
      IDEAL1 = DEAL()
C
C     RE-DEAL 2ND CARD UNTIL IT DOESN'T MATCH THE FIRST.
C
   20 CONTINUE
      IDEAL2 = DEAL()
      IF (IDEAL1 .EQ. IDEAL2) GOTO 20
C
C     RE-ORDER CARDS BY VALUE.
C
      IF (IDEAL1 .GT. IDEAL2) THEN
        ISWAP  = IDEAL1
        IDEAL1 = IDEAL2
        IDEAL2 = ISWAP
      END IF
C
C     SHOW DEALER CARDS TO PLAYER.
C
      PRINT *, CARDS(IDEAL1)
      PRINT *, CARDS(IDEAL2)
C
C     BETTING LOOP, RUNS UNTIL VALID IS TRUE.
C
      VALID = .FALSE.
   30 CONTINUE
C
C     ASK FOR BET.
C
      PRINT 110
      READ (*, 200, IOSTAT=ISTAT) IBET
      IF (ISTAT .NE. 0) THEN
        PRINT 120
        GOTO 30
      END IF
C
C     DEAL CARD AND OUTPUT RESULT.
C
      IF (IBET .LE. 0) THEN
        VALID = .TRUE.
        PRINT 130
      ELSE IF (IBET .GT. IBANKR) THEN
        PRINT 140, IBANKR
      ELSE
        VALID = .TRUE.
        IPLAYR = DEAL()
        PRINT 150, CARDS(IPLAYR)
        IF (IDEAL1 .LT. IPLAYR .AND. IPLAYR .LT. IDEAL2) THEN
          PRINT 160
          IBANKR = IBANKR + IBET
        ELSE
          PRINT 170
          IBANKR = IBANKR - IBET
        END IF
        CALL OUTPUT(IBANKR)
      END IF
      IF (.NOT. VALID) GOTO 30
      IF (IBANKR .GT. 0) GOTO 10
      RETURN

  100 FORMAT (' HERE ARE YOUR NEXT TWO CARDS:')
  110 FORMAT (/,' WHAT IS YOUR BET?')
  120 FORMAT (' INVALID INPUT.')
  130 FORMAT (' CHICKEN!!',/)
  140 FORMAT (' SORRY, MY FRIEND BUT YOU BET TOO MUCH.',/,
     &' YOU HAVE ONLY $',I4,' TO BET.')
  150 FORMAT (' CARD: ',A)
  160 FORMAT (' YOU WIN!!')
  170 FORMAT (' SORRY, YOU LOSE.')
  200 FORMAT (I8)
      END
C     *****************************************************************
      SUBROUTINE OUTPUT(IBANKR)
C
C     OUTPUTS BANKROLL TO SCREEN.
C
      INTEGER IBANKR
      IF (IBANKR .GT. 0) PRINT 100, IBANKR
  100 FORMAT (/,' YOU NOW HAVE $',I4,'.')
      END
C     *****************************************************************
      BLOCK DATA
C
C     COMMON VARIABLES:
C
C     JBANKR - INITIAL BANKROLL ($100).
C     CARDS  - CARD NAMES.
C
      INTEGER     JBANKR
      CHARACTER*5 CARDS(13)
      COMMON /GLOBAL/ JBANKR, CARDS
      DATA JBANKR /100/
      DATA CARDS  /'2','3','4','5','6','7','8','9','10',
     &             'JACK','QUEEN','KING','ACE'/
      END
