C     ******************************************************************
C
C     PENNEY'S GAME
C
C     TWO PLAYERS (USER AND COMPUTER) BET ON BEING THE FIRST TO SEE A
C     PARTICULAR SEQUENCE OF HEADS OR TAILS IN CONSECUTIVE TOSSES OF A
C     FAIR COIN.
C
C     ******************************************************************
      PROGRAM PENNEY
      EXTERNAL  PLAY
      CHARACTER A

      CALL SRAND(TIME())
      PRINT 100

   10 CONTINUE
      CALL PLAY()
      PRINT 200
      READ (*, 300, IOSTAT=ISTAT) A
      IF (ISTAT .EQ. 0 .AND. (A .EQ. 'Y' .OR. A .EQ. 'y')) GOTO 10

  100 FORMAT (' PENNEY''S GAME')
  200 FORMAT (/,' ANOTHER GAME? ',$)
  300 FORMAT (A)
      END
C     ******************************************************************
      INTEGER FUNCTION CMPSEQ(IUSR)
C
C     IF USER'S SEQUENCE IS GIVEN AND NOT -1, THE COMPUTER SELECTS THE
C     OPTIMUM SEQUENCE (WHICH IS ~2-1-2).
C
      INTEGER IUSR

      IF (IUSR .EQ. -1) THEN
        CMPSEQ = NINT(RAND(0) * 7)
      ELSE
        CMPSEQ = IOR(ISHFT(IUSR, -1),
     &               IAND(ISHFT(NOT(IUSR), 1), ISHFT(1, 2)))
      END IF
      END
C     ******************************************************************
      INTEGER FUNCTION USRSEQ(ICMP)
C
C     READS USER'S SEQUENCE FROM INPUT.
C
      INTEGER     ICMP
      CHARACTER*3 A
      INTEGER     I, ISTAT

   10 CONTINUE
      PRINT 100
      READ (*, 200, IOSTAT=ISTAT) A

      IF (ISTAT .NE. 0) THEN
        PRINT 300
        GOTO 10
      END IF

      USRSEQ = 0

      DO 20 I = 1, 3
      IF (A(I:I) .NE. 'H' .AND. A(I:I) .NE. 'h' .AND.
     &    A(I:I) .NE. 'T' .AND. A(I:I) .NE. 't') THEN
        PRINT 300
        GOTO 10
      END IF

      IF (A(I:I) .EQ. 'H' .OR. A(I:I) .EQ. 'h') THEN
        USRSEQ = IOR(USRSEQ, ISHFT(8, -I))
      END IF
   20 CONTINUE

      IF (USRSEQ .EQ. ICMP) THEN
        PRINT 400
        GOTO 10
      END IF

  100 FORMAT (/,' ENTER YOUR SEQUENCE OF THREE (H/T): ',$)
  200 FORMAT (A)
  300 FORMAT (' INVALID INPUT.',
     &' PLEASE ENTER ONLY CHARACTERS "H" AND "T".')
  400 FORMAT (' INVALID INPUT.',
     &' PICK A DIFFERENT SEQUENCE THAN THE COMPUTER.')
      END
C     ******************************************************************
      INTEGER FUNCTION TOSS(ICMP, IUSR)
C
C     FLIPS A COIN UNTIL EITHER THE COMPUTER'S OR THE USER'S PICKED
C     SEQUENCE MATCHES. RETURNS 1 IF THE USER HAS WON, ELSE 0.
C
      EXTERNAL OUTSEQ
      INTEGER  ICMP, IUSR
      INTEGER  ILAST

      ILAST = NINT(RAND(0) * 7)
      PRINT 100
      CALL OUTSEQ(ILAST)

   10 CONTINUE
      IF (ICMP .EQ. ILAST) THEN
        TOSS = 0
        RETURN
      ELSE IF (IUSR .EQ. ILAST) THEN
        TOSS = 1
        RETURN
      END IF

      ILAST = IOR(IAND(ISHFT(ILAST, 1), 6), NINT(RAND(0)))

      IF (IAND(ILAST, 1) .EQ. 1) THEN
        PRINT 200, 'H'
      ELSE
        PRINT 200, 'T'
      END IF
      GOTO 10

  100 FORMAT (/,' TOSSED SEQUENCE: ',$)
  200 FORMAT (A,$)
      END
C     ******************************************************************
      SUBROUTINE OUTSEQ(ISEQ)
C
C     PRINTS THE GIVEN SEQUENCE OF THREE TO SCREEN.
C
      INTEGER ISEQ
      INTEGER I

      DO 10 I = 2, 0, -1
      IF (IAND(ISEQ, ISHFT(1, I)) .GT. 0) THEN
        PRINT 100, 'H'
      ELSE
        PRINT 100, 'T'
      END IF
   10 CONTINUE

  100 FORMAT (A,$)
      END
C     ******************************************************************
      SUBROUTINE PLAY()
C
C     THE GAME STARTS HERE.
C
      EXTERNAL OUTSEQ
      INTEGER  CMPSEQ, USRSEQ, TOSS
      INTEGER  ICMP, IUSR, IWIN

      IUSR = -1
      ICMP = -1

      IF (RAND(0) .GE. 0.5) THEN
        PRINT 100
        IUSR = USRSEQ(ICMP)
        ICMP = CMPSEQ(IUSR)
        PRINT 300
        CALL OUTSEQ(ICMP)
        PRINT *
      ELSE
        PRINT 200
        ICMP = CMPSEQ(IUSR)
        PRINT 300
        CALL OUTSEQ(ICMP)
        IUSR = USRSEQ(ICMP)
      END IF

      IWIN = TOSS(ICMP, IUSR)

      IF (IWIN .EQ. 1) THEN
        PRINT 400
      ELSE
        PRINT 500
      END IF

  100 FORMAT (/,' YOU PICK FIRST.',$)
  200 FORMAT (/,' THE COMPUTER PICKS FIRST.')
  300 FORMAT (' THE COMPUTER PICKED: ',$)
  400 FORMAT (/,' YOU WIN!')
  500 FORMAT (/,' THE COMPUTER WINS!')
      END
