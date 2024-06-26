C     ******************************************************************
C
C     MAGIC 8-BALL IN FORTRAN 77.
C
C     ******************************************************************
      PROGRAM MAGIC8
      CHARACTER*25 ANSY(20), QUEST
      INTEGER      I, IR, ISTAT

      DATA ANSY /'IT IS CERTAIN','IT IS DECIDEDLY SO',
     &'WITHOUT A DOUBT','YES, DEFINITELY','YOU MAY RELY ON IT',
     &'AS I SEE IT, YES','MOST LIKELY','OUTLOOK GOOD',
     &'SIGNS POINT TO YES','YES','REPLY HAZY, TRY AGAIN',
     &'ASK AGAIN LATER','BETTER NOT TELL YOU NOW','CANNOT PREDICT NOW',
     &'CONCENTRATE AND ASK AGAIN','DON''T BET ON IT','MY REPLY IS NO',
     &'MY SOURCES SAY NO','OUTLOOK NOT SO GOOD','VERY DOUBTFUL'/
C
C     INTIALISE RANDOM NUMBER GENERATOR USING CURRENT TIME.
C
      CALL SRAND(TIME())
C
C     READ QUESTION FROM INPUT.
C
      PRINT 100
  100 FORMAT (' ASK A QUESTION: ',$)
      READ (*, 200, IOSTAT=ISTAT) QUEST
  200 FORMAT (A)
      IF (ISTAT .NE. 0 .OR. QUEST .EQ. ' ') STOP
C
C     THROW AWAY THE FIRST 99 RANDOM NUMBERS FOR HIGHER ENTROPY.
C
      DO 10 I = 1, 100
      IR = 1 + INT(RAND(0) * SIZE(ANSY))
   10 CONTINUE
C
C     PRINT THE ANSWER.
C
      PRINT 300, ANSY(IR)
  300 FORMAT (1X,A)
      END
