C     ******************************************************************
C
C     GUILLOTINE
C
C     FRENCH VARIANT OF THE HANGMAN GAME BY PHILIPP ENGEL, 2022.
C     ADAPTED FROM THE PYTHON VERSION BY AL SWEIGART.
C
C     ******************************************************************
      PROGRAM FRENCH
      EXTERNAL  NEW, NEXT
      CHARACTER A
      INTEGER   INEXT

      CALL SRAND(ABS(TIME()))

   10 CONTINUE
      CALL NEW()
   20 CONTINUE
      CALL NEXT(INEXT)
      IF (INEXT .EQ. 0) GOTO 20
      PRINT 100
      READ (*, 200) A
      IF (A .EQ. 'Y' .OR. A .EQ. 'y') GOTO 10

  100 FORMAT (' ANOTHER GAME? (Y/N) ',$)
  200 FORMAT (A1)
      END
C     ******************************************************************
      INTEGER FUNCTION LTRIM(STR)
C
C     RETURNS LENGTH OF TRIMMED STRING, LIKE LEN_TRIM() IN FORTRAN 90.
C
      CHARACTER*(*) STR

      DO 10, LTRIM = LEN(STR), 1, -1
      IF (STR(LTRIM:LTRIM) .NE. ' ') RETURN
  10  CONTINUE
      END
C     ******************************************************************
      SUBROUTINE CLS()
C
C     CLEARS THE SCREEN AND MOVES CURSOR TO UPPER LEFT CORNER, USING
C     ANSI ESCAPE SEQUENCES.
C
      PRINT 100, ACHAR(27), ACHAR(27)
  100 FORMAT (A1,'[2J',A1,'[0;0H',$)
      END
C     ******************************************************************
      SUBROUTINE NEW()
C
C     CHOOSES A RANDOM SECRET WORD.
C
      INTEGER     LTRIM
      CHARACTER*8 CATE, SECR, MISS, CORR, WORDY(64)
      INTEGER     NSECR, NMISS, NCORR
      INTEGER     I, J

      COMMON /STATE/ CATE, SECR, MISS, CORR, NSECR, NMISS, NCORR
      COMMON /WORDS/ WORDY

      DO 10 I = 1, 1000
      J = 1 + INT(RAND(0) * SIZE(WORDY))
   10 CONTINUE

      SECR = WORDY(J)
      MISS = ' '
      CORR = ' '

      NSECR = LTRIM(SECR)
      NMISS = 0
      NCORR = 0
      END
C     ******************************************************************
      SUBROUTINE NEXT(INEXT)
C
C     READS AND MATCHES INPUT. IF THE CURRENT GAME IS FINISHED, INEXT IS
C     SET TO -1, ELSE 0.
C
      EXTERNAL    CLS, OUTPUT
      CHARACTER   A
      CHARACTER*8 CATE, SECR, MISS, CORR
      INTEGER     NSECR, NMISS, NCORR
      INTEGER     I, INEXT, ISTAT, N

      COMMON /STATE/ CATE, SECR, MISS, CORR, NSECR, NMISS, NCORR

      INEXT = 0
C
C     CLEAR SCREEN AND PRINT GUILLOTINE.
C
      CALL CLS()
      PRINT 100
      CALL OUTPUT(NMISS)
      PRINT 110, CATE
C
C     PRINT MISSED CHARACTERS.
C
      DO 10 I = 1, NMISS
      IF (MISS(I:I) .NE. ' ') PRINT 120, MISS(I:I)
   10 CONTINUE
      PRINT *
C
C     PRINT GUESSED CHARACTERS.
C
      PRINT *
      DO 20 I = 1, NSECR
      IF (CORR(I:I) .EQ. ' ') THEN
        PRINT 120, '_'
      ELSE
        PRINT 120, CORR(I:I)
      END IF
   20 CONTINUE
      PRINT 130
C
C     GUESSED THE WORD?
C
      IF (NCORR .EQ. NSECR) THEN
        INEXT = 1
        IF (NMISS .EQ. 0) THEN
          PRINT 140
        ELSE
          PRINT 150, NMISS
        END IF
        RETURN
      END IF
C
C     READ SINGLE CHARACTER FROM INPUT.
C
   30 PRINT 160
      READ (*, 200, IOSTAT=ISTAT) A
      IF (ISTAT .NE. 0) GOTO 30
C
C     CONVERT TO UPPER CASE.
C
      IF (A .GE. 'a' .AND. A .LE. 'z') THEN
        A = ACHAR(IACHAR(A) - 32)
      END IF
C
C     CHARACTER NOT IN RANGE A - Z?
C
      IF (A .LT. 'A' .OR. A .GT. 'Z') THEN
        PRINT 170
        GOTO 30
      END IF
C
C     CHARACTER ALREADY GUESSED?
C
      DO 40 I = 1, NSECR
      IF (A .EQ. MISS(I:I) .OR. A .EQ. CORR(I:I)) THEN
        PRINT 180
        GOTO 30
      END IF
   40 CONTINUE
C
C     CHARACTER IN SECRET WORD?
C
      N = 0

      DO 50 I = 1, NSECR
      IF (A .EQ. SECR(I:I)) THEN
        CORR(I:I) = A
        N = N + 1
      END IF
   50 CONTINUE
C
C     COUNT CORRECT LETTERS.
C
      NCORR = NCORR + N

      IF (N .EQ. 0) THEN
        NMISS = NMISS + 1
        MISS(NMISS:NMISS) = A
      END IF
C
C     MAX. NUMBER OF TRIES REACHED?
C
      IF (NMISS .LT. LEN(SECR)) RETURN
C
C     PLAY GUILLOTINE ANIMATION.
C
      DO 60 I = NMISS, 10
      CALL CLS()
      PRINT 100
      CALL OUTPUT(I)
      CALL SLEEP(1)
   60 CONTINUE

      PRINT 190, SECR
      INEXT = -1

  100 FORMAT (/,22X,' GUILLOTINE',/,
     &10X,' FRENCH VARIANT OF THE HANGMAN GAME',/)
  110 FORMAT (' THE CATEGORY IS ',A,/,' MISSED LETTERS:',$)
  120 FORMAT (1X,A1,$)
  130 FORMAT (/)
  140 FORMAT (' PERFECT!')
  150 FORMAT (' YOU MISSED ',I1,' LETTERS.')
  160 FORMAT (' GUESS A LETTER: ',$)
  170 FORMAT (' INVALID CHARACTER. GUESS AGAIN.')
  180 FORMAT (' YOU HAVE ALREADY GUESSED THAT LETTER. GUESS AGAIN.')
  190 FORMAT (' OOH-LA-LA, YOU''RE DEAD -- THE WORD WAS ',A,/,
     &' VIVE LA FRANCE!')
  200 FORMAT (A1)
      END
C     ******************************************************************
      SUBROUTINE OUTPUT(NMISS)
C
C     PRINTS CURRENT PICTURE OF GUILLOTINE.
C
      INTEGER      NMISS
      CHARACTER*40 LINE, PICY(11)
      INTEGER      I

      COMMON /PICS/ PICY

      DO 10 I = 1, 40, 5
      LINE = PICY(1 + NMISS)
      PRINT 100, LINE(I:I + 5)
   10 CONTINUE
      PRINT *
  100 FORMAT (5X,A5)
      END
C     ******************************************************************
      BLOCK DATA
C
C     COMMON VARIABLES.
C
      CHARACTER*8  CATE, SECR, MISS, CORR, WORDY(64)
      CHARACTER*40 PICY(11)
      INTEGER      NSECR, NMISS, NCORR

      COMMON /STATE/ CATE, SECR, MISS, CORR, NSECR, NMISS, NCORR
      COMMON /WORDS/ WORDY
      COMMON /PICS/  PICY

      DATA CATE /'ANIMALS'/
      DATA WORDY /'ANT','BABOON','BADGER','BAT','BEAR','BEAVER','CAMEL',
     &'CAT','CLAM','COBRA','COUGAR','COYOTE','CROW','DEER','DOG',
     &'DONKEY','DUCK','EAGLE','FERRET','FOX','FROG','GOAT','GOOSE',
     &'HAWK','LION','LIZARD','LLAMA','MOLE','MONKEY','MOOSE','MOUSE',
     &'MULE','NEWT','OTTER','OWL','PANDA','PARROT','PIGEON','PYTHON',
     &'RABBIT','RAM','RAT','RAVEN','RHINO','SALMON','SEAL','SHARK',
     &'SHEEP','SKUNK','SLOTH','SNAKE','SPIDER','STORK','SWAN','TIGER',
     &'TOAD','TROUT','TURKEY','TURTLE','WEASEL','WHALE','WOLF','WOMBAT',
     &'ZEBRA'/
      DATA PICY /'                                        ',
     &           '|    |    |    |    |    |    |    |=== ',
     &           '|   ||   ||   ||   ||   ||   ||   ||===|',
     &           '|===||   ||   ||   ||   ||   ||   ||===|',
     &           '|===||   ||   ||   ||   ||   ||\ /||===|',
     &           '|===||   ||   ||   || _ ||/ \||\ /||===|',
     &           '|===||| /|||/ ||   || _ ||/ \||\ /||===|',
     &           '|===||| /|||/ ||   || _ ||/ \||\O/||===|',
     &           '|===||___||| /|||/ || _ ||/ \||\O/||===|',
     &           '|===||   ||   ||___|||_/||//\||\O/||===|',
     &           '|===||   ||   ||   || _ ||/_\||\ /||=O=|'/
      END
