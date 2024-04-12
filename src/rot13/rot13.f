C     ******************************************************************
C
C     BASIC ROT-13 ENCODER/DECODER IN FORTRAN 77
C
C     ******************************************************************
      PROGRAM ROT13
      INTEGER       LTRIM
      CHARACTER*256 MSG
      INTEGER       I, J, ISTAT
C
C     READ INPUT MESSAGE.
C
      PRINT 100
      READ (*, 200, IOSTAT=ISTAT) MSG
      IF (ISTAT .NE. 0) STOP

  100 FORMAT ('ENTER MESSAGE: ',$)
  200 FORMAT (A)
C
C     PRINT EACH CHARACTER.
C
      DO 10 I = 1, LTRIM(MSG)
      J = ICHAR(MSG(I:I))
      IF (J .GT. 64 .AND. J .LT. 78) THEN
        J = J + 13
      ELSE IF (J .GT. 77 .AND. J .LT. 91) THEN
        J = J - 13
      ELSE IF (J .GT. 96 .AND. J .LT. 100) THEN
        J = J + 13
      ELSE IF (J .GT. 109 .AND. J .LT. 123) THEN
        J = J - 13
      END IF
      PRINT 300, CHAR(J)
   10 CONTINUE

  300 FORMAT (A1,$)
      PRINT *
      END
C     ******************************************************************
      INTEGER FUNCTION LTRIM(STR)
C
C     RETURNS LENGTH OF TRIMMED STRING, LIKE LEN_TRIM() IN FORTRAN 90.
C
      CHARACTER*(*) STR
      INTEGER       I

      DO 10, I = LEN(STR), 1, -1
      IF (STR(I:I) .NE. ' ') GOTO 20
   10 CONTINUE
   20 LTRIM = I
      END
