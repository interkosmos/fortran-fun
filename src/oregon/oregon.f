C     ******************************************************************
C
C     THE OREGON TRAIL IN FORTRAN 77
C
C     ******************************************************************
C
C     THIS PROGRAM IS A PORT OF THE 1978 VERSION OF "THE OREGON TRAIL"
C     TO ANSI FORTRAN 77, ORIGINALLY WRITTEN IN HP TIME-SHARED BASIC BY
C     DON RAWITSCH, BILL HEINEMANN, AND PAUL DILLENBERGER IN 1971.
C
C     AN ADDITIONAL INTEGER FUNCTION "TIME()" THAT RETURNS THE CURRENT
C     TIME AS A TIMESTAMP IN SECONDS IS REQUIRED AND MUST BE LINKED
C     WITH THE EXECUTABLE, IF NOT PROVIDED BY THE FORTRAN COMPILER
C     ALREADY.
C
C     ******************************************************************
C     AUTHOR:  PHILIPP ENGEL
C     DATE:    2023-11-29
C     VERSION: 1.2
C     LICENCE: ISC
C     ******************************************************************
      PROGRAM OREGON
      EXTERNAL INSTR, PLAY, SRAND
      INTEGER  TIME
      LOGICAL  ASK

      CALL SRAND(TIME())
      PRINT 100
      IF (ASK()) CALL INSTR()
      CALL PLAY()

  100 FORMAT (1X,'TYPE "STOP" AT ANY TIME TO QUIT.',/,
     &1X,'DO YOU NEED INSTRUCTIONS? (Y/N)')
      END
C     ******************************************************************
      LOGICAL FUNCTION ASK()
C
C     READS USER INPUT AND RETURNS TRUE IF INPUT STARTS WITH "Y", AND
C     FALSE ON ANY OTHER INPUT.
C
      EXTERNAL    UPPER
      CHARACTER*4 A
      INTEGER     ISTAT

      ASK = .FALSE.
      READ (*, 100, IOSTAT=ISTAT) A
      IF (ISTAT .NE. 0) RETURN
      CALL UPPER(A)
      IF (A .EQ. 'STOP') STOP
      IF (A .NE. 'Y') RETURN
      ASK = .TRUE.

  100 FORMAT (A)
      END
C     ******************************************************************
      INTEGER FUNCTION INPUT(IMIN, IMAX)
C
C     READS INTEGER VALUE WITH GIVEN MINIUM AND MAXIMUM FROM USER INPUT.
C
      EXTERNAL    UPPER
      INTEGER     IMIN, IMAX
      CHARACTER*4 A
      INTEGER     ISTAT

   10 CONTINUE
      READ (*, 100, IOSTAT=ISTAT) A
      IF (ISTAT .NE. 0) THEN
        PRINT 200
        GOTO 10
      END IF
      CALL UPPER(A)
      IF (A .EQ. 'STOP') STOP
      READ (A, *, IOSTAT=ISTAT) INPUT
      IF (ISTAT .NE. 0) THEN
        PRINT 200
        GOTO 10
      END IF
      IF (INPUT .LT. IMIN) THEN
        PRINT 300
        GOTO 10
      ELSE IF (INPUT .GT. IMAX) THEN
        PRINT 400
        GOTO 10
      END IF

  100 FORMAT (A)
  200 FORMAT (1X,'INVALID.')
  300 FORMAT (1X,'TOO LOW.')
  400 FORMAT (1X,'TOO HIGH.')
      END
C     ******************************************************************
      REAL FUNCTION RAND()
C
C     THIS FUNCTION RETURNS A PSEUDO-RANDOM NUMBER FOR EACH INVOCATION.
C     IT IS A FORTRAN 77 ADAPTATION OF THE "INTEGER VERSION 2" MINIMAL
C     STANDARD NUMBER GENERATOR WHOSE PASCAL CODE APPEARS IN THE
C     ARTICLE:
C
C         PARK, STEVEN K. AND MILLER, KEITH W., "RANDOM NUMBER
C         GENERATORS: GOOD ONES ARE HARD TO FIND", COMMUNICATIONS OF
C         THE ACM, OCTOBER, 1988.
C
C     ORIGINAL FORTRAN 77 IMPLEMENTATION BY DICK VALENT AND FRED CLARE.
C
      INTEGER MPLIER, MODLUS, MOBYMP, MOMDMP
      PARAMETER (MPLIER=16807, MODLUS=2147483647, MOBYMP=127773,
     &           MOMDMP=2836)
      INTEGER JSEED, IFRST
      INTEGER HVLUE, LVLUE, TESTV, NEXTN
      COMMON  /SEED/ JSEED, IFRST
      SAVE    NEXTN

      IF (IFRST .EQ. 0) THEN
        NEXTN = JSEED
        IFRST = 1
      END IF
      HVLUE = NEXTN / MOBYMP
      LVLUE = MOD(NEXTN, MOBYMP)
      TESTV = MPLIER * LVLUE - MOMDMP * HVLUE
      IF (TESTV .GT. 0) THEN
        NEXTN = TESTV
      ELSE
        NEXTN = TESTV + MODLUS
      END IF
      RAND = REAL(NEXTN) / REAL(MODLUS)
      END
C     ******************************************************************
      INTEGER FUNCTION SHOOT(ILEVL)
C
C     SHOOT STUFF BY LETTING THE PLAYER ENTER INSTANCES OF ONOMATOPOEIA.
C
      EXTERNAL    UPPER
      INTEGER     TIME
      REAL        RAND
      INTEGER     ILEVL
      CHARACTER*4 A, S(4)
      INTEGER     I, T1, T2
      DATA S /'BANG','BLAM','POW','WHAM'/

      I = INT(RAND() * 4 + 1)
      PRINT 100, S(I)
      T1 = TIME()
      READ (*, 200) A
      T2 = TIME()
      SHOOT = (ABS(T2 - T1) * 2) - ILEVL - 1
      CALL UPPER(A)
      IF (A .EQ. 'STOP') STOP
      IF (A .NE. S(I)) SHOOT = 9

  100 FORMAT (1X,'TYPE:',1X,A)
  200 FORMAT (A)
      END
C     ******************************************************************
      INTEGER FUNCTION SKILL()
C
C     RETURNS THE MARKSMANSHIP LEVEL (1 TO 5).
C
      INTEGER INPUT

      PRINT 100
      SKILL = INPUT(1, 5)

  100 FORMAT (1X,'HOW GOOD A SHOT ARE YOU WITH YOUR RIFLE?',/,/,
     &3X,'(1) ACE MARKSMAN, (2) GOOD SHOT, (3) FAIR TO MIDDLIN''',/,
     &8X,'(4) NEED MORE PRACTICE, (5) SHAKY KNEES',/,/,
     &1X,'ENTER ONE OF THE ABOVE -- THE BETTER YOU CLAIM YOU ARE,',/,
     &1X,'THE FASTER YOU''LL HAVE TO BE WITH YOUR GUN TO BE ',
     &'SUCCESSFUL.')
      END
C     ******************************************************************
      SUBROUTINE ARRIVE(ITMIL, ILMIL, ICTRN, ICASH, IAMMU, ICLTH, IFOOD,
     &                  IMISC, IEATS)
C
C     FINAL TURN.
C
      INTEGER      ITMIL, ILMIL, ICTRN, ICASH
      INTEGER      IAMMU, ICLTH, IFOOD, IMISC, IEATS
      CHARACTER*10 AWEEKY(7)
      INTEGER      IFINL
      REAL         FFRAC
      COMMON /WDAYS/ AWEEKY

      FFRAC = (2040.0 - ILMIL) / (ITMIL - ILMIL)
      IFOOD = IFOOD + (1 - INT(FFRAC)) * (8 + 5 * IEATS)
      PRINT 100
      IFINL = INT(FFRAC * 14)
      ICTRN = ICTRN * 14 + IFINL
      IFINL = IFINL + 1

      IF (IFINL .GT. 7) IFINL = IFINL - 7
      IF (ICTRN .LE. 124) THEN
        ICTRN = ICTRN - 93
        PRINT 200, AWEEKY(IFINL), 'JULY', ICTRN
      ELSE IF (ICTRN .LE. 155) THEN
        ICTRN = ICTRN - 124
        PRINT 200, AWEEKY(IFINL), 'AUGUST', ICTRN
      ELSE IF (ICTRN .LE. 185) THEN
        ICTRN = ICTRN - 155
        PRINT 200, AWEEKY(IFINL), 'SEPTEMBER', ICTRN
      ELSE IF (ICTRN .LE. 216) THEN
        ICTRN = ICTRN - 185
        PRINT 200, AWEEKY(IFINL), 'OCTOBER', ICTRN
      ELSE IF (ICTRN .LE. 246) THEN
        ICTRN = ICTRN - 216
        PRINT 200, AWEEKY(IFINL), 'NOVEMBER', ICTRN
      ELSE
        ICTRN = ICTRN - 246
        PRINT 200, AWEEKY(IFINL), 'DECEMBER', ICTRN
      END IF

      PRINT 300, IFOOD, IAMMU, ICLTH, IMISC, ICASH
      PRINT 400
      STOP

  100 FORMAT (/,1X,'YOU FINALLY ARRIVED AT OREGON CITY',/,
     &1X,'AFTER 2040 LONG MILES -- HOORAY!!',/,1X,'A REAL PIONEER!',/)
  200 FORMAT (1X,A,1X,A,1X,I2,1X,'1847')
  300 FORMAT (1X,60('-'),/,1X,'FOOD BULLETS CLOTHING MISC.SUPP. CASH',/,
     &1X,I4,1X,I7,1X,I8,1X,I10,1X,I4)
  400 FORMAT (/,10X,'PRESIDENT JAMES K. POLK SENDS YOU HIS',/,
     &16X,'HEARTIEST CONGRATULATIONS',/,/,10X,'AND WISHES YOU A ',
     &'PROSPEROUS LIFE AHEAD',/,21X,'AT YOUR NEW HOME',/)
      END
C     ******************************************************************
      SUBROUTINE BLIZZ(ITMIL, IAMMU, ICLTH, IFOOD, IMISC, IFILL, IFINJ,
     &                 IEATS)
C
C     BLIZZARD IN MOUNTAIN PASS.
C
      EXTERNAL SICK
      REAL     RAND
      INTEGER  ITMIL, IAMMU, ICLTH, IFOOD, IMISC, IFILL, IFINJ, IEATS

      PRINT 100
      IFOOD = IFOOD - 25
      IMISC = IMISC - 10
      IAMMU = IAMMU - 300
      ITMIL = ITMIL - 30 - INT(40 * RAND())

      IF (ICLTH .LT. 18 + INT(2 * RAND())) THEN
        CALL SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
      END IF

  100 FORMAT (1X,'BLIZZARD IN MOUNTAIN PASS -- TIME AND SUPPLIES LOST.')
      END
C     ******************************************************************
      SUBROUTINE DIE()
C
C     READS FINAL INFORMATION AND STOPS GAME.
C
      LOGICAL ASK
      LOGICAL L

      PRINT 100
      L = ASK()
      PRINT 200
      L = ASK()
      PRINT 300
      IF (ASK()) THEN
        PRINT 400
      ELSE
        PRINT 500
      END IF
      PRINT 600
      STOP

  100 FORMAT (/,1X,'DUE TO YOUR UNFORTUNATE SITUATION, THERE ARE A FEW',
     &/,1X,'FORMALITIES WE MUST GO THROUGH:',/,/,1X,'WOULD YOU LIKE A ',
     &'MINISTER? (Y/N)')
  200 FORMAT (1X,'WOULD YOU LIKE A FANCY FUNERAL? (Y/N)')
  300 FORMAT (1X,'WOULD YOU LIKE US TO INFORM YOUR NEXT OF KIN? (Y/N)')
  400 FORMAT (1X,'THAT WILL BE $4.50 FOR THE TELEGRAPH CHARGE.')
  500 FORMAT (1X,'BUT YOUR AUNT SADIE IN ST. LOUIS IS REALLY WORRIED',/,
     &1X,'ABOUT YOU.')
  600 FORMAT (/,1X,'WE THANK YOU FOR THIS INFORMATION AND WE ARE ',
     &'SORRY YOU',/,1X,'DIDN''T MAKE IT TO THE GREAT TERRITORY OF ',
     &'OREGON.',/,1X,'BETTER LUCK NEXT TIME.',/,/,31X,'SINCERLY,',/,
     &18X,'THE OREGON CITY CHAMBER OF COMMERCE',/)
      END
C     ******************************************************************
      SUBROUTINE DOCTOR(ICASH, IFILL, IFINJ)
C
C     VISIT OL' DOC BLANCHARD.
C
      INTEGER ICASH, IFILL, IFINJ

      ICASH = ICASH - 20
      IF (ICASH .LT. 0) THEN
        PRINT 100
        IF (IFINJ .EQ. 1) THEN
          PRINT 200
        ELSE
          PRINT 300
        END IF
        CALL DIE()
      END IF
      PRINT 400
      IFILL = 0
      IFINJ = 0

  100 FORMAT (1X,'YOU CAN''T AFFORD A DOCTOR.')
  200 FORMAT (1X,'YOU DIED OF INJURIES.')
  300 FORMAT (1X,'YOU DIED OF PNEUMONIA.')
  400 FORMAT (1X,'DOCTOR''S BILL IS $20.')
      END
C     ******************************************************************
      SUBROUTINE EAT(IFOOD, IEATS)
C
C     LETS THE PLAYER DECIDE HOW TO EAT.
C
      INTEGER IFOOD, IEATS
      INTEGER IAMOU, INPUT

   10 CONTINUE
      PRINT 100
      IEATS = INPUT(1, 3)
      IAMOU = 8 + 5 * IEATS
      IF (IFOOD - IAMOU .LT. 0) THEN
        PRINT 200
        GOTO 10
      END IF
      IFOOD = IFOOD - IAMOU

  100 FORMAT (1X,'DO YOU WANT TO EAT',/,
     &1X,'(1) POORLY, (2) MODERATELY, (3) WELL')
  200 FORMAT (1X,'YOU CAN''T EAT THAT WELL.')
      END
C     ******************************************************************
      SUBROUTINE FORT(ICASH, IFOOD, IAMMU, ICLTH, IMISC)
C
C     YE OLDE FORT SHOPPE.
C
      INTEGER INPUT
      INTEGER ICASH, IFOOD, IAMMU, ICLTH, IMISC
      INTEGER ISPND

      IF (ICASH .LE. 0) THEN
        PRINT 100
        RETURN
      END IF
      PRINT 200
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      IFOOD = IFOOD + INT(2. / 3. * ISPND)
      PRINT 300
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      IAMMU = IAMMU + INT(2. / 3. * ISPND * 50)
      PRINT 400
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      ICLTH = ICLTH + INT(2. / 3. * ISPND)
      PRINT 500
      ISPND = INPUT(0, ICASH)
      ICASH = ICASH - ISPND
      IMISC = IMISC + INT(2. / 3. * ISPND)

  100 FORMAT (1X,'YOU DON''T HAVE ANY MONEY TO SPEND.')
  200 FORMAT (1X,'ENTER WHAT YOU WISH TO SPEND ON THE FOLLOWING:',/,
     &1X,'FOOD')
  300 FORMAT (1X,'AMMUNITION')
  400 FORMAT (1X,'CLOTHING')
  500 FORMAT (1X,'MISCELLANEOUS SUPPLIES')
      END
C     ******************************************************************
      SUBROUTINE HUNT(IAMMU, ILEVL, IFOOD)
C
C     HUNTING POOR WILDLIFE.
C
      INTEGER SHOOT
      REAL    RAND
      INTEGER IAMMU, ILEVL, IFOOD
      INTEGER IBANGT

      IF (IAMMU .LT. 39) THEN
        PRINT 100
        RETURN
      END IF
      IBANGT = SHOOT(ILEVL)
      IF (IBANGT .LE. 1) THEN
        PRINT 200
        IFOOD = IFOOD + 52 + INT(RAND() * 6)
        IAMMU = IAMMU - 10 - INT(RAND() * 4)
      ELSE IF (100 * RAND() .LT. 13 * IBANGT) THEN
        PRINT 300
      ELSE
        PRINT 400
        IFOOD = IFOOD + 48 - 2 * IBANGT
        IAMMU = IAMMU - 10 - 3 * IBANGT
      END IF

  100 FORMAT (1X,'TOUGH -- YOU NEED MORE BULLETS TO GO HUNTING.')
  200 FORMAT (1X,'RIGHT BETWEEN THE EYES -- YOU GOT A BIG ONE!!',/,
     &1X,'FULL BELLIES TONIGHT!')
  300 FORMAT (1X,'YOU MISSED -- AND YOUR DINNER GOT AWAY ...')
  400 FORMAT (1X,'NICE SHOT -- RIGHT ON TARGET -- GOOD EATIN'' ',
     &'TONIGHT!!')
      END
C     ******************************************************************
      SUBROUTINE INSTR()
C
C     OUTPUTS THE GAME INSTRUCTIONS.
C
      PRINT 100
      READ (*, 400)
      PRINT 200
      READ (*, 400)
      PRINT 300
      READ (*, 400)

  100 FORMAT (1X,'THIS PROGRAM SIMULATES A TRIP OVER THE OREGON TRAIL ',
     &'FROM',/,1X,'INDEPENDENCE, MISSOURI TO OREGON CITY, OREGON IN ',
     &'1847.',/,1X,'YOUR FAMILY OF FIVE WILL COVER THE 2040 MILE ',
     &'OREGON',/,1X,'TRAIL IN 5-6 MONTHS -- IF YOU MAKE IT ALIVE.',/,/,
     &1X,'YOU HAD SAVED $900 TO SPEND FOR THE TRIP, AND YOU''VE JUST',/,
     &1X,'PAID $200 FOR A WAGON.',/,/,1X,'YOU WILL NEED TO SPEND THE ',
     &'REST OF YOUR MONEY ON THE',/,1X,'FOLLOWING ITEMS:',/,/,
     &1X,'PRESS RETURN KEY.')
  200 FORMAT (3X,'OXEN - YOU CAN SPEND $200-$300 ON YOUR TEAM. THE ',
     &'MORE',/,10X,'YOU SPEND, THE FASTER YOU''LL GO BECAUSE',/,10X,
     &'YOU''LL HAVE BETTER ANIMALS.',/,/,3X,'FOOD - THE MORE YOU ',
     &'HAVE, THE LESS CHANCE THERE IS OF',/,10X,'GETTING SICK.',/,/,
     &3X,'AMMUNITION - $1 BUYS A BELT OF 50 BULLETS. YOU WILL',/,10X,
     &'NEED BULLETS FOR ATTACKS BY ANIMALS AND',/,10X,'BANDITS, AND ',
     &'FOR HUNTING FOOD.',/,/,3X,'CLOTHING - THIS IS ESPECIALLY ',
     &'IMPORTANT FOR THE COLD',/,10X,'WEATHER YOU WILL ENCOUNTER ',
     &'WHEN CROSSING THE',/,10X,'MOUNTAINS.',/,/,3X,'MISCELLANEOUS ',
     &'SUPPLIES - THIS INCLUDES MEDICINE AND',/,10X,'OTHER THINGS ',
     &'YOU WILL NEED FOR SICKNESS AND',/,10X,'EMERGENCY REPAIRS.',/,/,
     &1X,'PRESS RETURN KEY.')
  300 FORMAT (1X,'YOU CAN SPEND ALL YOUR MONEY BEFORE YOU START YOUR',/,
     &1X,'TRIP -- OR YOU CAN SAVE SOME OF YOUR CASH TO SPEND AT',/,
     &1X,'FORTS ALONG THE WAY WHEN YOU RUN LOW. HOWEVER, ITEMS',/,
     &1X,'COST MORE AT THE FORTS. YOU CAN ALSO GO HUNTING ALONG',/,
     &1X,'THE WAY TO GET MORE FOOD.',/,/,1X,'WHENEVER YOU HAVE TO USE ',
     &1X,'YOUR TRUSTY RIFLE ALONG THE ',/,1X,'WAY, YOU WILL BE TOLD ',
     &1X,'TO TYPE IN A WORD (ONE THAT SOUNDS',/,1X,'LIKE A GUN SHOT). ',
     &'THE FASTER YOU TYPE IN THAT WORD AND',/,1X,'THE RETURN KEY, ',
     &'THE BETTER LUCK YOU''LL HAVE WITH',/,1X,'YOUR GUN.',/,/,1X,'AT ',
     &'EACH TURN, ALL ITEMS ARE SHOWN IN DOLLAR AMOUNTS',/,1X,'EXCEPT ',
     &'BULLETS.',/,/,1X,'WHEN ASKED TO ENTER MONEY AMOUNTS, DON''T ',
     &'USE A $.',/,/,1X,'GOOD LUCK!!',/,/,1X,'PRESS RETURN KEY.')
  400 FORMAT (A)
      END
C     ******************************************************************
      SUBROUTINE PLAY()
C
C     INITIAL ROUTINE. GAME STARTS HERE.
C
      EXTERNAL ARRIVE, BLIZZ, DIE, DOCTOR, EAT, FORT, HUNT, RIDERS
      EXTERNAL SHOP, SICK
      INTEGER  INPUT, SHOOT, SKILL
      REAL     RAND

      CHARACTER*17 ADATEY(20)
      INTEGER      I, IBANGT, IEVNTY(15), ISELEC
      INTEGER      IAMMU, IANIM, ICLTH, IFOOD, IMISC, IEVTC, ICTRN,
     &             ILEVL, IEATS, IF950, IFPAS, IFMOU, IFFRT, IFILL,
     &             IFINJ, ITMIL, ILMIL, ICASH
      REAL         R

      COMMON /EVNTS/ IEVNTY
      COMMON /STATE/ IAMMU, IANIM, ICLTH, IFOOD, IMISC, IEVTC, ICTRN,
     &               ILEVL, IEATS, IF950, IFPAS, IFMOU, IFFRT, IFILL,
     &               IFINJ, ITMIL, ILMIL, ICASH
      COMMON /DATES/ ADATEY

      ILEVL = SKILL()
      CALL SHOP(ICASH, IANIM, IFOOD, IAMMU, ICLTH, IMISC)
      DO 10 I = 1, 20
      ICASH = MAX(0, ICASH)
      IAMMU = MAX(0, IAMMU)
      IANIM = MAX(0, IANIM)
      ICLTH = MAX(0, ICLTH)
      IFOOD = MAX(0, IFOOD)
      IMISC = MAX(0, IMISC)
      IF (ITMIL .GE. 2040) THEN
        CALL ARRIVE(ITMIL, ILMIL, ICTRN, ICASH, IAMMU,
     &              ICLTH, IFOOD, IMISC, IEATS)
      END IF
      ICTRN = ICTRN + 1
      PRINT 100, ADATEY(I)
      ILMIL = ITMIL
      IF (IFILL .EQ. 1 .OR. IFINJ .EQ. 1) THEN
        CALL DOCTOR(ICASH, IFILL, IFINJ)
      END IF
      IF (IF950 .EQ. 1) THEN
        IF950 = 0
        PRINT 110, 950
      ELSE
        PRINT 110, ITMIL
      END IF
      IF (IFOOD .LE. 13) PRINT 120
      PRINT 130, IFOOD, IAMMU, ICLTH, IMISC, ICASH
      IF (IFFRT .EQ. -1) THEN
        PRINT 140
        ISELEC = INPUT(1, 3)
        IF (ISELEC .EQ. 1) THEN
          CALL FORT(ICASH, IFOOD, IAMMU, ICLTH, IMISC)
          ITMIL = ITMIL - 45
        ELSE IF (ISELEC .EQ. 2) THEN
          CALL HUNT(IAMMU, ILEVL, IFOOD)
          ITMIL = ITMIL - 45
        END IF
      ELSE
        PRINT 150
        ISELEC = INPUT(1, 2)
        IF (ISELEC .EQ. 1) THEN
          CALL HUNT(IAMMU, ILEVL, IFOOD)
          ITMIL = ITMIL - 45
        END IF
      END IF
      IF (IFOOD .GE. 13) THEN
        CALL EAT(IFOOD, IEATS)
      ELSE
        PRINT 160
        CALL DIE()
      END IF
      ITMIL = ITMIL + 200 + INT((IANIM - 220) / 5 + 10 * RAND())
      R = ((ITMIL/100 - 4)**2 + 72) / ((ITMIL/100 - 4)**2 + 12) - 1
      IF (RAND() * 10 .LE. R) THEN
        CALL RIDERS(ILEVL, ITMIL, IANIM, IAMMU, IMISC, IFINJ)
      END IF
      IEVTC = 0
      R = 100 * RAND()
   20 CONTINUE
      IEVTC = IEVTC + 1
      IF (IEVTC .LT. 16 .AND. R .GT. IEVNTY(IEVTC)) GOTO 20
      IF (IEVTC .EQ. 1) THEN
        PRINT 170
        ITMIL = ITMIL - 15 - INT(5 * RAND())
        IMISC = IMISC - 8
      ELSE IF (IEVTC .EQ. 2) THEN
        PRINT 180
        ITMIL = ITMIL - 25
        IANIM = IANIM - 20
      ELSE IF (IEVTC .EQ. 3) THEN
        PRINT 190
        ITMIL = ITMIL - 5 - INT(4 * RAND())
        IMISC = IMISC - 2 - INT(3 * RAND())
      ELSE IF (IEVTC .EQ. 4) THEN
        PRINT 200
        ITMIL = ITMIL - 17
      ELSE IF (IEVTC .EQ. 5) THEN
        PRINT 210
        ITMIL = ITMIL - 10
      ELSE IF (IEVTC .EQ. 6) THEN
        PRINT 220
        ITMIL = ITMIL - INT(10 * RAND()) - 2
      ELSE IF (IEVTC .EQ. 7) THEN
        IF (ITMIL .GT. 950) THEN
          PRINT 230
          IF (ICLTH .GT. 22 + 4 * RAND()) THEN
            PRINT 240
          ELSE
            PRINT 250
            CALL SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
          END IF
        ELSE
          PRINT 260
          IFOOD = IFOOD - 10
          IAMMU = IAMMU - 500
          IMISC = IMISC - 15
          ITMIL = ITMIL - INT(10 * RAND()) - 5
        END IF
      ELSE IF (IEVTC .EQ. 8) THEN
        PRINT 270
        IBANGT = SHOOT(ILEVL)
        IAMMU = IAMMU - 20 * IBANGT
        IF (IAMMU .LT. 0) THEN
          PRINT 280
          ICASH = ICASH / 3
        END IF
        IF (IAMMU .LT. 0 .OR. IBANGT .GT. 1) THEN
          PRINT 290
          IFINJ = 1
          IMISC = IMISC - 5
          IANIM = IANIM - 20
        ELSE IF (IBANGT .LE. 1) THEN
          PRINT 300
        END IF
      ELSE IF (IEVTC .EQ. 9) THEN
        PRINT 310
        IFOOD = IFOOD - 40
        IAMMU = IAMMU - 400
        IMISC = IMISC - INT(RAND() * 8) - 3
        ITMIL = ITMIL - 15
      ELSE IF (IEVTC .EQ. 10) THEN
        PRINT 320
        ITMIL = ITMIL - 10 - INT(5 * RAND())
      ELSE IF (IEVTC .EQ. 11) THEN
        PRINT 330
        IAMMU = IAMMU - 10
        IMISC = IMISC - 5
        IF (IMISC .LT. 0) THEN
          PRINT 340
          CALL DIE()
        END IF
      ELSE IF (IEVTC .EQ. 12) THEN
        PRINT 350
        IFOOD = IFOOD - 30
        ICLTH = ICLTH - 20
        ITMIL = ITMIL - 20 - INT(20 * RAND())
      ELSE IF (IEVTC .EQ. 13) THEN
        PRINT 360
        IF (IAMMU .LE. 39) THEN
          PRINT 370
          CALL DIE()
        END IF
        IBANGT = SHOOT(ILEVL)
        IF (IBANGT .GT. 2) THEN
          PRINT 380
        ELSE
          PRINT 390
        END IF
        IAMMU = IAMMU - 20 * IBANGT
        ICLTH = ICLTH - IBANGT * 4
        IFOOD = IFOOD - IBANGT * 8
      ELSE IF (IEVTC .EQ. 14) THEN
        PRINT 400
        ITMIL = ITMIL - 5 - INT(RAND() * 3)
        IAMMU = IAMMU - 200
        IMISC = IMISC - 4 - INT(RAND() * 3)
      ELSE IF (IEVTC .EQ. 15) THEN
        IF ((IEATS .EQ. 1) .OR.
     &      (IEATS .EQ. 2 .AND. RAND() .GT. 0.25) .OR.
     &      (IEATS .EQ. 3 .AND. RAND() .LT. 0.5)) THEN
          CALL SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
        END IF
      ELSE IF (IEVTC .EQ. 16) THEN
        PRINT 410
        IFOOD = IFOOD + 14
      END IF
      IF (ITMIL .GT. 950) THEN
        R = 9 - ((ITMIL/100 - 15)**2 / ((ITMIL/100 - 15)**2 + 12))
        IF (RAND() * 10 .LE. R) THEN
          PRINT 420
          IF (RAND() .LE. 0.1) THEN
            PRINT 430
            ITMIL = ITMIL - 60
          ELSE IF (RAND() .LE. 0.11) THEN
            PRINT 440
            IMISC = IMISC - 5
            IAMMU = IAMMU - 200
            ITMIL = ITMIL - 20 - INT(30 * RAND())
          ELSE
            PRINT 450
            ITMIL = ITMIL - 45 - INT(RAND() / 0.02)
          END IF
        END IF
        IF (IFPAS .NE. 1) THEN
          IFPAS = 1
          IF950 = 1
          IF (RAND() .GE. 0.8) THEN
            PRINT 460
          ELSE
            CALL BLIZZ(ITMIL, IAMMU, ICLTH, IFOOD,
     &                 IMISC, IFILL, IFINJ, IEATS)
          END IF
        END IF
      END IF
      IF (ITMIL .GE. 1700 .AND. IFMOU .NE. 1) THEN
        IFMOU = 1
        IF (RAND() .GE. 0.7) THEN
          CALL BLIZZ(ITMIL, IAMMU, ICLTH, IFOOD,
     &               IMISC, IFILL, IFINJ, IEATS)
        END IF
      END IF
      IFFRT = -1 * IFFRT
   10 CONTINUE
      PRINT 470
      CALL DIE()

  100 FORMAT (/,1X,'MONDAY,',1X,A,/,1X,60('-'))
  110 FORMAT (1X,'TOTAL MILEAGE IS',1X,I4)
  120 FORMAT (1X,'YOU''D BETTER DO SOME HUNTING OR BUY FOOD AND SOON!!')
  130 FORMAT (/,1X,'FOOD BULLETS CLOTHING MISC.SUPP. CASH',/,
     &1X,I4,1X,I7,1X,I8,1X,I10,1X,I4,/)
  140 FORMAT (1X,'DO YOU WANT TO',/,
     &1X,'(1) STOP AT THE NEXT FORT, (2) HUNT, (3) CONTINUE')
  150 FORMAT (1X,'DO YOU WANT TO',/,1X,'(1) HUNT, (2) CONTINUE')
  160 FORMAT (1X,'YOU RAN OUT OF FOOD AND STARVED TO DEATH.')
  170 FORMAT (1X,'WAGON BREAKS DOWN -- LOSE TIME AND SUPPLIES FIXING ',
     &'IT.')
  180 FORMAT (1X,'OX INJURES LEG -- SLOWS YOU DOWN REST OF TRIP.')
  190 FORMAT (1X,'BAD LUCK -- YOUR DAUGHTER BROKE HER ARM.',/,
     &1X,'YOU HAD TO STOP AND USE SUPPLIES TO MAKE A SLING.')
  200 FORMAT (1X,'OX WANDERS OFF -- SPEND TIME LOOKING FOR IT.')
  210 FORMAT (1X,'YOUR SON GETS LOST -- SPEND HALF THE DAY LOOKING ',
     &'FOR HIM.')
  220 FORMAT (1X,'UNSAFE WATER -- LOSE TIME LOOKING FOR CLEAN SPRING.')
  230 FORMAT (1X,'COLD WEATHER -- BRRRRRRR!')
  240 FORMAT (1X,'YOU HAVE ENOUGH CLOTHING TO KEEP YOU WARM.')
  250 FORMAT (1X,'YOU DON''T HAVE ENOUGH CLOTHING TO KEEP YOU WARM.')
  260 FORMAT (1X,'HEAVY RAINS -- TIME AND SUPPLIES LOST.')
  270 FORMAT (1X,'BANDITS ATTACK.')
  280 FORMAT (1X,'YOU RAN OUT OF BULLETS -- THEY GET LOTS OF CASH.')
  290 FORMAT (1X,'YOU GOT SHOT IN THE LEG AND THEY TOOK ONE OF YOUR ',
     &'OXEN.',/,1X,'BETTER HAVE A DOC LOOK AT YOUR WOUND.')
  300 FORMAT (1X,'QUICKEST DRAW OUTSIDE OF DODGE CITY!!',/,
     &1X,'YOU GOT ''EM!')
  310 FORMAT (1X,'THERE WAS A FIRE IN YOUR WAGON -- FOOD AND SUPPLIES ',
     &'DAMAGE!')
  320 FORMAT (1X,'LOSE YOUR WAY IN HEAVY FOG -- TIME IS LOST.')
  330 FORMAT (1X,'YOU KILLED A POISONOUS SNAKE AFTER IT BIT YOU.')
  340 FORMAT (1X,'YOU DIE OF SNAKEBITE SINCE YOU HAVE NO MEDICINE.')
  350 FORMAT (1X,'WAGON GETS SWAMPED FORDING RIVER -- LOSE FOOD AND ',
     &'CLOTHES.')
  360 FORMAT (1X,'WILD ANIMALS ATTACK!!')
  370 FORMAT (1X,'YOU WERE TOO LOW ON BULLETS -- THE WOLVES ',
     &'OVERPOWERED YOU.',/,1X,'YOU DIED OF INJURIES.')
  380 FORMAT (1X,'SLOW ON THE DRAW -- THEY GOT AT YOUR FOOD AND ',
     &'CLOTHES.')
  390 FORMAT (1X,'NICE SHOOTIN'' PARDNER -- THEY DIDN''T GET MUCH.')
  400 FORMAT (1X,'HAIL STORM -- SUPPLIES DAMAGED.')
  410 FORMAT (1X,'HELPFUL INDIANS SHOW YOU WERE TO FIND MORE FOOD.')
  420 FORMAT (1X,'RUGGED MOUNTAINS.')
  430 FORMAT (1X,'YOU GOT LOST -- LOSE VALUABLE TIME TRYING TO ',
     &'FIND TRAIL!')
  440 FORMAT (1X,'WAGON DAMAGED -- LOSE TIME AND SUPPLIES.')
  450 FORMAT (1X,'THE GOING GETS SLOW.')
  460 FORMAT (1X,'YOU MADE IT SAFELY THROUGH SOUTH PASS -- NO SNOW.')
  470 FORMAT (1X,'YOU HAVE BEEN ON THE TRAIL TOO LONG ...',/,
     &1X,'YOUR FAMILY DIES IN THE FIRST BLIZZARD OF WINTER.')
      END
C     ******************************************************************
      SUBROUTINE RIDERS(ILEVL, ITMIL, IANIM, IAMMU, IMISC, IFINJ)
C
C     RIDERS ATTACK (OR NOT).
C
      INTEGER INPUT, SHOOT
      REAL    RAND
      INTEGER ILEVL, ITMIL, IANIM, IAMMU, IMISC, IFINJ
      INTEGER IBANGT, IHORF, ISELEC

      IHORF = 0
      IF (RAND() .LT. 0.8) THEN
        PRINT 100
      ELSE
        PRINT 110
        IHORF = 1
      END IF
      PRINT 120
      IF (RAND() .LE. 0.2) IHORF = 1 - IHORF
      ISELEC = INPUT(1, 4)
      IF (ISELEC .EQ. 1) THEN
        IF (IHORF .EQ. 1) THEN
          ITMIL = ITMIL + 15
          IANIM = IANIM - 10
        ELSE
          ITMIL = ITMIL + 20
          IMISC = IMISC - 15
          IAMMU = IAMMU - 150
          IANIM = IANIM - 40
        END IF
      ELSE IF (ISELEC .EQ. 2 .OR. ISELEC .EQ. 4) THEN
        IBANGT = SHOOT(ILEVL)
        IF (ISELEC .EQ. 2) THEN
          IAMMU = IAMMU - IBANGT * 40 - 80
        ELSE
          IAMMU = IAMMU - IBANGT * 30 - 80
          ITMIL = ITMIL - 25
        END IF
        IF (IBANGT .LE. 1) THEN
          PRINT 130
        ELSE IF (IBANGT .GT. 1 .AND. IBANGT .LE. 4) THEN
          PRINT 140
        ELSE IF (IBANGT .GT. 5) THEN
          PRINT 150
          IFINJ = 1
        END IF
      ELSE IF (ISELEC .EQ. 3) THEN
        IF (RAND() .GT. 0.8) THEN
          PRINT 160
          RETURN
        END IF
        IAMMU = IAMMU - 150
        IMISC = IMISC - 15
      END IF
      IF (IHORF .EQ. 0) THEN
        PRINT 170
        IF (IAMMU .LT. 0) THEN
          PRINT 180
          CALL DIE()
        END IF
      ELSE
        PRINT 190
      END IF

  100 FORMAT (1X,'RIDERS AHEAD. THEY LOOK HOSTILE.')
  110 FORMAT (1X,'RIDERS AHEAD. THEY DON''T LOOK HOSTILE.')
  120 FORMAT (1X,'TACTICS',/,1X,'(1) RUN, (2) ATTACK, (3) CONTINUE, ',
     &'(4) CIRCLE WAGONS')
  130 FORMAT (1X,'NICE SHOOTING -- YOU DROVE THEM OFF.')
  140 FORMAT (1X,'KINDA SLOW WITH YOUR COLT .45.')
  150 FORMAT (1X,'LOUSY SHOT -- YOU GOT KNIFED.',/,
     &1X,'YOU HAVE TO SEE OL'' DOC BLANCHARD.')
  160 FORMAT (1X,'THEY DID NOT ATTACK.')
  170 FORMAT (1X,'RIDERS WERE HOSTILE -- CHECK FOR LOSSES.')
  180 FORMAT (1X,'YOU RAN OUT OF BULLETS AND GOT MASSACRED BY THE ',
     &'RIDERS.')
  190 FORMAT (1X,'RIDERS WERE FRIENDLY, BUT CHECK FOR POSSIBLE LOSSES.')
      END
C     ******************************************************************
      SUBROUTINE SHOP(ICASH, IANIM, IFOOD, IAMMU, ICLTH, IMISC)
C
C     SHOP VISIT IN MISSOURI. THE PLAYER HAS TO BUY AT LEAST OXEN FOR
C     $200 TO $300.
C
      INTEGER ICASH, IANIM, IFOOD, IAMMU, ICLTH, IMISC
      INTEGER INPUT

      PRINT 100, ICASH
      IANIM = INPUT(200, 300)
      ICASH = ICASH - IANIM
      PRINT 200, ICASH
      IFOOD = INPUT(0, ICASH)
      ICASH = ICASH - IFOOD
      PRINT 300, ICASH
      IAMMU = INPUT(0, ICASH)
      ICASH = ICASH - IAMMU
      IAMMU = IAMMU * 50
      PRINT 400, ICASH
      ICLTH = INPUT(0, ICASH)
      ICASH = ICASH - ICLTH
      PRINT 500, ICASH
      IMISC = INPUT(0, ICASH)
      ICASH = ICASH - IMISC
      PRINT 600, ICASH

  100 FORMAT (1X,'YOU HAVE ',I3,' DOLLARS LEFT.',/,
     &1X,'HOW MUCH DO YOU WANT TO SPEND ON YOUR OXEN TEAM?')
  200 FORMAT (1X,'YOU NOW HAVE ',I3,' DOLLARS LEFT.',/,
     &1X,'HOW MUCH DO YOU WANT TO SPEND ON FOOD?')
  300 FORMAT (1X,'YOU NOW HAVE ',I3,' DOLLARS LEFT.',/,
     &1X,'HOW MUCH DO YOU WANT TO SPEND ON AMMUNITION?')
  400 FORMAT (1X,'YOU NOW HAVE ',I3,' DOLLARS LEFT.',/,
     &1X,'HOW MUCH DO YOU WANT TO SPEND ON CLOTHING?')
  500 FORMAT (1X,'YOU NOW HAVE ',I3,' DOLLARS LEFT.',/,
     &1X,'HOW MUCH DO YOU WANT TO SPEND ON MISCELLANEOUS SUPPLIES?')
  600 FORMAT (1X,'AFTER ALL YOUR PURCHASES, YOU NOW HAVE ',I3,
     &' DOLLARS LEFT.')
      END
C     ******************************************************************
      SUBROUTINE SICK(IEATS, ITMIL, IMISC, IFILL, IFINJ)
C
C     ILLNESS EVENTS.
C
      REAL    RAND
      INTEGER IEATS, ITMIL, IMISC, IFILL, IFINJ

      IF (100 * RAND() .LT. 10 + 35 * (IEATS - 1)) THEN
        PRINT 100
        ITMIL = ITMIL - 5
        IMISC = IMISC - 2
      ELSE IF (100 * RAND() .LT. 100 - (40 / 4**(IEATS - 1))) THEN
        PRINT 200
        ITMIL = ITMIL - 5
        IMISC = IMISC - 5
      ELSE
        PRINT 300
        ITMIL = ITMIL - 10
        IFILL = 1
      END IF
      IF (IMISC .LT. 0) THEN
        PRINT 400
        IF (IFINJ .EQ. 1) THEN
          PRINT 500
        ELSE
          PRINT 600
        END IF
        CALL DIE()
      END IF

  100 FORMAT (1X,'MILD ILLNESS -- MEDICINE USED.')
  200 FORMAT (1X,'BAD ILLNESS -- MEDICINE USED.')
  300 FORMAT (1X,'SERIOUS ILLNESS -- YOU MUST STOP FOR MEDICAL ',
     &'ATTENTION.')
  400 FORMAT (1X,'YOU RAN OUT OF MEDICAL SUPPLIES.')
  500 FORMAT (1X,'YOU DIED OF INJURIES.')
  600 FORMAT (1X,'YOU DIED OF PNEUMONIA.')
      END
C     ******************************************************************
      SUBROUTINE SRAND(ISEED)
C
C     THIS SUBROUTINE SETS THE INTEGER SEED TO BE USED WITH THE
C     COMPANION RAND FUNCTION TO THE VALUE OF ISEED. A FLAG IS SET TO
C     INDICATE THAT THE SEQUENCE OF PSEUDO-RANDOM NUMBERS FOR THE
C     SPECIFIED SEED SHOULD START FROM THE BEGINNING.
C
      INTEGER ISEED
      INTEGER JSEED, IFRST
      COMMON /SEED/ JSEED, IFRST

      JSEED = ISEED
      IFRST = 0
      END
C     ******************************************************************
      SUBROUTINE UPPER(STR)
C
C     CONVERTS A STRING TO UPPER CASE.
C
      INTEGER IA, IZ
      PARAMETER (IA=97, IZ=122)
      CHARACTER*(*) STR
      INTEGER       I, J

      DO 10 I = 1, LEN(STR)
      J = ICHAR(STR(I:I))
      IF (J .GE. IA .AND. J .LE. IZ) STR(I:I) = CHAR(J - 32)
   10 CONTINUE
      END
C     ******************************************************************
      BLOCK DATA
C
C     COMMON VARIABLES:
C
C     JSEED - PRNG SEED VALUE.
C     IFRST - FIRST RANDOM VALUE.
C     EVNTS - ARRAY OF EVENT PROBABILITIES.
C     DATES - ARRAY OF DATE STRINGS.
C     WDAYS - ARRAY OF WEEKDAY STRINGS.
C     IAMMU - AMOUNT SPENT ON AMMUNITION.
C     IANIM - AMOUNT SPENT ON ANIMALS.
C     ICLTH - AMOUNT SPENT ON CLOTHING.
C     IFOOD - AMOUNT SPENT ON FOOD.
C     IMISC - AMOUNT SPENT ON MISCELLANEAOUS SUPPLIES.
C     IEVTC - COUNTER IN GENERATING EVENTS.
C     ICTRN - TURN NUMBER FOR SETTING DATE.
C     ILEVL - CHOICE OF SHOOTING EXPERTISE LEVEL.
C     IEATS - CHOICE OF EATING.
C     IF950 - FLAG FOR CLEARING SOUTH PASS IN SETTING MILEAGE.
C     IFPAS - FLAG FOR CLEARING SOUTH PASS.
C     IFMOU - FLAG FOR CLEARING BLUE MOUNTAINS.
C     IFFRT - FLAG FOR FORT OPTION.
C     IFILL - FLAG FOR ILLNESS.
C     IFINJ - FLAG FOR INJURY.
C     ITMIL - TOTAL MILEAGE WHOLE TRIP.
C     ILMIL - TOTAL MILEAGE UP THROUGH PREVIOUS TURN.
C     ICASH - CASH LEFT AFTER INITIAL PURCHASE.
C
      IMPLICIT INTEGER (I-N)
      CHARACTER*17 ADATEY(20)
      CHARACTER*10 AWEEKY(7)
      INTEGER      IEVNTY(15)
      COMMON /SEED/  JSEED, IFRST
      COMMON /EVNTS/ IEVNTY
      COMMON /DATES/ ADATEY
      COMMON /WDAYS/ AWEEKY
      COMMON /STATE/ IAMMU, IANIM, ICLTH, IFOOD, IMISC, IEVTC, ICTRN,
     &               ILEVL, IEATS, IF950, IFPAS, IFMOU, IFFRT, IFILL,
     &               IFINJ, ITMIL, ILMIL, ICASH
      DATA JSEED, IFRST /123456789,0/
      DATA ICTRN, ITMIL /0,0/
      DATA IEVNTY /6,11,13,15,17,22,32,35,37,42,44,54,64,69,95/
      DATA IFFRT, IFILL, IFINJ, IF950, IFPAS, IFMOU /-1,0,0,0,0,0/
      DATA ICASH  /700/
      DATA AWEEKY /'MONDAY,','TUESDAY,','WEDNESDAY,','THURSDAY,',
     &'FRIDAY,','SATURDAY,','SUNDAY,'/
      DATA ADATEY /'MARCH 29 1847','APRIL 12 1847','APRIL 26 1847',
     &'MAY 10 1847','MAY 24 1847','JUNE 7 1847','JUNE 21 1847',
     &'JULY 5 1847','JULY 19 1847','AUGUST 2 1847','AUGUST 16 1847',
     &'AUGUST 31 1847','SEPTEMBER 13 1847','SEPTEMBER 27 1847',
     &'OCTOBER 11 1847','OCTOBER 25 1847','NOVEMBER 8 1847',
     &'NOVEMBER 22 1847','DECEMBER 6 1847','DECEMBER 20 1847'/
      END