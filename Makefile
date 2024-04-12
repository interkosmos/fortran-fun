.POSIX:

FC     = gfortran
FFLAGS = -O2

ACEY       = src/acey/acey.f90
ACEY77     = src/acey77/acey.f
AMAZING    = src/amazing/amazing.f90
BUZZWORD   = src/buzzword/buzzword.f90
DELMAR     = src/delmar/delmar.f
DONUT      = src/donut/donut.f
FRENCH     = src/french/french.f
HAMURABI   = src/hamurabi/hamurabi.f90
HAMURABI77 = src/hamurabi77/hamurabi.f
GUESS      = src/guess/guess.f
JACKPOT    = src/jackpot/jackpot.f
MAGIC8     = src/magic8/magic8.f
OREGON     = src/oregon/oregon.f
PENNEY     = src/penney/penney.f
PI         = src/pi/pi.f
ROCK       = src/rock/rock.f90
ROCK77     = src/rock77/rock.f
ROT13      = src/rot13/rot13.f
RUSSIAN    = src/russian/russian.f90
RUSSIAN77  = src/russian77/russian.f
WUMPUS     = src/wumpus/wumpus.f

.PHONY: all clean

all: acey acey77 amazing buzzword delmar donut french hamurabi hamurabi77 \
     guess jackpot magic8 oregon penney pi rock rock77 rot13 russian russian77 \
     wumpus

acey: $(ACEY)
	$(FC) $(FFLAGS) -o acey $(ACEY)

acey77: $(ACEY77)
	$(FC) $(FFLAGS) -o acey77 $(ACEY77)

amazing: $(AMAZING)
	$(FC) $(FFLAGS) -o amazing $(AMAZING)

buzzword: $(BUZZWORD)
	$(FC) $(FFLAGS) -o buzzword $(BUZZWORD)

delmar: $(DELMAR)
	$(FC) $(FFLAGS) -o delmar $(DELMAR)

donut: $(DONUT)
	$(FC) $(FFLAGS) -o donut $(DONUT)

french: $(FRENCH)
	$(FC) $(FFLAGS) -o french $(FRENCH)

hamurabi: $(HAMURABI)
	$(FC) $(FFLAGS) -o hamurabi $(HAMURABI)

hamurabi77: $(HAMURABI77)
	$(FC) $(FFLAGS) -o hamurabi77 $(HAMURABI77)

guess: $(GUESS)
	$(FC) $(FFLAGS) -o guess $(GUESS)

jackpot: $(JACKPOT)
	$(FC) $(FFLAGS) -o jackpot $(JACKPOT)

magic8: $(MAGIC8)
	$(FC) $(FFLAGS) -o magic8 $(MAGIC8)

oregon: $(OREGON)
	$(FC) $(FFLAGS) -o oregon $(OREGON)

penney: $(PENNEY)
	$(FC) $(FFLAGS) -o penney $(PENNEY)

pi: $(PI)
	$(FC) $(FFLAGS) -o pi $(PI)

rock: $(ROCK)
	$(FC) $(FFLAGS) -o rock $(ROCK)

rock77: $(ROCK77)
	$(FC) $(FFLAGS) -o rock77 $(ROCK77)

rot13: $(ROT13)
	$(FC) $(FFLAGS) -o rot13 $(ROT13)

russian: $(RUSSIAN)
	$(FC) $(FFLAGS) -o russian $(RUSSIAN)

russian77: $(RUSSIAN77)
	$(FC) $(FFLAGS) -o russian77 $(RUSSIAN77)

wumpus: $(WUMPUS)
	$(FC) $(FFLAGS) -o wumpus $(WUMPUS)

clean:
	if [ -e acey ];       then rm acey; fi
	if [ -e acey77 ];     then rm acey77; fi
	if [ -e amazing ];    then rm amazing; fi
	if [ -e buzzword ];   then rm buzzword; fi
	if [ -e delmar ];     then rm delmar; fi
	if [ -e donut ];      then rm donut; fi
	if [ -e french ];     then rm french; fi
	if [ -e hamurabi ];   then rm hamurabi; fi
	if [ -e hamurabi77 ]; then rm hamurabi77; fi
	if [ -e guess ];      then rm guess; fi
	if [ -e jackpot ];    then rm jackpot; fi
	if [ -e magic8 ];     then rm magic8; fi
	if [ -e oregon ];     then rm oregon; fi
	if [ -e penney ];     then rm penney; fi
	if [ -e pi ];         then rm pi; fi
	if [ -e rock ];       then rm rock; fi
	if [ -e rock77 ];     then rm rock77; fi
	if [ -e rot13 ];      then rm rot13; fi
	if [ -e russian ];    then rm russian; fi
	if [ -e russian77 ];  then rm russian77; fi
	if [ -e wumpus ];     then rm wumpus; fi
