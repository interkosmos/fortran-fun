# Fortran Fun

A collection of text-based computer games and other small programs written in
Fortran. See
[FORTRAN Computer Games](https://cyber.dabamos.de/programming/fortran/computer-games/)
for further descriptions.

## Overview

| Directory         | Language | Description                                  |
|-------------------|----------|----------------------------------------------|
| `src/acey/`       | F2018    | Acey Ducey card game.                        |
| `src/acey77/`     | F77      | Acey Ducey in FORTRAN 77.                    |
| `src/amazing/`    | F2018    | Maze generator.                              |
| `src/buzzword/`   | F2018    | Phrases generator.                           |
| `src/delmar/`     | F77      | [Horse race simulation](https://cyber.dabamos.de/programming/fortran/computer-games/delmar.html) ported from FORTRAN IV. |
| `src/donut/`      | F77      | Spinning ASCII donut.                        |
| `src/french/`     | F77      | French variant of hangman game.              |
| `src/hamurabi/`   | F2018    | Ancient city management game.                |
| `src/hamurabi77/` | F77      | FORTRAN 77 version of [Hamurabi](http://cyber.dabamos.de/programming/fortran/computer-games/hamurabi.html). |
| `src/guess/`      | F77      | Guess a number game.                         |
| `src/jackpot/`    | F77      | Slot machine.                                |
| `src/magic8/`     | F77      | Fortune telling.                             |
| `src/oregon/`     | F77      | [Oregon Trail](https://cyber.dabamos.de/programming/fortran/oregon/) in FORTRAN 77. |
| `src/penney/`     | F77      | Source port of Penney’s Game.                |
| `src/pi/`         | F77      | Spigot algorithm for the digits of Pi.       |
| `src/rock/`       | F2018    | Rock, Paper, Scissors against the computer.  |
| `src/rock77/`     | F77      | FORTRAN 77 version of Rock, Paper, Scissors. |
| `src/rot13/`      | F77      | Basic Rot-13 encoder/decoder.                |
| `src/russian/`    | F2018    | Virtual revolver.                            |
| `src/russian77/`  | F77      | Virtual revolver in FORTRAN 77.              |
| `src/wumpus/`     | F77      | [Hunt the Wumpus](https://cyber.dabamos.de/programming/fortran/computer-games/wumpus.html). |

## Build Instructions

Just execute the Makefile:

```
$ make
```

Overwrite the argument `FC` to switch the Fortran compiler:

```
$ make FC=ifx
```

## Licence

ISC
