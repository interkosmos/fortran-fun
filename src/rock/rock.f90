! rock.f90
program main
    !! Rock, Scissors, Paper
    !!
    !! Remember the game of rock-scissors-paper. You and your opponent make a
    !! motion three times with your fists and then either show:
    !!
    !! * a flat hand (paper),
    !! * fist (rock),
    !! * two fingers (scissors).
    !!
    !! Depending upon what is shown, the game is a tie (both show the same) or
    !! one person wins. Paper wraps up rock, so it wins. Scissors cut paper, so
    !! they win. And rock breaks scissors, so it wins.
    !!
    !! In this computerized version of rock-scissors-paper, you can play up to
    !! ten games vs. the computer.
    !!
    !! Charles Lund wrote this game while at the American School in The Hague,
    !! Netherlands. Converted to Fortran 2018 by Philipp Engel.
    implicit none (type, external)

    integer,          parameter :: RULES(3) = [ 2, 3, 1 ]
    character(len=*), parameter :: NAMES(3) = [ &
        character(len=8) :: 'Rock', 'Scissors', 'Paper' ]

    integer :: computer, player
    integer :: ncomputer, nplayer
    integer :: ngames
    integer :: i, stat
    real    :: r

    call random_init(.false., .false.)

    print '(20x, "Game of Rock, Scissors, Paper")'
    print '(25x, "Creative Computing")'
    print '(23x, "Morristown, New Jersey")'

    do
        print '(/, "How many games?")'
        read (*, *, iostat=stat) ngames

        if (stat /= 0 .or. ngames < 1) then
            print '("Invalid input.")'
            cycle
        end if

        if (ngames > 10) then
            print '("Sorry, but we aren''t allowed to play that many.")'
            cycle
        end if

        exit
    end do

    ncomputer = 0
    nplayer = 0

    do i = 1, ngames
        print '(/,"Game number: ", i0)', i
        print '("1) Rock 2) Scissors 3) Paper")'
        print '("What is your choice?")'

        ! Read player's choice.
        do
            read (*, *, iostat=stat) player

            if ((player - 1) * (player - 2) * (player - 3) /= 0) then
                print '("Invalid input.")'
                cycle
            end if

            exit
        end do

        ! Let the computer choose.
        call random_number(r)
        computer = 1 + int(r * 3)

        print '("You have chosen:         ", a)', NAMES(player)
        print '("The computer has chosen: ", a)', NAMES(computer)

        ! Determine the winner.
        if (player == computer) then
            print '("Tie game. No winner.")'
        else if (RULES(player) == computer) then
            print '("You win!")'
            nplayer = nplayer + 1
        else
            print '("The computer wins!")'
            ncomputer = ncomputer + 1
        end if
    end do

    print '(/, "Here is the final game score:")'
    print '("The computer has won ", i0, " game(s),")', ncomputer
    print '("you have won ", i0, " game(s),")', nplayer
    print '("and ", i0, " game(s) ended in a tie.")', ngames - (ncomputer + nplayer)
    print '(/, "Thanks for playing!")'
end program main
