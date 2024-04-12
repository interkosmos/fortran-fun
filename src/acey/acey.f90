! acey.f90
program main
    !! Acey Ducey
    !!
    !! This is a simulation of the Acey Ducey card game. In the game, the dealer
    !! (the computer) deals two cards face up. You have an option to bet or not
    !! to bet depending on whether or not you feel the next card dealt will have
    !! a value between the first two.
    !!
    !! Your initial money is set to $100. The game keeps going on until you lose
    !! all your money or interrupt the program.
    !!
    !! The original BASIC program author was Bill Palmby of Prairie View,
    !! Illinois. Converted to Fortran 2018 by Philipp Engel.
    implicit none (type, external)

    ! Alter the following statement to start with more or less than $100:
    integer,          parameter :: INITIAL_BANKROLL = 100
    character(len=*), parameter :: CARD_NAMES(13)   = [ character(len=5) :: &
        ' 2', ' 3', ' 4', ' 5', ' 6', ' 7', ' 8', ' 9', '10', &
        'Jack', 'Queen', 'King', 'Ace' ]

    character(len=3) :: input
    integer          :: bankroll, dealer1, dealer2, player
    integer          :: bet, stat, swap
    logical          :: done, valid

    call random_init(.false., .false.)

    print '(/, 11x, "Acey Ducey Card Game")'
    print '(3x, "Creative Computing  Morristown, New Jersey", /)'
    print '("Acey-Ducey is played in the following manner.")'
    print '("The dealer (computer) deals two cards face up.")'
    print '("You have an option to bet or not bet depending")'
    print '("on whether or not you feel the card will have")'
    print '("a value between the first two.")'
    print '("If you do not want to bet, bet a 0.")'

    done = .false.

    do while (.not. done)
        bankroll = INITIAL_BANKROLL
        print '(/, "You now have $", i0, ".")', bankroll

        do while (bankroll > 0)
            print '("Here are your next two cards:")'
            dealer1 = deal_card()
            dealer2 = dealer1

            ! If the cards match, redeal 2nd card until they don't.
            do while (dealer1 == dealer2)
                dealer2 = deal_card()
            end do

            ! Reorder cards by value.
            if (dealer1 > dealer2) then
                swap    = dealer1
                dealer1 = dealer2
                dealer2 = swap
            end if

            ! Show dealer cards to player.
            print '(a)', CARD_NAMES(dealer1)
            print '(a)', CARD_NAMES(dealer2)

            valid = .false.

            do while (.not. valid)
                print '(/, "What is your bet?")'

                read (*, *, iostat=stat) bet

                if (stat /= 0) then
                    print '("Invalid input.")'
                    cycle
                end if

                if (bet <= 0) then
                    valid = .true.
                    print '("Chicken!!", /)'
                else if (bet > bankroll) then
                    print '("Sorry, my friend but you bet too much.")'
                    print '("Your have only $", i0, " to bet.")', bankroll
                else
                    valid = .true.
                    player = deal_card()
                    print '("Card: ", a)', CARD_NAMES(player)

                    if (dealer1 < player .and. player < dealer2) then
                        print '("You win!!")'
                        bankroll = bankroll + bet
                    else
                        print '("Sorry, you lose.")'
                        bankroll = bankroll - bet
                    end if

                    if (bankroll > 0) &
                        print '(/, "You now have $", i0, ".")', bankroll
                end if
            end do
        end do

        print '(/, "Sorry, my friend but you blew your wad.")'
        print '("Try again? (yes or no)")'

        read (*, *) input

        if (input(1:1) /= 'Y' .and. input(1:1) /= 'y') &
            done = .true.
    end do

    print '("OK. Hope you had fun.")'
contains
    integer function deal_card() result(card)
        !! Returns random integer in range [1, 13].
        real :: r

        call random_number(r)
        card = 1 + nint(r * 12)
    end function deal_card
end program main
