! russian.f90
program main
    !! Russian Roulette
    !!
    !! In this game, you are given by the computer a revolver loaded with one
    !! bullet and five empty chambers. You spin the chamber and pull the trigger
    !! by inputting a "1", or, if you want to quit, input a "2". You win if you
    !! play ten times and are still alive.
    !!
    !! Tom Adametx wrote this program while a student at Curtis Jr. High School
    !! in Sudbury, Massachusetts. Converted to Fortran 2018 by Philipp Engel.
    implicit none (type, external)

    integer :: n, input, stat
    real    :: r

    call random_init(.false., .false.)

    print '(26x, "Russian Roulette")'
    print '(25x, "Creative Computing")'
    print '(23x, "Morristown, New Jersey", /)'
    print '("This is a game of Russian Roulette. Here is a revolver.")'

    main_loop: do
        print '(/, "Type ""1"" to spin the chamber and pull the trigger.")'
        print '("Type ""2"" to give up.")'
        print '("Go!")'

        n = 0

        player_loop: do
            input = 0
            read (*, *, iostat=stat) input

            if (stat /= 0 .or. (input /= 1 .and. input /= 2)) then
                print '("Invalid input. Try again.")'
                cycle player_loop
            end if

            select case (input)
                case (1)
                    n = n + 1
                    call random_number(r)

                    ! Uhh-ohh ...
                    if (r <= 1.0 / 6) then
                        print '("Bang!! You are dead!")'
                        print '("Condolences will be sent to your relatives.")'
                        print '(/, "... next victim ...")'
                        cycle main_loop
                    end if

                    ! Player has won.
                    if (n > 10) then
                        print '("You win!")'
                        print '("Let someone else blow his brains out.")'
                        cycle main_loop
                    end if

                    ! Player got lucky.
                    print '("- CLICK -")'
                    cycle player_loop

                case (2)
                    print '("Chicken!!")'
                    print '(/, "... next victim ...")'
                    cycle main_loop
            end select
        end do player_loop
    end do main_loop
end program main
