! hamurabi.f90
program main
    !! Hamurabi
    !!
    !! In this game you direct the administrator of Sumeria, Hammurabi, how to
    !! manage the city. The city initially has 1,000 acres, 100 people and 3,000
    !! bushels of grain in storage.
    !!
    !! You may buy and sell land with your neighboring city-states for bushels
    !! of grain -- the price will vary between 17 and 26 bushels per acre. You
    !! also must use grain to feed your people and as seed to plant the next
    !! year’s crop.
    !!
    !! You will quickly find that a certain number of people can only tend a
    !! certain amount of land and that people starve if they are not fed enough.
    !! You also have the unexpected to contend with such as a plague, rats
    !! destroying stored grain, and variable harvests.
    !!
    !! You will also find that managing just the few resources in this game is
    !! not a trivial job over a period of say ten years. The crisis of
    !! population density rears its head very rapidly.
    !!
    !! This program was originally written in Focal at DEC; author unknown.
    !! David Ahl converted it to BASIC and added the 10-year performance
    !! assessment. If you wish to change any of the factors, the extensive
    !! remarks in the program should make modification fairly straightforward.
    !! Converted to Fortran 2018 by Philipp Engel
    implicit none (type, external)

    integer, parameter :: NYEARS = 10 ! Number of years to play.

    integer :: acres      ! Acres owned by player.
    integer :: death      ! Number of people who died.
    integer :: harvest    ! Bushels harvested in one year.
    integer :: immigrants ! Immigration per year.
    integer :: percentage ! Percentage of people who died.
    integer :: population ! Size of the population.
    integer :: price      ! Current price of bushels.
    integer :: rats       ! Bushels eaten by rats.
    integer :: starved    ! Number of people starved to death in one year.
    integer :: storage    ! Number of bushels stored.
    integer :: year       ! The current year.
    integer :: yield      ! Yield of acres.
    logical :: plague     ! Flag for horrible plague.

    integer :: n, nfeed, nplant, quota
    real    :: r(5)

    call random_init(.false., .false.)

    ! Initialise the game state.
    year       = 0
    yield      = 3
    harvest    = 3000
    storage    = 2800
    population = 95
    immigrants = 5
    acres      = harvest / yield
    rats       = harvest - storage
    plague     = .false.

    print '(30x, "Hamurabi")'
    print '(26x, "Creative Computing")'
    print '(24x, "Morristown, New Jersey", /)'
    print '("Try your hand at governing ancient Sumeria for a ten-year")'
    print '("term of office.")'

    ! The main loop.
    do
        ! Get five random numbers.
        call random_number(r)

        year = year + 1
        population = population + immigrants
        print '(/, "Hamurabi: I beg to report to you,", /)'
        print '(10x, "in year ", i0, ", ", i0, " people starved, ", i0, " came to the city.")', &
            year, starved, immigrants

        ! A plague strikes! Half the population died.
        if (plague) then
            population = population / 2
            print '(/, 10x, "A horrible plague struck! Half the people died.", /)'
        end if

        print '(10x, "Population is now ", i0, ".")', population
        print '(10x, "The city now owns ", i0, " acres.")', acres
        print '(10x, "You harvested ", i0, " bushels per acre.")', yield
        print '(10x, "The rats ate ", i0, " bushels.")', rats
        print '(10x, "You now have ", i0, " bushels in store.", /)', storage

        ! Review performance and quit.
        if (year == NYEARS + 1) call review(acres, population, percentage, death)

        ! Roll new price per acre.
        price = int(10 * r(1)) + 17

        ! Ask the player to buy or sell land.
        print '("Land is trading at ", i0, " bushels per acre.")', price

        call trade(acres, storage, price)
        nfeed = feed(storage)
        nplant = plant(acres, population, storage)

        ! A bountiful harvest!
        yield = 1 + int(5 * r(2))
        harvest = nplant * yield
        rats = 0

        ! Rats are running wild.
        if (int(yield / 2.0) == yield / 2) then
            n = 1 + int(5 * r(3))
            rats = storage / n
        end if

        storage = storage - rats + harvest

        ! Let's have some babies. (Actually, it's immigration.)
        n = 1 + int(5 * r(4))
        immigrants = int(n * (20 * acres + storage) / population / 100 + 1)

        ! Horrors, a 15% chance of plague.
        plague = (int(10 * (2 * r(5) - 0.3)) == 1)
        quota = nfeed / 20

        ! Either a new year, or impeachment if too many people starved.
        if (population < quota) then
            starved = 0
        else
            starved = population - quota

            if (starved > 0.45 * population) then
                print '(/, "You starved ", i0, " people in one year!!")', starved
                call fink()
            end if

            percentage = ((year - 1) * percentage + starved * 100 / population) / year
            population = quota
            death = death + starved
        end if
    end do
contains
    integer function feed(storage)
        integer, intent(inout) :: storage

        integer :: stat

        do
            print '("How many bushels do you wish to feed your people?")'
            read (*, *, iostat=stat) feed

            if (stat /= 0 .or. feed < 0) call resign()

            if (feed > storage) then
                call no_bushels(storage)
                cycle
            end if

            storage = storage - feed
            exit
        end do
    end function feed

    integer function plant(acres, population, storage)
        integer, intent(in)    :: acres
        integer, intent(in)    :: population
        integer, intent(inout) :: storage

        integer :: stat

        do
            print '("How many acres do you wish to plant with seed?")'
            read (*, *, iostat=stat) plant

            if (stat /= 0) cycle
            if (plant == 0) return
            if (plant < 0) call resign()

            if (plant > acres) then
                call no_acres(acres)
                cycle
            end if

            if (int(plant / 2.0) > storage) then
                call no_bushels(storage)
                cycle
            end if

            if (plant >= 10 * population) then
                print '("But you have only ", i0, " people to tend the field! Now then,")', population
                cycle
            end if

            storage = storage - int(plant / 2.0)
            exit
        end do
    end function plant

    subroutine fink()
        print '("Due to this extreme mismanagement you have not only")'
        print '("been impeached and thrown out of office but you have")'
        print '("also been declared national fink!!")'
        call quit()
    end subroutine fink

    subroutine no_acres(acres)
        integer, intent(in) :: acres

        print '("Hamurabi: Think again. You own only ", i0, " acres. Now then,")', acres
    end subroutine no_acres

    subroutine no_bushels(storage)
        integer, intent(in) :: storage

        print '("Hamurabi: Think again. You have only ", i0, " bushels of grain. Now then,")', storage
    end subroutine no_bushels

    subroutine resign()
        print '("Hamurabi: I cannot do what you wish. Get yourself another steward!!")'
        call quit()
    end subroutine resign

    subroutine review(acres, population, percentage, death)
        integer, intent(in) :: acres
        integer, intent(in) :: population
        integer, intent(in) :: percentage
        integer, intent(in) :: death

        integer :: land, n
        real    :: r

        land = acres / population

        print '("In your 10-year term of office, ", i0, " percent")', percentage
        print '("of the population starved per year on the average,")'
        print '("i.e. a total of ", i0, " people died!!")', death
        print '("You started with 10 acres per person and ended")'
        print '("with ", i0, " acres per person.")', land

        if (percentage > 33 .or. land < 7) call fink()

        if (percentage > 10 .or. land < 9) then
            print '("Your heavy-handed performance smacks of Nero and Ivan IV.")'
            print '("The people (remaining) find you an unpleasant ruler, and,")'
            print '("frankly, hate your guts!!")'
        else if (percentage > 3 .or. land < 10) then
            call random_number(r)
            n = int(population * 0.8 * r)

            print '("Your performance could have been somewhat better but")'
            print '("really wasn''t too bad at all. ",i0," people")', n
            print '("would dearly like to see you assassinated but we all have our")'
            print '("trivial problems.")'
        else
            print '("A fantastic performance!! Charlemange, Disraeli, and")'
            print '("Jefferson combined could not have done better!")'
        end if

        call quit()
    end subroutine review

    subroutine trade(acres, storage, price)
        integer, intent(inout) :: acres
        integer, intent(inout) :: storage
        integer, intent(in)    :: price

        integer :: n, stat

        do
            print '("How many acres do you wish to buy?")'
            read (*, *, iostat=stat) n

            if (stat /= 0 .or. n < 0) call resign()

            if (price * n > storage) then
                call no_bushels(storage)
                cycle
            end if

            exit
        end do

        if (n > 0) then
            acres = acres + n
            storage = storage - price * n
            return
        end if

        do
            print '("How many acres do you wish to sell?")'
            read (*, *, iostat=stat) n

            if (stat /= 0 .or. n < 0) call resign()

            if (n >= acres) then
                call no_acres(acres)
                cycle
            end if

            exit
        end do

        if (n > 0) then
            acres = acres - n
            storage = storage + price * n
        end if
    end subroutine trade

    subroutine quit()
        print '(/, "So long for now.")'
        stop
    end subroutine quit
end program main
