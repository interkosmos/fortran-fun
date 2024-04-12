! amazing.f90
program main
    !! Amazing
    !!
    !! This program will print out a different maze every time it is run and
    !! guarantees only one path through.
    !!
    !! The original program author was Jack Tauber of Windsor, Connecticut.
    !! Converted to Fortran 2018 by Philipp Engel.
    implicit none (type, external)

    integer, parameter :: DIR_LEFT  = 1
    integer, parameter :: DIR_UP    = 2
    integer, parameter :: DIR_RIGHT = 3
    integer, parameter :: DIR_DOWN  = 4

    integer, parameter :: EXIT_DOWN  = 1
    integer, parameter :: EXIT_RIGHT = 2

    ! Intitially set to zero, unprocessed cells.
    ! Filled in with consecutive non-zero numbers as cells are processed.
    integer, allocatable :: used(:, :)

    ! Initially set to zero (all paths blocked).
    ! Remains 0 if there is no exit down or right.
    ! Set to 1 if there is an exit down.
    ! Set to 2 if there is an exit right.
    ! Set to 3 if there are exits down and right.
    integer, allocatable :: walls(:, :)

    integer :: choice, entrance, n, stat
    integer :: col, row, ncols, nrows
    logical :: dirs(4)
    real    :: r

    ! Initialise random number generator.
    call random_init(.false., .false.)

    print '(/, 13x, "Amazing Program")'
    print '("Creative Computing  Morristown, New Jersey")'

    do
        print '(/, "What are your width and height?")'
        read (*, *, iostat=stat) ncols, nrows
        if (stat == 0 .and. ncols > 1 .and. nrows > 1) exit
        print '("Meaningless dimensions. Try again.")'
    end do

    ! Allocate memory, fill arrays with zeros.
    allocate (used(ncols, nrows), source=0)
    allocate (walls(ncols, nrows), source=0)

    call random_number(r)
    entrance = 1 + int(r * ncols)

    row = 1
    col = entrance

    n = 1
    used(col, row) = n
    n = n + 1

    do while (n < ncols * nrows + 1)
        ! Remove possible dirs that are blocked or
        ! hit cells that have been processed already.
        dirs(:) = .true.

        if (col == 1 .or. used(col - 1, row) /= 0) &
            dirs(DIR_LEFT) = .false.

        if (row == 1 .or. used(col, row - 1) /= 0) &
            dirs(DIR_UP) = .false.

        if (col == ncols .or. used(col + 1, row) /= 0) &
            dirs(DIR_RIGHT) = .false.

        if (row == nrows .or. used(col, row + 1) /= 0) &
            dirs(DIR_DOWN) = .false.

        if (any(dirs)) then
            ! If we can move in a direction, move and make opening.
            do
                call random_number(r)
                choice = 1 + int(r * size(dirs))
                if (dirs(choice)) exit
            end do

            select case (choice)
                case (DIR_LEFT)
                    col = col - 1
                    walls(col, row) = EXIT_RIGHT

                case (DIR_UP)
                    row = row - 1
                    walls(col, row) = EXIT_DOWN

                case (DIR_RIGHT)
                    walls(col, row) = walls(col, row) + EXIT_RIGHT
                    col = col + 1

                case (DIR_DOWN)
                    walls(col, row) = walls(col, row) + EXIT_DOWN
                    row = row + 1
            end select

            used(col, row) = n
            n = n + 1
        else
            ! Move to the next used cell, and try again.
            do
                if (col < ncols) then
                    col = col + 1
                else if (row < nrows) then
                    col = 1
                    row = row + 1
                else
                    col = 1
                    row = 1
                end if

                if (used(col, row) /= 0) exit
            end do
        end if
    end do

    ! Add random exit.
    call random_number(r)
    col = 1 + int(r * ncols)
    row = nrows

    walls(col, row) = walls(col, row) + 1

    ! Print the maze.
    do col = 1, ncols
        if (col == entrance) then
            write (*, '(".  ")', advance='no')
        else
            write (*, '(".--")', advance='no')
        end if
    end do

    write (*, '(".")')

    do row = 1, nrows
        write (*, '("|")', advance='no')

        do col = 1, ncols
            if (walls(col, row) < 2) then
                write (*, '("  |")', advance='no')
            else
                write (*, '("   ")', advance='no')
            end if
        end do

        write (*, *)

        do col = 1, ncols
            if (walls(col, row) == 0 .or. walls(col, row) == 2) then
                write (*, '(":--")', advance='no')
            else
                write (*, '(":  ")', advance='no')
            end if
        end do

        write (*, '(".")')
    end do
end program main
