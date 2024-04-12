! buzzword.f90
program main
    !! Buzzword
    !!
    !! This program is an invaluable aid for preparing speeches and briefings
    !! about educational technology. This buzzword generator provides sets of
    !! three highly-acceptable words to work into your material. Your audience
    !! will never know that the phrases don't really mean much of anything
    !! because they sound so great! Full instructions for running are given in
    !! the program.
    !!
    !! This version of Buzzword was written by David Ahl. Port to Fortran 2018
    !! by Philipp Engel.
    implicit none (type, external)

    character(len=*), parameter :: WORDS(39) = [ character(len=15) :: &
        'ability',        'basal',         'behavioral',      'child-centered', &
        'differentiated', 'discovery',     'flexible',        'heterogeneous', &
        'homogeneous',    'manipulative',  'modular',         'tavistock', &
        'individualized', 'learning',      'evaluative',      'objective', &
        'cognitive',      'enrichment',    'scheduling',      'humanistic', &
        'integrated',     'non-graded',    'training',        'vertical age', &
        'motivational',   'creative',      'grouping',        'modification', &
        'accountability', 'process',       'core curriculum', 'algorithm', &
        'performance',    'reinforcement', 'open classroom',  'resource', &
        'structure',      'facility',      'environment' ]

    character :: a
    integer   :: stat
    integer   :: i(3)
    real      :: r(3)

    call random_init(.false., .false.)

    print '(26x, "Buzzword Generator")'
    print '(15x, "Creative Computing  Morristown, New Jersey", /)'

    print '("This program prints highly acceptable phrases in")'
    print '("""educator-speak"" that you can work into reports")'
    print '("and speeches.  Whenever a question mark is printed,")'
    print '("type a ""y"" for another phrase or ""n"" to quit.", /)'

    print '("Here''s the first phrase:")'

    do
        call random_number(r)

        i = [ (13 * r(1) + 1), (13 * r(2) + 14), (13 * r(3) + 27) ]

        print '(/, a, 1x, a, 1x, a, /)', trim(WORDS(i(1))), &
                                         trim(WORDS(i(2))), &
                                         trim(WORDS(i(3)))

        write (*, '("? ")', advance='no')
        read (*, '(a)', iostat=stat) a

        if (stat /= 0) exit
        if (a /= 'Y' .and. a /= 'y') exit
    end do

    print '("Come back when you need help with another report!")'
end program main
