program problem02
    implicit none
    integer, dimension(1:4) :: no           !Student No.
    integer, dimension(1:5) :: wa           !Sum of each subject
    integer, dimension(1:4, 1:5) :: ten     !Points of each subject
    real, dimension(1:5) ::avg              !Average point of each subjects
    integer :: i, j

    wa = 0
    
    !Input from data file
    open(1, file='data.txt')
    do i = 1, 4
        read (1, '(I3, 5I6)') no(i), (ten(i, j), j = 1, 5) !Load data of points
    end do

    !Calculate sum by each subject
    do j = 1, 5
        do i = 1, 4
            wa(j) = wa(j) + ten(i, j)
        end do
    end do

    !Calculate average by each subject
    do j = 1, 5
        avg(j) = real(wa(j)) / 4.0e0
    end do

    !Output
    print '(A4, A6, A7, A5, 2A6)', 'Num', 'ENG', 'MATH', 'JPN', 'SCI', 'SOC'
    do i = 1, 4
        print '(I3, I7, 4I6)', no(i), (ten(i, j), j = 1, 5)
    end do
    print '(1X, A, 1X, 5F6.1)', 'Avg', (avg(j), j = 1, 5)

end program problem02