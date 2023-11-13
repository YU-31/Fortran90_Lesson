!create multiplication table
program m2_6_8

    implicit none
    integer :: i, j

    print '(3X, 1X, "|", 9I3)', (i, i = 1, 9)
    print '(3X, 1X, "+", 27A1)', ('-', i = 1, 27)
    do j = 1, 9
        print '(I3, 1X, "|", 9I3)', j, (i * j, i = 1,9)
    end do
    
end program m2_6_8