program problem03
    implicit none
    integer :: i
    real :: x(3), y(3), z(3), s, a

    print *, 'Calculate area of triangle.'
    print *, 'Input x, y, z of three points.'
    do i = 1, 3
        read *, x(i), y(i), z(i)
    end do

    !A(x(1), y(1), z(1)), ... , C(x(3), y(3), z(3))
    !ex. 
    !   dist(1,2) = AB, dist(2,3)=BC, dist(3,1)=CA
    !
    s = (dist(1,2) + dist(2,3) + dist(3,1)) / 2
    a = (s * (s - dist(1,2)) * (s - dist(2,3)) * (s - dist(3,1)))**0.5

    print *, 'Entered points:'
    do i = 1, 3
        print('("(", f10.4, ", ", f10.4, ", ", f10.4, ")")'), x(i), y(i), z(i)
    end do
    print ('("Area : ",f10.5)'), a

    !Function to calculate distance of two points
    contains
    real function dist(i,j)
        implicit none
        integer :: i, j !Use i,j independently in function dist
        dist = ((x(i) - x(j))**2 + (y(i)-y(j))**2 + (z(i) - z(j))**2)**0.5
    end function dist
end program problem03