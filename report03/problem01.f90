program problem01
    implicit none
    integer :: i, j, n
    real, dimension(1:10) :: x, y, z, d

    print *, 'Sort by shortest distance.'
    print *, 'Input Total Number'
    read *, n
    print *, 'Input x, y, z'
    do i = 1, n
        read *, x(i), y(i), z(i)
    end do

    do i = 1, n
        call dist(x(i), y(i), z(i), d(i))
    end do

    do i = 1, n-1
        do j = i+1, n
            if ( d(i) > d(j) ) then
                call swap(x(i), x(j))
                call swap(y(i), y(j))
                call swap(z(i), z(j))
                call swap(d(i), d(j))
            end if
        end do
    end do

    print *, 'Show (x, y, z), distance; Sorted by shortest distance.'
    do i = 1, n
        print ('("(", f10.4, ", ", f10.4, ", ", f10.4, "), d =", f10.4)'), x(i), y(i), z(i), d(i)
    end do
end program problem01

!Program to exchange numbers
!from (a,b) to (b,a)
subroutine swap(a, b)
    implicit none
    real :: a, b, c
    c = a
    a = b
    b = c 
end subroutine swap

!Program to calculate distance
subroutine dist(x, y, z, d)
    implicit none
    real :: x, y, z, d
    d = (x**2 + y**2 + z**2)**0.5    
end subroutine dist