program assignment01
    implicit none
    integer :: i, j, n, num(100)

    print *, 'Input Total Number'
    read *, n
    print *, 'Numbers'
    read *, (num(i), i=1, n)

    do i = 1, n-1
        do j = i+1, n
            if ( num(i) > num(j) ) call SWAP(num(i), num(j))
        end do
    end do

    print ('(10i5)'), (num(i), i = 1, n)
end program assignment01

!Program to exchange numbers
!from (a,b) to (b,a)
subroutine SWAP(a, b)
    implicit none
    integer :: a, b, c
    c = a
    a = b
    b = c 
end subroutine SWAP