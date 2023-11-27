program problem02
    implicit none
    integer :: i
    real(8) :: x, x0, dx, fn, diffn

    dx = 0.1
    x0 = 0.0

    do i = 0, 50
        x = x0 + i * dx !Increase by dx(=0.1)
        print ('(3f8.3)'), x, fn(x), diffn(x)
    end do
end program problem02

!Function to calculate value of f(x)
function fn(x)
    implicit none
    real(8) :: fn, x    !Variables are indepent from main
    fn = x**3 - 5 * x**2 + 4 * x + 2    
end function fn

!Function to calculate value of f'(x)
function diffn(x)
    implicit none
    real(8) :: diffn, fn, x, dx     !Variables are indepent from main
    dx = 1.0e-8
    diffn = (fn(x + dx) - fn(x)) / dx
end function diffn