program bessel_rungeKutta
   implicit none
   integer :: nsteps   !全ステップ数
   !integer :: nout     !データ出力回数
   !integer :: ntime    !出力データの間隔 !Runge-Kuttaだとそもそも計算回数が少ない
   integer :: i
   real :: h   !xの刻み幅
   real :: xend    !xの最終値
   real :: xstep   !xの出力刻み幅
   real :: x0, y0, z0  !x, y, zの初期値
   real :: k1, k2, k3, k4, l1, l2, l3, l4
   real, dimension(0:1000) :: bessel
   real, dimension(0:1000) :: x1, y1, z1, x2, y2, z2, x3, y3, z3

   open (1, file='input.txt')
   open (21, file='rungeKutta2.dat')
   open (22, file='rungeKutta3.dat')
   open (23, file='rungeKutta4.dat')

   read (1, '(6f6.3)') h, xend, xstep, x0, y0, z0
   close (1)

   x1(0) = x0
   x2(0) = x0
   x3(0) = x0
   y1(0) = y0
   y2(0) = y0
   y3(0) = y0
   z1(0) = z0
   z2(0) = z0
   z3(0) = z0
   nsteps = nint((xend - x0)/h)
   !nout = nint((xend - x0)/xstep)
   !ntime = nout/nsteps

   !  初期条件の処理はSubroutine内  !

   !2次
   call kl_set1()
   x1(1) = x1(0) + h
   y1(1) = y1(0) + (k1 + k2)*0.50     !f0 = dy/dz = 0
   z1(1) = z1(0) + (l1 + l2)*0.50     !g0 = -(1/x)(dy/dx) - y = -(-1/2) - 1 = -1/2

   do i = 1, nsteps - 1
      k1 = h*f(x1(i), y1(i), z1(i))
      l1 = h*g(x1(i), y1(i), z1(i))
      k2 = h*f(x1(i) + h/2, y1(i) + k1/2, z1(i) + l1/2)
      l2 = h*g(x1(i) + h/2, y1(i) + k1/2, z1(i) + l1/2)

      x1(i + 1) = x1(i) + h
      y1(i + 1) = y1(i) + (k1 + k2)/2
      z1(i + 1) = z1(i) + (l1 + l2)/2
   end do

   !3次
   call kl_set2()
   x2(1) = x2(0) + h
   y2(1) = y2(0) + (k1 + 4*k2 + k3)/6
   z2(1) = z2(0) + (l1 + 4*l2 + l3)/6

   do i = 1, nsteps - 1
      k1 = h*f(x2(i), y2(i), z2(i))
      l1 = h*g(x2(i), y2(i), z2(i))
      k2 = h*f(x2(i) + h/2, y2(i) + k1/2, z2(i) + l1/2)
      l2 = h*g(x2(i) + h/2, y2(i) + k1/2, z2(i) + l1/2)
      k3 = h*f(x2(i) + h/2, y2(i) - k1 + 2*k2, z2(i) - l1 + 2*l2)
      l3 = h*g(x2(i) + h/2, y2(i) - k1 + 2*k2, z2(i) - l1 + 2*l2)

      x2(i + 1) = x2(i) + h
      y2(i + 1) = y2(i) + (k1 + 4*k2 + k3)/6
      z2(i + 1) = z2(i) + (l1 + 4*l2 + l3)/6
   end do

   !4次
   call kl_set3
   x3(1) = x3(0) + h
   y3(1) = y3(0) + (k1 + 2*k2 + 2*k3 + k4)/6
   z3(1) = z3(0) + (l1 + 2*l2 + 2*l3 + l4)/6

   do i = 1, nsteps - 1
      k1 = h*f(x3(i), y3(i), z3(i))
      l1 = h*g(x3(i), y3(i), z3(i))
      k2 = h*f(x3(i) + h/2, y3(i) + k1/2, z3(i) + l1/2)
      l2 = h*g(x3(i) + h/2, y3(i) + k1/2, z3(i) + l1/2)
      k3 = h*f(x3(i) + h/2, y3(i) + k2/2, z3(i) + l2/2)
      l3 = h*g(x3(i) + h/2, y3(i) + k2/2, z3(i) + l2/2)
      k4 = h*f(x3(i) + h, y3(i) + k3, z3(i) + l3)
      l4 = h*g(x3(i) + h, y3(i) + k3, z3(i) + l3)

      x3(i + 1) = x3(i) + h
      y3(i + 1) = y3(i) + (k1 + 2*k2 + 2*k3 + k4)/6
      z3(i + 1) = z3(i) + (l1 + 2*l2 + 2*l3 + l4)/6
   end do

   do i = 0, nsteps
      bessel(i) = bessel_j0(x1(i))
   end do

   do i = 0, nsteps!, ntime
      write (21, '(4f10.6)') x1(i), y1(i), z1(i), abs(y1(i) - bessel(i))
      write (22, '(4f10.6)') x2(i), y2(i), z2(i), abs(y2(i) - bessel(i))
      write (23, '(4f10.6)') x3(i), y3(i), z3(i), abs(y3(i) - bessel(i))
   end do
   close (21)
   close (22)
   close (23)

contains
   real function f(x, y, z) result(fresult)
      implicit none
      real, intent(in) :: x, y, z
      fresult = z
      return
   end function f

   real function g(x, y, z) result(gresult)
      implicit none
      real, intent(in) :: x, y, z
      gresult = -(1/x)*f(x, y, z) - y
      return
   end function g

   !  初期条件によるk, lの計算  !
   !初期条件：
   !    x0 = 0において, y0 = 1, dy/dx = 0, (1/x)(dy/dx) = -1/2
   !    f0 = dy/dz = 0, g0 = -(1/x)(dy/dx) - y = -(-1/2) - 1 = -1/2
   !    k1 = h*f0 = 0, l1 = h*g0 = -(1/2)h

   subroutine kl_set1()
      k1 = 0
      l1 = -h/2
      k2 = h*f(h/2, 1 + k1/2, l1/2)
      l2 = h*g(h/2, 1 + k1/2, l1/2)
   end subroutine kl_set1

   subroutine kl_set2()
      k1 = 0
      l1 = -h/2
      k2 = h*f(h/2, 1 + k1/2, l1/2)
      l2 = h*g(h/2, 1 + k1/2, l1/2)
      k3 = h*f(h/2, 1 - k1 + 2*k2, -l1 + 2*l2)
      l3 = h*g(h/2, 1 - k1 + 2*k2, -l1 + 2*l2)
   end subroutine kl_set2

   subroutine kl_set3()
      k1 = 0
      l1 = -h/2
      k2 = h*f(h/2, 1 + k1/2, l1/2)
      l2 = h*g(h/2, 1 + k1/2, l1/2)
      k3 = h*f(h/2, 1 + k2/2, l2/2)
      l3 = h*g(h/2, 1 + k2/2, l2/2)
      k4 = h*f(h, 1 + k3, l3)
      l4 = h*g(h, 1 + k3, l3)
   end subroutine kl_set3

end program bessel_rungeKutta
