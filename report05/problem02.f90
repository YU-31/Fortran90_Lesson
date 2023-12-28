program bessel_rungeKutta
   implicit none
   integer :: nsteps   !全ステップ数
   integer :: i
   real :: h   !xの刻み幅
   real :: xend    !xの最終値
   real :: xstep   !xの出力刻み幅
   real :: x0, y0, z0  !x, y, zの初期値
   real :: k1, k2, k3, k4, l1, l2, l3, l4
   real, dimension(0:1000) :: x1, y1, z1, x2, y2, z2, x3, y3, z3
   real, dimension(0:1000) :: bessel   !真の解を格納
   integer :: nout, ntime

   !  ファイル操作   !
   open (1, file='input.txt')
   open (21, file='rungeKutta2.dat')   !2次の結果の記録
   open (22, file='rungeKutta3.dat')   !3次の結果の記録
   open (23, file='rungeKutta4.dat')   !4次の結果の記録

   !  各パラメータの読み取り  !
   read (1, '(6f6.3)') h, xend, xstep, x0, y0, z0
   close (1)

   !  初期設定  !
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
   nsteps = nint((xend - x0)/h)  !計算回数
   nout = nint((xend - x0)/xstep)   !出力回数
   ntime = nsteps/nout  !出力のx間隔

   !  演算  !
   !初期条件によるパラメータk, lの設定はSubroutine内

   !初期条件によるk, lの計算
   !    k = h*f, l = h*g
   !初期条件：
   !    x0 = 0において, y0 = 1, dy/dx = 0, (1/x)(dy/dx) = -1/2
   !    ->  f0 = dy/dx = 0, g0 = -(1/x)(dy/dx) - y = -(-1/2) - 1 = -1/2
   !    ->  k1 = h*f0 = 0, l1 = h*g0 = -(1/2)h, ... と定まる
   k1 = 0
   l1 = -h/2

   !2次
   do i = 0, nsteps - 1
      if (i == 0) then
         k2 = h*f(x1(i) + h/2, y1(i) + k1/2, z1(i) + l1/2)
         l2 = h*g(x1(i) + h/2, y1(i) + k1/2, z1(i) + l1/2)

         x1(i + 1) = x1(i) + h
         y1(i + 1) = y1(i) + (k1 + k2)/2
         z1(i + 1) = z1(i) + (l1 + l2)/2
      else
         k1 = h*f(x1(i), y1(i), z1(i))
         l1 = h*g(x1(i), y1(i), z1(i))
         k2 = h*f(x1(i) + h/2, y1(i) + k1/2, z1(i) + l1/2)
         l2 = h*g(x1(i) + h/2, y1(i) + k1/2, z1(i) + l1/2)

         x1(i + 1) = x1(i) + h
         y1(i + 1) = y1(i) + (k1 + k2)/2
         z1(i + 1) = z1(i) + (l1 + l2)/2
      end if
   end do

   !3次
   do i = 0, nsteps - 1
      if (i == 0) then
         k2 = h*f(x2(i) + h/2, y2(i) + k1/2, z2(i) + l1/2)
         l2 = h*g(x2(i) + h/2, y2(i) + k1/2, z2(i) + l1/2)
         k3 = h*f(x2(i) + h/2, y2(i) - k1 + 2*k2, z2(i) - l1 + 2*l2)
         l3 = h*g(x2(i) + h/2, y2(i) - k1 + 2*k2, z2(i) - l1 + 2*l2)

         x2(i + 1) = x2(i) + h
         y2(i + 1) = y2(i) + (k1 + 4*k2 + k3)/6
         z2(i + 1) = z2(i) + (l1 + 4*l2 + l3)/6
      else
         k1 = h*f(x2(i), y2(i), z2(i))
         l1 = h*g(x2(i), y2(i), z2(i))
         k2 = h*f(x2(i) + h/2, y2(i) + k1/2, z2(i) + l1/2)
         l2 = h*g(x2(i) + h/2, y2(i) + k1/2, z2(i) + l1/2)
         k3 = h*f(x2(i) + h/2, y2(i) - k1 + 2*k2, z2(i) - l1 + 2*l2)
         l3 = h*g(x2(i) + h/2, y2(i) - k1 + 2*k2, z2(i) - l1 + 2*l2)

         x2(i + 1) = x2(i) + h
         y2(i + 1) = y2(i) + (k1 + 4*k2 + k3)/6
         z2(i + 1) = z2(i) + (l1 + 4*l2 + l3)/6
      end if
   end do

   !4次
   do i = 0, nsteps - 1
      if (i == 0) then
         k2 = h*f(x3(i) + h/2, y3(i) + k1/2, z3(i) + l1/2)
         l2 = h*g(x3(i) + h/2, y3(i) + k1/2, z3(i) + l1/2)
         k3 = h*f(x3(i) + h/2, y3(i) + k2/2, z3(i) + l2/2)
         l3 = h*g(x3(i) + h/2, y3(i) + k2/2, z3(i) + l2/2)
         k4 = h*f(x3(i) + h, y3(i) + k3, z3(i) + l3)
         l4 = h*g(x3(i) + h, y3(i) + k3, z3(i) + l3)

         x3(i + 1) = x3(i) + h
         y3(i + 1) = y3(i) + (k1 + 2*k2 + 2*k3 + k4)/6
         z3(i + 1) = z3(i) + (l1 + 2*l2 + 2*l3 + l4)/6
      else
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
      end if
   end do

   !  真の解の求値  !
   do i = 0, nsteps - 1
      bessel(i) = bessel_j0(x1(i))
   end do

   !  結果の出力  !
   do i = 0, nsteps
      write (21, '(4f10.6)') x1(i), y1(i), z1(i), abs(y1(i) - bessel(i))
      write (22, '(4f10.6)') x2(i), y2(i), z2(i), abs(y2(i) - bessel(i))
      write (23, '(4f10.6)') x3(i), y3(i), z3(i), abs(y3(i) - bessel(i))
   end do
   close (21)
   close (22)
   close (23)

   !  内部手続き  !
contains
   !関数f, gの定義
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

end program bessel_rungeKutta
