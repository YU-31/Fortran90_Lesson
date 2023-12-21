program bessel1
   implicit none
   integer :: nsteps   !全ステップ数
   integer :: nout     !データ出力回数
   integer :: ntime    !出力データの間隔
   integer :: i
   real :: h   !xの刻み幅
   real :: xend    !xの最終値
   real :: xstep   !xの出力刻み幅
   real :: x0, y0, z0  !x, y, zの初期値
   real, dimension(0:10000) :: bessel
   real, dimension(0:10000) :: x1, y1, z1

   open (1, file='input.txt')
   open (21, file='output.dat')

   read (1, '(6f6.3)') h, xend, xstep, x0, y0, z0
   close (1)

   x1(0) = x0
   y1(0) = y0
   z1(0) = z0
   nsteps = nint((xend - x0)/h)
   nout = nint((xend - x0)/xstep)
   ntime = nsteps/nout

   !一回目のみ特殊
   !初期条件：
   !    x0 = 0において, y0 = 1, dy/dx = 0, (1/x)(dy/dx) = -1/2
   x1(1) = x1(0) + h
   y1(1) = y1(0) + h*0      !f0 = dy/dz = 0
   z1(1) = z1(0) - h/2   !g0 = -(1/x)(dy/dx) - y = -(-1/2) - 1 = -1/2

   do i = 1, nsteps - 1
      x1(i + 1) = x1(i) + h
      y1(i + 1) = y1(i) + h*f(x1(i), y1(i), z1(i))
      z1(i + 1) = z1(i) + h*g(x1(i), y1(i), z1(i))
   end do

   do i = 0, nsteps
      bessel(i) = bessel_j0(x1(i))
   end do

   do i = 0, nsteps, ntime
      write (21, '(4f10.6)') x1(i), y1(i), z1(i), abs(y1(i) - bessel(i))
   end do
   close (21)

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

end program bessel1
