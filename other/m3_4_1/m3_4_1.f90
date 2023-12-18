program m3_4_1_eulerMethod
    implicit none
    integer :: ne, nout, ntime, i   !全ステップ数, データ出力回数, 出力データの間隔, カウント用
    real :: h   !xの刻み幅
    real :: xe  !xの最終値
    real :: xstep   !xの出力刻み幅
    real :: x0, y0  !x,yの初期値
    real, dimension(0:1000) :: x1, y1

    open (1, file = 'input.txt')
    open (2, file = 'output.txt')

    read (1, '(5f6.3)') h, xe, xstep, x0, y0
    close(1)

    x1(0) = x0
    y1(0) = y0
    ne = nint((xe - x0) / h)    !計算回数
    nout = nint((xe - x0) / xstep)  !出力回数
    ntime = ne / nout   !出力データの間隔

    do i = 0, ne - 1
        x1(i + 1) = x1(i) + h
        y1(i + 1) = y1(i) + h * f(x1(i), y1(i))
    end do

    do i = 0, ne - 1
        write (2, *) x1(i), y1(i)
    end do
    close(2)

    contains
    real function f(x, y)
        real, intent(in) :: x, y
        f = x
    end function f

end program m3_4_1_eulerMethod