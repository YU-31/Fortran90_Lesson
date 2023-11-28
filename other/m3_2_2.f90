program m3_2_2
    implicit none
    
    real :: xtmp, xnew, eps
    integer :: cnt = 0

    print '("必要精度を入力してください（正の数）")'    !必要精度 = 最小区間幅
    read *, eps
    print '("初期値を入力してください")'
    read *, eps

    do
        cnt = cnt + 1
        xnew = xtmp - f(xtmp) / df(xtmp)
        if ( abs( xnew - xtmp ) <= eps ) then
            exit
        end if
        xtmp = xnew
    end do

    print '(f10.6, "が解として得られました")', xnew
    print '("その時の関数値は", f10.6, "です")', f(xnew)
    print '("更新回数は", i2, "回です")', cnt

    contains
    function f(x) result(fvalue)
        implicit none
        real, intent(in) :: x
        real :: fvalue
        fvalue = x ** 3 + x ** 2 - 5.0 * x - 2.0
    end function f

    function df(x) result(dfvalue)
        implicit none
        real, intent(in) :: x
        real :: dfvalue
        dfvalue = 3.0 * x ** 2 + 2.0 * x - 5.0
    end function df
end program m3_2_2