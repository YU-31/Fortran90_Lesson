program newtonRaphson_method
    implicit none

    real :: xtmp, xnew, eps
    integer :: cnt = 0, n

    print *, "必要精度：1.0*10^-n -> nを入力してください（正の数）"    !必要精度 = 解の差の最小値
    read *, n
    eps = 1.0e-1 ** n

    print '("初期値を入力してください")'
    read *, xtmp

 
    !    演算    !
    cnt = 0
    xnew = xtmp
    print *, "計算過程は以下の通りです。"
    print '("更新回数", 7x, "推定解", 12x, "関数値")'
    print '(3x, i3, 3x, 2x, f13.6, 2x, f18.6)', cnt, xnew, f(xnew)
    do
        cnt = cnt + 1
        xnew = xtmp - f(xtmp) / df(xtmp)
        print '(3x, i3, 3x, 2x, f13.6, 2x, f18.6)', cnt, xnew, f(xnew)
        if ( abs( xnew - xtmp ) <= eps ) then  
            exit
        end if
        xtmp = xnew
    end do

    !    最終的な計算結果の出力    !
    print '(f10.6, "が解として得られました")', xnew
    print '("その時の関数値は", f10.6, "です")', f(xnew)
    print '("更新回数は", i2, "回です")', cnt

    !  内部手続き  !
    contains

    function f(x) result(fvalue)
        implicit none
        real, intent(in) :: x
        real :: fvalue
        fvalue = x ** 3 - 21.0 * x ** 2 + 56.0 * x + 204.0
    end function f

    function df(x) result(dfvalue)
        implicit none
        real, intent(in) :: x
        real :: dfvalue
        dfvalue = 3.0 * x ** 2 - 42.0 * x + 56.0
    end function df

end program newtonRaphson_method