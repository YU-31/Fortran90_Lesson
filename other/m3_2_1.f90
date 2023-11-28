program m3_2_1
    implicit none
    
    real :: xmin, xmax, xc, eps
    integer :: cnt = 0

    print '("必要精度を入力してください（正の数）")'    !必要精度 = 最小区間幅
    read *, eps

    !                   初期区間の入力                       !
    !   初期区間内に解が含まれることを仮定している（line: 19）  !
    !   解を含む適切な初期区間が与えられるまでループする        !
    do
      print '("初期区間[xmin, xmax]を設定します（xmin<xmax）")'  
      print '("xminの値を入力してください")'
      read *, xmin
      print '("xmaxの値を入力してください")'
      read *, xmax
      if ( f(xmin) * f(xmax) < 0 ) then         !中間値の定理により、区間の両端で符号が異なれば少なくとも1つの解を持つ
        exit
      end if
    end do

    !         計算過程の出力        !
    !   計算結果は line: 41 で出力  !
    print '("計算過程")'
    print '("反復回数", 6x, "推定解", 6x, "関数値")'

    !         区間の更新          !
    !   区間を1/2ずつ狭めていく    !
    do
        cnt =  cnt + 1                          !区間の更新回数
        xc = (xmin + xmax) / 2                  !区間の中点xc
        if ( f(xmin) * f(xc) < 0 ) then         !左側の区間が解を含むとき
            xmax =xc
        else if ( f(xc) == 0 ) then             !xcが解であるとき
            exit
        else
            xmin =xc
        end if

        print '(3x, i2, 3x, 4x, f10.6, 2x, f10.6)', cnt, xc, f(xc)

        if ( abs(xmax - xmin) <= eps ) then     !区間幅が必要精度以下になると終了
            exit
        end if
    end do

    !    最終的な計算結果の出力    !
    print '(f10.6, "が解として得られました")', xc
    print '("その時の関数値は", f10.6, "です")', f(xc)
    print '("区間分割回数は", i2, "回です")', cnt
    print '("最終分割後の区間長は",f10.6, "です")', xmax - xmin

    !   f(x)の定義   !
    contains
    function f(x) result(fvalue)
        real, intent(in) ::  x
        real :: fvalue
        fvalue = x ** 3 + x ** 2 - 5.0 * x - 2.0 
    end function f
end program m3_2_1