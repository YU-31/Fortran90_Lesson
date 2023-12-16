program bisection_method
    implicit none

    real :: xmin, xmax, xc, eps
    integer :: cnt = 0, n

    print *, "必要精度：1.0*10^-n -> nを入力してください（正の数）"    !必要精度 = 解の差の最小値
    read *, n
    eps = 1.0e-1 ** n

    !                   初期区間の入力                       !
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
      print '("計算過程")'
      print '("反復回数", 6x, "推定解", 6x, "関数値")'
  
      !         演算          !
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
    print '("更新回数は", i2, "回です")', cnt

    !  内部手続き  !
    contains

    function f(x) result(fvalue)
        implicit none
        real, intent(in) :: x
        real :: fvalue
        fvalue = x ** 3 - 21.0 * x ** 2 + 56.0 * x + 204.0
    end function f

end program bisection_method