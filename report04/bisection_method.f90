program bisection_method
    implicit none
    
    real :: xmin, xmax, xc, eps
    integer :: cnt = 0

    real, dimension(100) :: xCoefficient = 0    !係数を格納
    integer :: xOrder, i

    !                               方程式の構成                             !
    !               f(x) = a_n*x^n + ... + a_1*x + a_0 の形式               !
    !   f(x) = (a_1*x + b_1)...(a_n*x + b_n) の形式も入力できるようにすべき   !
    print *, "方程式の次数を入力してください"
    read *, xOrder
    print *, "方程式の係数を次数の高い順に入力してください"
    read *, (xCoefficient(i), i = 1, xOrder + 1)

    !  入力結果の表示  !
    print *, "入力された方程式f(x) = 0の関数f(x)は"
    print *, "f(x) = "
    do i = 1, xOrder + 1
        if ( i < xOrder ) then
            if ( xCoefficient(i) >= 0 ) then    !2次以上の項について
                print ('(8x, "+", 1x, f4.1, 1x, "* x **", i2)'), xCoefficient(i), xOrder + 1 - i
            else
                print ('(8x, "-", 1x, f4.1, 1x, "* x **", i2)'), abs(xCoefficient(i)), xOrder + 1 - i
            end if
        else if ( i == xOrder ) then            !1次の項について、次数を表記しない (ex. x^1 → x)
            if ( xCoefficient(i) >= 0 ) then
                print ('(8x, "+", 1x, f4.1, 1x, "* x")'), xCoefficient(i)
            else
                print ('(8x, "-", 1x, f4.1, 1x, "* x")'), abs(xCoefficient(i))
            end if
        else                                    !定数項について、x以降を記述しない
            if ( xCoefficient(i) >= 0 ) then
                print ('(8x, "+", 1x, f4.1)'), xCoefficient(i)
            else
                print ('(8x, "-", 1x, f4.1)'), abs(xCoefficient(i))
            end if
        end if
    end do
    print *, "です"

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
      !   計算結果は line: 80 で出力  !
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

    !  内部手続き  !
    contains
    !  関数の入力  !
    function f(x) result(fvalue)
        implicit none
        integer :: i
        real, intent(in) :: x
        real :: fvalue, ftmp
        
        ftmp = 0
        
        do i = 1, xOrder + 1
            ftmp = ftmp + xCoefficient(i) * x ** (xOrder + 1 - i)
        end do
        fvalue = ftmp
    end function f

!   NO NEED FOR THIS PROGRAM
!   THIS FUNCTION ENABLE TO CALCULATE DIFFERENCIAL
!
!    function df(x) result(dfvalue)
!        implicit none
!        integer :: i
!        real, intent(in) :: x
!        real :: dfvalue
!        
!        do i = 1, xOrder + 1
!            dfvalue = dfvalue + (xOrder + 1 - i) * xCoefficient(i) * x ** (xOrder - i)
!        end do
!    end function df

end program bisection_method