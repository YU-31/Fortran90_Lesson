!あとは初期区間を自動設定できるようにする
!配列番号の改修

program bisection_method
    implicit none
    
    real :: xmin, xmax, xc, eps, x1, x2
    integer :: cnt = 0, solution_num = 0, num

    real, dimension(:), allocatable :: xCoefficient1, xCoefficient2_a, xCoefficient2_b    !係数を格納
    real, dimension(:), allocatable :: solution !解を格納
    real, dimension(:), allocatable :: width    !最終的な区間長を記録
    integer, dimension(:), allocatable :: counter !反復回数の記録
    real, dimension(100) :: est !分割区間
    integer :: select, xOrder1, xOrder2, i

    !   方程式の構成    !
    print *, "入力する方程式の形式を選んでください"
    print *, "1. f(x) = a_n*x^n + a_n-1*x^n-1 + ... + a_1 * x + a_0"
    print *, "2. f(x) = (a_1*x + b_1)...(a_n*x + b_n)"
    read *, select
    do
        if ( select == 1 ) then
            print *, "方程式の次数を入力してください"
            read *, xOrder1
            allocate (xCoefficient1(0:xOrder1))
            xCoefficient1(:) = 0.0e0
            print *, "方程式の係数を次数の高い順に入力してください"
            read *, (xCoefficient1(i), i = 1, xOrder1 + 1)
            exit
        else if ( select == 2 ) then
            print *, "方程式の次数を入力してください"
            read *, xOrder2
            allocate (xCoefficient2_a(0:xOrder2), xCoefficient2_b(0:xOrder2))
            xCoefficient2_a(:) = 0.0e0
            xCoefficient2_b(:) = 0.0e0
            print *, "方程式の係数aを次数の高い順に入力してください"
            read *, (xCoefficient2_a(i), i = 1, xOrder2)
            print *, "方程式の係数bを次数の高い順に入力してください"
            read *, (xCoefficient2_b(i), i = 1, xOrder2)
            exit
        else
            print *, "1か2の数値で選択してください"
            read *, select
        end if
    end do

    !  入力結果の表示  !
    print *, "入力された方程式f(x) = 0の関数f(x)は"
    print *, "f(x) = "
    !f(x) = a_n*x^n + a_n-1*x^n-1 + ... + a_1 * x + a_0の場合
    if ( select == 1 ) then
        do i = 1, xOrder1 + 1
            if ( i < xOrder1 ) then
                if ( xCoefficient1(i) >= 0 ) then    !2次以上の項について
                    print ('(8x, "+", 1x, f4.1, 1x, "* x **", i2)'), xCoefficient1(i), xOrder1 + 1 - i
                else
                    print ('(8x, "-", 1x, f4.1, 1x, "* x **", i2)'), abs(xCoefficient1(i)), xOrder1 + 1 - i
                end if
            else if ( i == xOrder1 ) then            !1次の項について、次数を表記しない (ex. x^1 → x)
                if ( xCoefficient1(i) >= 0 ) then
                    print ('(8x, "+", 1x, f4.1, 1x, "* x")'), xCoefficient1(i)
                else
                    print ('(8x, "-", 1x, f4.1, 1x, "* x")'), abs(xCoefficient1(i))
                end if
            else                                    !定数項について、x以降を記述しない
                if ( xCoefficient1(i) >= 0 ) then
                    print ('(8x, "+", 1x, f4.1)'), xCoefficient1(i)
                else
                    print ('(8x, "-", 1x, f4.1)'), abs(xCoefficient1(i))
                end if
            end if
        end do
    !f(x) = (a_1*x + b_1)...(a_n*x + b_n)の場合
    else
        do i = 1, xOrder2 + 1
            if ( i == 1 ) then
                if ( xCoefficient2_b(i) >= 0 ) then
                    print ('(8x, "(", f4.1, "x", 1x, "+", 1x, f4.1, ")")'),&
                    & xCoefficient2_a(i), xCoefficient2_b(i)
                else
                    print ('(8x, "(", f4.1, "x", 1x, "-", 1x, f4.1, ")")'),&
                    & xCoefficient2_a(i), abs(xCoefficient2_b(i))
                end if
            else if ( i <= xOrder2 ) then
                if ( xCoefficient2_b(i) >= 0 ) then
                    print ('(6x, "* (", f4.1, "x", 1x, "+", 1x, f4.1, ")")'),&
                    & xCoefficient2_a(i), xCoefficient2_b(i)
                else
                    print ('(6x, "* (", f4.1, "x", 1x, "-", 1x, f4.1, ")")'),&
                    & xCoefficient2_a(i), abs(xCoefficient2_b(i))
                end if
            end if
        end do           
    end if
    print *, "です"

    print '("必要精度を入力してください（正の数）")'    !必要精度 = 最小区間幅
    read *, eps


    !                   初期区間の入力                       !
    !   初期区間内に解が含まれることを仮定している             !
    !   解を含む適切な初期区間が与えられるまでループする        !
!    do
!        print '("初期区間[xmin, xmax]を設定します（xmin<xmax）")'  
!        print '("xminの値を入力してください")'
!        read *, xmin
!        print '("xmaxの値を入力してください")'
!        read *, xmax
!        if ( select == 1 ) then
!            if ( f1(xmin) * f1(xmax) < 0 ) then         !中間値の定理により、区間の両端で符号が異なれば少なくとも1つの解を持つ
!            exit
!            end if
!        else
!            if ( f2(xmin) * f2(xmax) < 0 ) then
!            exit
!            end if
!        end if
!    end do

        !         計算過程の出力        !
    !   計算結果は line:    で出力  !



    !自動解探索
    allocate(solution(xOrder1))
    allocate(counter(xOrder1))
    allocate(width(xOrder1))
    if ( select == 1 ) then
        xmin = -50.0e0
        xmax = 50.0e0  !初期区間[-50, 50]
        do num = 1, 100
            est(num) = -50.0e0 + real(num) !区間リストの設定
        end do
        print '("計算過程は以下の通りです。")'
        do num = 1, 99      !解の探索
            x1 = est(num)
            x2 = est(num + 1)
            if ( f1(x1) * f1(x2) <0 ) then
                solution_num = solution_num + 1
                cnt = 0
                print ('(i1, "個目の解")'), solution_num
                print '("反復回数", 7x, "推定解", 12x, "関数値")'
                do
                    cnt = cnt + 1
                    xc = (x1 + x2) / 2
                    if ( f1(x1) * f1(xc) < 0 ) then
                        x2 = xc
                    else if ( f1(xc) == 0 ) then
                        exit
                    else
                        x1 = xc
                    end if
                    print '(3x, i2, 3x, 2x, f13.6, 2x, f18.6)', cnt, xc, f1(xc) !計算過程の出力
                    if ( abs(x1 - x2) <= eps ) then     !区間幅が必要精度以下になると終了
                        exit
                    else if ( cnt == 100 ) then             !100回の演算で収束しなければ強制終了
                        exit
                    end if
                end do
                solution(solution_num) = xc
                counter(solution_num) = cnt
                width(solution_num) = abs(x1 - x2)
            end if
        end do
    end if


  
    ! !         区間の更新          !
    ! !   区間を1/2ずつ狭めていく    !
    ! if ( select == 1 ) then
    !     do
    !         cnt =  cnt + 1                          !区間の更新回数
    !         xc = (xmin + xmax) / 2                  !区間の中点xc
    !         if ( f1(xmin) * f1(xc) < 0 ) then         !左側の区間が解を含むとき
    !             xmax = xc
    !         else if ( f1(xc) == 0 ) then             !xcが解であるとき
    !             exit
    !         else
    !             xmin = xc
    !         end if
    
    !         print '(3x, i2, 3x, 2x, f13.6, 2x, f18.6)', cnt, xc, f1(xc)
    
    !         if ( abs(xmax - xmin) <= eps ) then     !区間幅が必要精度以下になると終了
    !             exit
    !         else if ( cnt == 100 ) then             !100回の演算で収束しなければ強制終了
    !             exit
    !         end if
    !     end do
    ! !f(x) = (a_1*x + b_1)...(a_n*x + b_n)の場合
    ! else
    !     do
    !         cnt =  cnt + 1                          !区間の更新回数
    !         xc = (xmin + xmax) / 2                  !区間の中点xc
    !         if ( f2(xmin) * f2(xc) < 0 ) then         !左側の区間が解を含むとき
    !             xmax = xc
    !         else if ( f2(xc) == 0 ) then             !xcが解であるとき
    !             exit
    !         else
    !             xmin = xc
    !         end if
    
    !         print '(3x, i2, 3x, 2x, f13.6, 2x, f18.6)', cnt, xc, f2(xc)
    
    !         if ( abs(xmax - xmin) <= eps ) then     !区間幅が必要精度以下になると終了
    !             exit
    !         else if ( cnt == 100 ) then             !100回の演算で収束しなければ強制終了
    !             exit
    !         end if
    !     end do
    ! end if
      
  
    !    最終的な計算結果の出力    !
    do solution_num = 1, xOrder1
        print '(i1, "個目の解：")', solution_num
        print '(f10.6, "が解として得られました")', solution(solution_num)
        if (select == 1) then
            print '("その時の関数値は", f10.6, "です")', f1(solution(solution_num))
        else
            print '("その時の関数値は", f10.6, "です")', f2(solution(solution_num))
        end if        
        print '("区間分割回数は", i2, "回です")', counter(solution_num)
        print '("最終分割後の区間長は",f10.6, "です")', width(solution_num)
    end do

    ! print '(f10.6, "が解として得られました")', (solution(solution_num), solution_num = 1, xOrder1)
    ! if (select == 1) then
    !     print '("その時の関数値は", f10.6, "です")', f1(xc)
    ! else
    !     print '("その時の関数値は", f10.6, "です")', f2(xc)
    ! end if        
    ! print '("区間分割回数は", i2, "回です")', cnt
    ! print '("最終分割後の区間長は",f10.6, "です")', xmax - xmin

    !  内部手続き  !
    contains
    !  関数の入力  !
    !f(x) = a_n*x^n + a_n-1*x^n-1 + ... + a_1 * x + a_0の場合
    function f1(x) result(fvalue1)
        implicit none
        integer :: i
        real, intent(in) :: x
        real :: fvalue1, ftmp1
        
        ftmp1 = 0
        
        do i = 1, xOrder1 + 1
            ftmp1 = ftmp1 + xCoefficient1(i) * x ** (xOrder1 + 1 - i)
        end do
        fvalue1 = ftmp1
    end function f1

    !f(x) = (a_1*x + b_1)...(a_n*x + b_n)の場合
    function f2(x) result(fvalue2)
        implicit none
        real,intent(in) :: x
        real :: fvalue2, ftmp2
    
        ftmp2 = 1.0e0
        
        do i = 1, xOrder2
            ftmp2 = ftmp2 * ( xCoefficient2_a(i) * x ** (xOrder2 + 1 - i) + xCoefficient2_b(i))
        end do
        fvalue2 = ftmp2
    end function f2

end program bisection_method