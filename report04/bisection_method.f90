program bisection_method

    implicit none
    
    real :: x1, x2, xc, eps
    integer :: cnt = 0, solution_num = 0
    integer :: xOrder !次数
    integer :: i, num   !カウンタ
    integer :: n

    real, dimension(:), allocatable :: xCoefficient   !係数を格納
    real, dimension(:), allocatable :: solution !解を格納
    real, dimension(:), allocatable :: width    !最終的な区間長を記録
    integer, dimension(:), allocatable :: counter !反復回数の記録
    real, dimension(200) :: est

    !   方程式の構成    !
    print *, "f(x) = a_n*x^n + a_n-1*x^n-1 + ... + a_1 * x + a_0"
    print *, "方程式の次数を入力してください"
    read *, xOrder
    allocate (xCoefficient(xOrder))
    xCoefficient(:) = 0.0e0
    print *, "方程式の係数を次数の高い順に入力してください"
    read *, (xCoefficient(i), i = 1, xOrder + 1)

    !  入力結果の表示  !
    print *, "入力された方程式f(x) = 0の関数f(x)は"
    print *, "f(x) = "
    do i = 1, xOrder + 1
        if ( i < xOrder ) then
            if ( xCoefficient(i) >= 0 ) then    !2次以上の項について
                print ('(8x, "+", 1x, f5.1, 1x, "* x **", i2)'), xCoefficient(i), xOrder + 1 - i
            else
                print ('(8x, "-", 1x, f5.1, 1x, "* x **", i2)'), abs(xCoefficient(i)), xOrder + 1 - i
            end if
        else if ( i == xOrder ) then            !1次の項について、次数を表記しない (ex. x^1 → x)
            if ( xCoefficient(i) >= 0 ) then
                print ('(8x, "+", 1x, f5.1, 1x, "* x")'), xCoefficient(i)
            else
                print ('(8x, "-", 1x, f5.1, 1x, "* x")'), abs(xCoefficient(i))
            end if
        else                                    !定数項について、x以降を記述しない
            if ( xCoefficient(i) >= 0 ) then
                print ('(8x, "+", 1x, f5.1)'), xCoefficient(i)
            else
                print ('(8x, "-", 1x, f5.1)'), abs(xCoefficient(i))
            end if
        end if
    end do
    print *, "です"
    print *, "この関数が重解を持つ場合等には正しく解が求まらないことがあります"

    print *, "必要精度：1.0*10^-n -> nを入力してください（正の数）"    !必要精度 = 最小区間幅
    read *, n
    eps = 1.0e-1 ** n

    !   自動解探索  !
    !区間は[-99.7, -98.7] -> [-98.7, -97.7] -> ... -> [99.3, 100.3]と自動設定され区間内の解を探索する
    !（反復回数が1,2回で終わるのを防ぐために敢えて不自然な区間にしている）
    !1区間内に解が2個以上存在する場合や、重解を持つ場合には正しく求まらない
    do num = 1, 200
        est(num) = -99.7e0 + real(num) !区間リストの設定
    end do
    print *, "計算過程は以下の通りです。"
    allocate(solution(xOrder)) !解、更新回数、区間幅のリストは解の個数に設定
    allocate(counter(xOrder))
    allocate(width(xOrder))
    do num = 1, 199      !解の探索
        x1 = est(num)
        x2 = est(num + 1)   !区間リストの隣合う要素から参照して区間を自動設定
        if ( f(x1) * f(x2) <0 ) then
            solution_num = solution_num + 1
            cnt = 0
            print '(i1, "個目の解")', solution_num
            print '("反復回数", 7x, "推定解", 12x, "関数値")'
            do
                cnt = cnt + 1
                xc = (x1 + x2) / 2
                if ( f(x1) * f(xc) < 0 ) then
                    x2 = xc
                else if ( f(xc) == 0 ) then
                    print '(3x, i2, 3x, 2x, f13.6, 2x, f18.6)', cnt, xc, f(xc) !計算過程の出力
                    exit
                else
                    x1 = xc
                end if
                print '(3x, i2, 3x, 2x, f13.6, 2x, f18.6)', cnt, xc, f(xc) !計算過程の出力
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
      
    !    最終的な計算結果の出力    !
    do solution_num = 1, xOrder
        print '(i1, "個目の解：")', solution_num
        print '(f10.6, "が解として得られました")', solution(solution_num)
        print '("その時の関数値は", f10.6, "です")', f(solution(solution_num))
        print '("区間分割回数は", i3, "回です")', counter(solution_num)
        print '("最終分割後の区間長は",f10.6, "です")', width(solution_num)
    end do

    !  内部手続き  !
    contains
    !  関数の入力  !
    function f(x) result(fvalue)
        implicit none
        integer :: i
        real, intent(in) :: x
        real :: ftmp, fvalue
        
        ftmp = 0
        
        do i = 1, xOrder + 1
            ftmp = ftmp + xCoefficient(i) * x ** (xOrder + 1 - i)
        end do
        fvalue = ftmp
    end function f

end program bisection_method