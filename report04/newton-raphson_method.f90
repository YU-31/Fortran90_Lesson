program newtonRaphson_method
    implicit none

    real :: x1, x2, xnew, xinit, xtmp, eps
    integer :: cnt = 0, solution_num = 0
    integer :: xOrder !次数
    integer :: i, num   !カウンタ
    integer :: n

    real, dimension(:), allocatable :: xCoefficient   !係数を格納
    real, dimension(:), allocatable :: solution !解を格納
    integer, dimension(:), allocatable :: counter !反復回数の記録
    real, dimension(:), allocatable :: xinit_list !初期値の記録
    real, dimension(20) :: est

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

    !   演算    !
    !同じ解が求まったときにそれを採用しないようにする必要性
    allocate(solution(xOrder))
    allocate(counter(xOrder))
    allocate(xinit_list(xOrder))
    solution_num = 1
    do num = 1, 20
        est(num) = -99.7e0 + 10 * real(num)
    end do
    do num = 1, 19
        xinit = est(num)
        xtmp = est(num)
        do
            cnt = cnt + 1
            xnew = xtmp - f(xtmp) / df(xtmp)
            if ( abs( xnew - xtmp ) <= eps .or. cnt == 100 ) then   !1個目に得られた解は必ず格納
                if ( solution_num == 1 ) then
                    solution(solution_num) = xnew
                    counter(solution_num) = cnt
                    xinit_list(solution_num) = xinit
                    solution_num = solution_num + 1
                    cnt = 0
                    exit
                else if ( abs(xnew - solution(solution_num - 1)) <= 1.0e-4 .and.&
                    & cnt <= counter(solution_num - 1) ) then   !前に得られた解と同じで、更新回数が少ない時はそれを採用
                    solution(solution_num - 1) = xnew
                    counter(solution_num - 1) = cnt
                    xinit_list(solution_num - 1) = xinit
                    cnt = 0
                    exit
                else    !新規の解は新たに格納
                    solution(solution_num) = xnew
                    counter(solution_num) = cnt
                    xinit_list(solution_num) = xinit
                    solution_num = solution_num + 1
                    cnt = 0
                    exit
                end if
            end if
            xtmp = xnew
        end do
    end do

    !    最終的な計算結果の出力    !
    do solution_num = 1, xOrder
        print '(i1, "個目の解：")', solution_num
        print '(f10.6, "が解として得られました")', solution(solution_num)
        print '("その時の関数値は", f10.6, "です")', f(solution(solution_num))
        print '("初期値は", f4.1, "です")', xinit_list(solution_num)
        print '("更新回数は", i3, "回です")', counter(solution_num)
    end do

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

   function df(x) result(dfvalue)
       implicit none
       integer :: i
       real, intent(in) :: x
       real :: dfvalue, dfvaluetmp
       
       dfvaluetmp = 0

       do i = 1, xOrder + 1
           dfvaluetmp = dfvaluetmp + (xOrder + 1 - i) * xCoefficient(i) * x ** (xOrder - i)
       end do
       dfvalue = dfvaluetmp
   end function df

end program newtonRaphson_method