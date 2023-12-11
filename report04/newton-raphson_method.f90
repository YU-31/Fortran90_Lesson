program newtonRaphson_method
    implicit none
    
    real :: xmin, xmax, xc, eps
    integer :: cnt = 0

    real, dimension(100) :: xCoefficient1 = 0, xCoefficient2_a, xCoefficient2_b = 0    !係数を格納
    integer :: select, xOrder1, xOrder2, i

    !                               方程式の構成                             !
    print *, "入力する方程式の形式を選んでください"
    print *, "1. f(x) = a_n*x^n + a_n-1*x^n-1 + ... + a_1 * x + a_0"
    print *, "2. f(x) = (a_1*x + b_1)...(a_n*x + b_n)"
    read *, select
    do
        if ( select == 1 ) then
            print *, "方程式の次数を入力してください"
            read *, xOrder1
            print *, "方程式の係数を次数の高い順に入力してください"
            read *, (xCoefficient1(i), i = 1, xOrder1 + 1)
            exit
        else if ( select == 2 ) then
            print *, "方程式の次数を入力してください"
            read *, xOrder2
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

!
!
!       演算
!
!     
  
    !    最終的な計算結果の出力    !
    print '(f10.6, "が解として得られました")', xc
    if (select == 1) then
        print '("その時の関数値は", f10.6, "です")', f1(xc)
    else
        print '("その時の関数値は", f10.6, "です")', f2(xc)
    end if        
    print '("区間分割回数は", i2, "回です")', cnt
    print '("最終分割後の区間長は",f10.6, "です")', xmax - xmin

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
end program newtonRaphson_method