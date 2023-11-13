!practice of multiplicaiton
program m2_5_4

    implicit none
    integer :: i, j, k

    var1 : do j = 1, 9
    var2 : do i = 1, 9

        print '(i1, 1x, a6, 1x, i1, 1x, a3)', i, 'kakeru', j, 'ha?'
        read *, k
        if ( k == i * j ) then
            print *, '!SEIKAI!'
        else
            print *, '!fuseikai...!'
        end if
        
    end do var2
    end do var1

    print *, 'matane..'

end program m2_5_4