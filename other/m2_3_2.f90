PROGRAM m2_3_2

        IMPLICIT NONE
        REAL :: x

        PRINT *, 'Input real number x:'
        READ *, x

        IF (x > 0) THEN
                PRINT *, 'Log(x)=', LOG(x)
        ELSE
                PRINT *, 'Calculation impossible for x <= 0'
        END IF

END PROGRAM m2_3_2
