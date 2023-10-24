PROGRAM problem02

        IMPLICIT NONE
        INTEGER :: n, m, s, k

        s = 0
        k = 0

10      PRINT *, 'Input age of each person (Quit, if negative value is given):'
        READ *, n

        IF (n < 0) THEN
                PRINT *, 'Are you sure to quit? If you want to quit, enter "1". If not, enter other integer.'
                READ *, m
                IF (m == 1) THEN
                        GO TO 999
                ELSE
                        GO TO 10
                END IF
        END IF

        s = s + 1

        IF (n >= 20 .AND. n < 30) k = k + 1
        
        GO TO 10

999     PRINT *, k, 'persons are meet the condition. Total numbers of persons =', s 

END PROGRAM problem02
