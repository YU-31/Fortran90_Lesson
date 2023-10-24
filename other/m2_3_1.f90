! How many underages ex2_3_1
PROGRAM m2_3_1
        IMPLICIT NONE
        INTEGER :: n, s, k

        s = 0
        k = 0

10      PRINT *, 'Input age of each person (Quit, if negative value is given) :'
        
        READ *, n
        IF (n < 0) GO TO 999
        s = s + 1
        IF (n <20) k = k + 1
        GO TO 10

999     PRINT *, 'persons are underage. Total No. of person =', s

END PROGRAM m2_3_1
